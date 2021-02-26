do_sorting_tasks <- function(final_target, site_id_list, prediction_files_df, prediction_type, ...) {

    ##### DEFINE TASK TABLE ROWS #####
    # set task names
    task_names <- site_id_list
    
    
    ##### DEFINE TASK TABLE COLUMNS #####
    ### STEP 1 ###
    ## subset predictions_files dataframe using site id
    # copy prediction file to tmp/ folder
    copy_step <- create_task_step(
        step_name = 'copy',
        target_name = function(task_name, step_name, ...) {
            sprintf('tmp/%s_%s_UNSORTED.csv', task_name, prediction_type)
        },
        command = function(task_name, step_name, ...) {
            # filter prediction files by site_id
            site_files <- filter(prediction_files_df, site_id == task_name)
            # build command to copy files
            psprintf("copy_prediction_file(",
                     "outfile = target_name,",
                     "site_id = I('%s')," = task_name,
                     "infile = '%s')" = site_files$source_filepath
                    )
        }
    )
    
    ###STEP 2 ###
    ## pass source filepath, outfile path to sorting function
    # pass unsorted file instead of filepaths
    sorting_step <- create_task_step(
        step_name = "sort",
        target_name = function(task_name, step_name, ...) {
            sprintf("%s_%s_%s", task_name, prediction_type, step_name)
        },
        command = function(task_name, target_name, steps, ...) {
            # filter prediction files by site_id
            site_files <- filter(prediction_files_df, site_id == task_name)
            # build command to sort profiles
            psprintf("sort_profiles(",
                     "unsorted_predictions_file = '%s'," = steps[['copy']]$target_name,
                     "outfile = I('%s'))" = file.path('tmp', site_files$out_file)
                    )
        } 
    )
    
    ##### STEP 3 #####
    ## do comparison
    comparison_step <- create_task_step(
        step_name = 'compare',
        target_name = function(task_name, step_name, ...) {
            sprintf('tmp/%s_%s_%s.csv', task_name, prediction_type, step_name)
        },
        command = function(target_name, steps, ...) {
            psprintf("compare_profiles(",
                     "outfile = target_name,",
                     "unsorted_predictions_file = '%s'," = steps[['copy']]$target_name,
                     "pgdl_sorted = `%s`)" = steps[['sort']]$target_name
                    )
        })
    
    ##### CREATE TASK PLAN #####
    task_plan <- create_task_plan(
        task_names = task_names,
        task_steps = list(copy_step, sorting_step, comparison_step),
        final_steps = c('compare'),
        add_complete = FALSE
    )
    
    ##### CREATE TASK REMAKE FILE #####
    task_makefile <- sprintf('%s_sorting_tasks.yml', prediction_type)
    create_task_makefile(
        task_plan = task_plan,
        makefile = task_makefile,
        sources = c(...),
        include =c('5_predictions.yml'),
        packages = c('tidyverse', 'purrr', 'readr', 'scipiper'),
        final_targets = final_target,
        finalize_funs = 'combine_all_comparisons',
        as_promises = TRUE,
        tickquote_combinee_objects = TRUE
    )
    
    ##### BUILD THE TASKS #####
    loop_tasks(task_plan = task_plan, task_makefile = task_makefile, num_tries = 100)
    
    ##### CLEAN UP FILES #####
    # remove the temporary target from remake's DB -- we don't need it to persist
    scdel(sprintf("%s_promise", basename(final_target)), remake_file=task_makefile)
    # Delete the task makefile since it is only needed internally for this function
    #  and not needed at all once loop_tasks is complete.
    file.remove(task_makefile)
}

copy_prediction_file <- function(outfile, site_id, infile) {  

    cd <- getwd()
    
    # copy the raw (unsorted) prediction csv files from the lake temperature neural network repo
    file.copy(infile, file.path(cd, outfile), overwrite=TRUE)

}

sort_pgdl <- function(matrix_row) {
    # if all temperatures below 4 deg C...
    if (length(matrix_row[which(matrix_row <= 4)]) == length(matrix_row)) {
        # sort profile to be monontonic by temperature and density
        matrix_row_sorted <- sort(matrix_row, decreasing=FALSE)
    # otherwise...
    } else {
        # sort profile to be monotonic by temperature
        matrix_row_sorted <- sort(matrix_row, decreasing=TRUE)
    }
    return(matrix_row_sorted)

}

sort_profiles <- function(unsorted_predictions_file, outfile) { 
    cd <- getwd()
    
    # load unsorted predictions
    pgdl_unsorted <- readr::read_csv(unsorted_predictions_file)
    # make copy of unsorted dataframe
    pgdl_sorted <- data.frame(pgdl_unsorted)
    
    # sort predictions using conditions defined in sort_pgdl function
    # pass data to sort function as matrix (without date column)
    pgdl_sorted_matrix <- t(apply(pgdl_sorted[-1], 1, sort_pgdl))
    # replace all but date column of pgdl_sorted dataframe with sorted data
    pgdl_sorted[-1] <- pgdl_sorted_matrix
    
    # save sorted predictions
    write_csv(pgdl_sorted, file = file.path(cd, outfile))
       
    # return sorted dataframe
    return(pgdl_sorted)
}

compare_profiles <- function(outfile, unsorted_predictions_file, pgdl_sorted) {

    # load unsorted predictions
    pgdl_unsorted <- readr::read_csv(unsorted_predictions_file)
    
    # compare sorted and unsorted dataframes using rowSums and == comparison
    # When applying the == test, every cell in a given row is compared between
    # the sorted and unsorted dataframes. If the value is identical, the cell value
    # in the dataframe generated by the test is 1 (TRUE, the values do match). If the 
    # values differ, the cell value will be 0 (FALSE, the values do not match). Since 
    # each row represents a single day and each column the water temperature at a 
    # particular depth, we can determine if the sorted profile differs from the 
    # unsorted profile by using rowSums to compute the sum of all cells in each row in 
    # the output dataframe generated by the == test. For each row (each day) this 
    # sum represents the number of depths at which the sorted temperature is the same 
    # as the unsorted temperature
    row_comparison <- rowSums(pgdl_sorted == pgdl_unsorted)
    
    # compare profile for each day to test if has been sorted
    pgdl_compared <- data.frame(date = pgdl_sorted$date, 
                                # To determine if the profile is sorted, test whether the comparison sum for each row
                                # is equal to (not sorted) or less than (sorted) the number of columns in the unsorted dataframe
                                profile_sorted = (ncol(pgdl_unsorted) != row_comparison),
                                # The number of depths at that profile is equivalent to the number of columns in the unsorted dataframe
                                num_depths = ncol(pgdl_unsorted),
                                # The number of depths for which the temperature has changed (due to sorting) can be computed by
                                # subtracting the row_comparison value (number of depths at which temp is unchanged) from
                                # the total number of depth (# of column in unsorted dataframe)
                                num_depths_w_changed_temp = (ncol(pgdl_unsorted) - row_comparison))   
    
    # save comparison results
    write_csv(pgdl_compared, file = outfile)
}

get_site_ids <- function(predictions_df) {
    predictions_df %>%
        pull(site_id)
}

combine_all_comparisons <- function(outfile, ...) {  
    print('final combine step')
    
    # make vector of results from final steps (compare steps)
    all_comparison_csvs <- c(...)

    # make indicator file
    scipiper::sc_indicate(outfile, data_file = all_comparison_csvs)
}
