do_sorting_tasks <- function(final_target, site_id_list, prediction_files, prediction_type, ...) {

    ##### DEFINE TASK TABLE ROWS #####
    # set task names
    task_names <- site_id_list
    
    
    ##### DEFINE TASK TABLE COLUMNS #####
    ### STEP 1 ###
    ## subset predictions_files dataframe using site id
    filepaths_step <- create_task_step(
        step_name = 'filepaths',
        target_name = function(task_name, step_name, ...) {
            sprintf('tmp/%s_%s_%s.csv', task_name, step_name, prediction_type)
        },
        command = function(task_name, target_name, step_name, ...) {
            psprintf("subset_prediction_files(",
                     "outfile = target_name,",
                     "site_id = I('%s')," = task_name,
                     "prediction_files = %s)" = prediction_files
                    )
        }
    )
    
    ###STEP 2 ###
    ## pass source filepath, outfile path to sorting function
    sorting_step <- create_task_step(
        step_name = "sort",
        target_name = function(task_name, step_name, ...) {
            sprintf("%s_%s_%s", task_name, step_name, prediction_type)
        },
        command = function(target_name, steps, ...) {
            sprintf("sort_profiles(site_filepaths_file = '%s')", steps[['filepaths']]$target_name) # if target in previous step = object, site_filepaths = `%s`
        }
    )
    
    ##### STEP 3 #####
    ## do comparison
    comparison_step <- create_task_step(
        step_name = 'compare',
        target_name = function(task_name, step_name, ...) {
            sprintf('tmp/%s_%s_%s.csv', task_name, step_name, prediction_type)
        },
        command = function(target_name, steps, ...) {
            psprintf("compare_profiles(",
                     "outfile = target_name,",
                     "site_filepaths_file = '%s'," = steps[['filepaths']]$target_name,
                     "pgdl_sorted = `%s`)" = steps[['sort']]$target_name
                    )
        })
    
    ##### CREATE TASK PLAN #####
    task_plan <- create_task_plan(
        task_names = task_names,
        task_steps = list(filepaths_step, sorting_step, comparison_step),
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
}

subset_prediction_files <- function(outfile, site_id, prediction_files) {  
  
    # subset prediction files dataframe to row for selected site
    # and save the dataframe of site-specific filepaths as a csv
    prediction_files_subset <- prediction_files %>%
        filter(site_id == !!site_id) %>%
        write_csv(file = outfile)
    
    # copy over predictions for that site
    copy_pgdl_predictions(prediction_files_subset)
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

sort_profiles <- function(site_filepaths_file) { 
    
    # load site filepaths file
    site_filepaths <- readr::read_csv(site_filepaths_file, col_types='ccccc')
    # load unsorted predictions
    pgdl_unsorted <- readr::read_csv(file.path(tempdir(), site_filepaths$raw_file))
    pgdl_sorted <- data.frame(pgdl_unsorted)
    
    # sort predictions using conditions defined in sort_pgdl function
    pgdl_sorted_matrix <- t(apply(pgdl_sorted[-1], 1, sort_pgdl))
    pgdl_sorted[-1] <- pgdl_sorted_matrix
    
    # save sorted predictions
    write_csv(pgdl_sorted, file = file.path(tempdir(), site_filepaths$out_file))
       
    # return sorted dataframe
    return(pgdl_sorted)
}

compare_profiles <- function(outfile, site_filepaths_file, pgdl_sorted) {
    # load site filepaths file
    site_filepaths <- readr::read_csv(site_filepaths_file, col_types='ccccc')
    # load unsorted predictions
    pgdl_unsorted <- readr::read_csv(file.path(tempdir(), site_filepaths$raw_file))
    
    # compare profile for each day to test if has been sorted
    pgdl_compared <- data.frame(date = pgdl_sorted$date, 
                                profile_sorted = (ncol(pgdl_unsorted) != rowSums(pgdl_sorted == pgdl_unsorted)),
                                num_depths = ncol(pgdl_unsorted),
                                num_depths_w_changed_temp = (ncol(pgdl_unsorted) - rowSums(pgdl_sorted == pgdl_unsorted)))
    
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
