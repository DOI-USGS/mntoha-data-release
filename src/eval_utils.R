filter_min_dates <- function(obs_rds_file, min_dates){

  obs <- readRDS(obs_rds_file)
   eval_sites <- group_by(obs, site_id) %>% summarize(n_dates = length(unique(date))) %>% filter(n_dates >= min_dates) %>%
     pull(site_id)

   filter(obs, site_id %in% eval_sites)
}

match_glm_obs <- function(target_name, eval_data, model_out_ind){

  model_proj_dir <- paste(str_split(model_out_ind, '/')[[1]][1:2], collapse = '/')
  eval_site_ids <- unique(eval_data$site_id)

  file_info <- tibble(file = names(yaml.load_file(model_out_ind))) %>% split_pb_filenames() %>%
    mutate(source_filepath = file.path(model_proj_dir, file)) %>%
    filter(site_id %in% eval_site_ids) %>% select(site_id, source_filepath)

  eval_data <- purrr::map(1:nrow(file_info), function(x){
    this_file <- file_info$source_filepath[x]
    this_id <- file_info$site_id[x]
    these_obs <- eval_data %>% filter(site_id %in% this_id)
    model_preds <- feather::read_feather(this_file) %>% select(time, contains('temp_')) %>%
      pivot_longer(-time, names_to = 'depth', values_to = 'temp', names_prefix = 'temp_') %>%
      mutate(depth = as.numeric(depth)) %>% filter(time %in% these_obs$date) %>%
      rename(date = time, pred = temp)

    prep_pred_obs(test_obs = these_obs, model_preds = model_preds) %>%
      select(site_id, date, depth, obs, pred, source_id)
  }) %>% purrr::reduce(bind_rows)

  write_csv(eval_data, path = target_name)

}

compare_as_rmse <- function(target_name, matched_preds){

  mutate(matched_preds, pred_diff = pred-obs) %>%
    group_by(site_id) %>% summarize(rmse = sqrt(mean((pred_diff)^2, na.rm=TRUE))) %>%
    write_csv(path = target_name)

}



# function to read a test file and identify the corresponding predictions
prep_pred_obs <- function(test_obs, model_preds) {

  # match up preds to test_obs, interpolating GLM predictions to match the observation depths
  pred_obs <- bind_rows(lapply(unique(test_obs$date), function(dt) {
    pred_1d <- filter(model_preds, date == dt, !is.na(depth))

    obs_1d <- filter(test_obs, date == dt) %>%
      rename(obs = temp)
    tryCatch({
      if(nrow(pred_1d) == 0) stop(sprintf('no predictions on %s', dt))
      if(min(pred_1d$depth) != 0) warning(sprintf('no GLM prediction at 0m on %s', dt))
      mutate(obs_1d, pred = approx(x=pred_1d$depth, y=pred_1d$pred, xout=obs_1d$depth, rule=1)$y)
    }, error=function(e) {
      message(sprintf('approx failed on %s: %s', dt, e$message))
      mutate(obs_1d, pred = NA)
    })
  }))


  return(pred_obs)
}

