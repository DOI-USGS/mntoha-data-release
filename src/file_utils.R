
split_pb_filenames <- function(files_df){
  extract(files_df, file, c('prefix','site_id','suffix'), "(pb0|pball)_(.*)_(temperatures_irradiance.feather)", remove = FALSE)
}

extract_pb0_ids <- function(model_out_ind){
  tibble(file = names(yaml::yaml.load_file(model_out_ind))) %>%
    split_pb_filenames() %>%
    pull(site_id)
}

update_hash_path <- function(yaml_out, yaml_in, add_path){

  hash_yaml <- yaml::yaml.load_file(yaml_in)
  old_paths <- names(hash_yaml)
  names(hash_yaml) <- file.path(add_path, old_paths)
  yaml::write_yaml(hash_yaml, file = yaml_out)
}

extract_pgdl_ids <- function(results_dir, pattern, dummy) {
  dir(results_dir, pattern)
}

bundle_pgdl_configs <- function(out_file, runs_dir, col_types='cccicccdiddddiiiiilcccccci') {
  # get a list of runs assuming that all runs are nested just within folders
  # named by site_id, all within a single runs_dir
  site_ids <- dir(runs_dir, pattern='nhdhr*')
  run_paths <- dir(file.path(runs_dir, site_ids), full.names=TRUE)

  configs_all <- lapply(run_paths, function(run_path) {
    run_complete <- all(file.exists(file.path(run_path, c('model_config.tsv'))))
    if(!run_complete) return(NULL)
    readr::read_tsv(file.path(run_path, 'model_config.tsv'), col_types=col_types)
  }) %>% bind_rows() %>%
    arrange(row) %>%
    select(
      row, site_id, phase, goal, fold, learning_rate, n_epochs,
      state_size, ec_threshold, dd_lambda, ec_lambda, l1_lambda,
      sequence_length, sequence_offset, max_batch_obs,
      inputs_fixed_file, inputs_prep_file, inputs_varied_file,
      model_restore_path, model_save_path)

  write_csv(configs_all, out_file)
}

create_release_fl <- function(geometry){
  release_fl <- rep(NA_character_, length(geometry))
  for (i in 1:length(geometry)){
    # format of meteo_nldas_N46.125-46.25_W86.25-86.375.csv
    this_box <- st_bbox(geometry[i])
    release_fl[i] <- sprintf("nldas_meteo_N%1.4f-%1.4f_W%1.4f-%1.4f.csv", this_box$ymin, this_box$ymax, -this_box$xmax, -this_box$xmin)
  }
  return(release_fl)
}
xwalk_meteo_lat_lon <- function(meteo_fl, meteo_dir, ldas_grid){


  all_meteo_fls <- data.frame(pipeline_fl = dir(meteo_dir), stringsAsFactors = FALSE) %>%
    filter(stringr::str_detect(pipeline_fl, "[0-9n]\\].csv")) %>%
    mutate(x = stringr::str_extract(pipeline_fl, 'x\\[[0-9]+\\]') %>% str_remove('x\\[') %>% str_remove('\\]') %>% as.numeric(),
           y = stringr::str_extract(pipeline_fl, 'y\\[[0-9]+\\]') %>% str_remove('y\\[') %>% str_remove('\\]') %>% as.numeric()) %>%
    left_join(suppressWarnings(st_centroid(ldas_grid))) %>% rename(geometry = ldas_grid_sfc) %>% select(-x, -y) %>%
    st_sf() %>% mutate(release_fl = create_release_fl(geometry))

  meteo_data <- readRDS(meteo_fl) %>% rename(pipeline_fl = meteo_fl)
  left_join(all_meteo_fls, meteo_data, by = 'pipeline_fl') %>% select(site_id, everything()) %>% st_drop_geometry()

}

create_metadata_file <- function(fileout, sites, table, lakes_sf, lat_lon_fl, nml_json_fl, benthic_area_fl, meteo_fl_info, gnis_names_fl){
  sdf <- sf::st_transform(lakes_sf, 2811) %>%
    mutate(perim = lwgeom::st_perimeter_2d(Shape), area = sf::st_area(Shape), circle_perim = 2*pi*sqrt(area/pi), SDF = perim/circle_perim) %>%
    sf::st_drop_geometry() %>% select(site_id, SDF)

  benthic_area <- read_csv(benthic_area_fl)
  nml_list <- RJSONIO::fromJSON(nml_json_fl)

  basic_info <- purrr::map(names(nml_list), function(x){
    this_nml <- nml_list[[x]]
    class(this_nml) <- "nml"
    tibble(site_id = x, depth = glmtools::get_nml_value(this_nml, 'lake_depth'),
           area = tail(glmtools::get_nml_value(this_nml, "A"), 1))
  }) %>% purrr::reduce(bind_rows)

  sites %>% inner_join((readRDS(lat_lon_fl)), by = 'site_id') %>%
    inner_join(sdf, by = 'site_id') %>%
    inner_join(basic_info, by = 'site_id') %>%
    inner_join(benthic_area, by = 'site_id') %>%
    rename(centroid_lon = longitude, centroid_lat = latitude) %>%
    inner_join(table, by = 'site_id') %>%
    inner_join(meteo_fl_info, by = 'site_id') %>% select(-pipeline_fl) %>%
    inner_join((readRDS(gnis_names_fl)), by = 'site_id') %>%
    select(site_id, lake_name = GNIS_Name, group_id, meteo_filename = release_fl, depth, area, everything()) %>%
    write_csv(fileout)

}
bundle_nml_files <- function(json_filename, xwalk_meteo_fl_names, lake_ids, nml_ind, gnis_names_fl){

  gnis_names <- readRDS(gnis_names_fl)

  prep_proj_dir <- paste(str_split(nml_ind, '/')[[1]][1:2], collapse = '/')
  nml_files <- file.path(prep_proj_dir, names(yaml.load_file(nml_ind)))
  file_bases <- tibble(file = basename(nml_files)) %>%
    mutate(filebase = str_remove(file, 'pball_|transfer_')) %>% pull(filebase)
  out_list <- vector("list", length = length(lake_ids)) %>% setNames(lake_ids)

  for (id in names(out_list)){
    this_nml_file <- nml_files[file_bases == paste0(id, '_glm3.nml')]
    if (!file.exists(this_nml_file)){

      stop(this_nml_file, " doesn't exist")
    }
    release_meteo_fl <- filter(xwalk_meteo_fl_names, site_id == id) %>% pull(release_fl)

    lake_name <- filter(gnis_names, site_id == id) %>% pull(GNIS_Name)
    lake_name <- ifelse(is.na(lake_name), 'undefined', lake_name)
    nml <- read_nml(nml_file = this_nml_file) %>%
      set_nml(arg_list = list('meteo_fl' = release_meteo_fl,
                              'lake_name' = lake_name,
                              'Kw_file' = sprintf('gam_%s_clarity.csv', id))) %>% unclass()
    out_list[[id]] <- nml
  }

  RJSONIO::toJSON(out_list, pretty = TRUE) %>% write(json_filename)
}

zip_nml_files <- function(zipfile, nml_json){

  cd <- getwd()
  on.exit(setwd(cd))
  zippath <- file.path(getwd(), zipfile)

  nml_list <- RJSONIO::fromJSON(nml_json)
  nml_dir <- file.path(tempdir(), 'nml')
  dir.create(nml_dir)
  site_ids <- names(nml_list)

  nml_files <- sapply(site_ids, function(x){
    class(nml_list[[x]]) <- 'nml'
    nml_file <- paste0(x, "_glm3.nml")
    glmtools::write_nml(nml_list[[x]], file = file.path(nml_dir, nml_file))
    return(nml_file)
  })

  setwd(nml_dir)

  if (file.exists(zippath)){
    unlink(zippath)
  }

  zip(zippath, files = nml_files)

  unlink(nml_dir, recursive = TRUE)
  setwd(cd)
}

group_meteo_fls <- function(meteo_dir, groups, counties_sf, use_states){

  # turn files into point locations
  # check group match with assign_group_id(points, polygons)
  # return data.frame with id and filename

  meteo_fls <- data.frame(files = dir(meteo_dir), stringsAsFactors = FALSE) %>%
    filter(stringr::str_detect(files, "[0-9n]\\].csv")) %>%
    mutate(x = stringr::str_extract(files, 'x\\[[0-9]+\\]') %>% str_remove('x\\[') %>% str_remove('\\]') %>% as.numeric(),
           y = stringr::str_extract(files, 'y\\[[0-9]+\\]') %>% str_remove('y\\[') %>% str_remove('\\]') %>% as.numeric()) %>%
    left_join(suppressWarnings(st_centroid(create_ldas_grid()))) %>% rename(geometry = ldas_grid_sfc) %>% select(-x, -y) %>%
    st_sf()

  state_meteo_rows <- counties_sf %>% group_by(state) %>% summarise() %>% filter(state %in% use_states) %>%
    st_buffer(0.07) %>% # degree buffer to extend the state to include those meteo cells too
    suppressWarnings() %>% st_covers(y = meteo_fls) %>% suppressWarnings() %>% as.data.frame() %>% pull(col.id)

  grouped_df <- st_intersects(x = meteo_fls, y = groups) %>% as.data.frame() %>% rename(group_idx = col.id) %>% suppressWarnings()

  meteo_fls %>% mutate(row.id = row_number()) %>%
    filter(row.id %in% state_meteo_rows) %>%
    inner_join(grouped_df) %>% mutate(group_id = groups$group_id[group_idx], meteo_filepath = file.path(meteo_dir, files)) %>%
    select(meteo_filepath, group_id) %>% st_drop_geometry() %>% suppressWarnings()

}

zip_meteo_groups <- function(outfile, xwalk_meteo_fl_names, grouped_meteo_fls){

  cd <- getwd()
  on.exit(setwd(cd))

  groups <- unique(grouped_meteo_fls$group_id)

  data_files <- c()
  for (group in groups){
    meteo_dir <- file.path(tempdir(), group)
    dir.create(meteo_dir)

    zipfile <- paste0('tmp/inputs_', group, '.zip')
    these_files <- grouped_meteo_fls %>% filter(group_id == !!group) %>%
      mutate(pipeline_fl = basename(meteo_filepath)) %>%
      inner_join(xwalk_meteo_fl_names, by = 'pipeline_fl')
    # write these files under the release name in a tempdir:
    for (i in 1:length(these_files$meteo_filepath)){
      data.table::fread(these_files$meteo_filepath[i], nrows = 14976) %>%
        data.table::fwrite(file.path(meteo_dir, these_files$release_fl[i]))
    }

    zippath <- file.path(getwd(), zipfile)

    if (file.exists(zippath)){
      unlink(zippath) #seems it was adding to the zip as opposed to wiping and starting fresh...
    }

    setwd(meteo_dir)
    zip(zippath, files = these_files$release_fl)
    setwd(cd)
    data_files <- c(data_files, zipfile)
    unlink(meteo_dir, recursive = TRUE)
  }
  scipiper::sc_indicate(outfile, data_file = data_files)
}


#' builds the data.frame that is used to define how model results are exported
#' @param site_ids which model ids to use in the export
#' @param model_out_ind the indicator file which defines the complete model run files
#' @param exp_prefix prefix to the exported files (e.g., 'pb0')
#' @param exp_suffix suffix to the exported files (e.g., 'irradiance')
export_pb_df <- function(site_ids, model_out_ind, exp_prefix, exp_suffix){

  model_proj_dir <- paste(str_split(model_out_ind, '/')[[1]][1:2], collapse = '/')
  tibble(file = names(yaml.load_file(model_out_ind))) %>%
    split_pb_filenames() %>% filter(site_id %in% site_ids) %>%
    mutate(out_file = sprintf('%s_%s_%s.csv', exp_prefix, site_id, exp_suffix),
           source_filepath = file.path(model_proj_dir, file)) %>%
    select(site_id, source_filepath, out_file)
}


build_pgdl_predict_df <- function(
  pgdl_config_file = 'out_data/pgdl_config.csv',
  model_dir='../lake-temperature-neural-networks/2_model/out/200316_runs',
  prefix='pgdl', suffix='temperatures', dummy){

  readr::read_csv(pgdl_config_file) %>%
    filter(phase=='finetune', goal=='predict') %>%
    mutate(
      source_filepath = file.path(gsub('2_model/out', model_dir, model_save_path), 'preds.npz'),
      out_file = paste0(prefix, '_', site_id, '_', suffix, '.csv')) %>%
    select(site_id, source_filepath, out_file)
}


zip_pb_export_groups <- function(outfile, file_info_df, site_groups,
                                 export = c('ice_flags','pb0_predictions','clarity','irradiance'),
                                 export_start, export_stop){

  export <- match.arg(export)

  model_feathers <- inner_join(file_info_df, site_groups, by = 'site_id') %>%
    select(-site_id)

  zip_pattern <- paste0('tmp/', export, '_%s.zip')

  cd <- getwd()
  on.exit(setwd(cd))

  groups <- rev(sort(unique(model_feathers$group_id)))
  data_files <- c()

  for (group in groups){
    zipfile <- sprintf(zip_pattern, group)

    these_files <- model_feathers %>% filter(group_id == !!group)

    zippath <- file.path(getwd(), zipfile)

    if (file.exists(zippath)){
      unlink(zippath) #seems it was adding to the zip as opposed to wiping and starting fresh...
    }

    for (i in 1:nrow(these_files)){
      fileout <- file.path(tempdir(), these_files$out_file[i])

      model_data <- feather::read_feather(these_files$source_filepath[i]) %>%
        rename(kd = extc_coef_0) %>%
        mutate(date = as.Date(lubridate::ceiling_date(time, 'days'))) %>%
        filter(date >= export_start & date <= export_stop)

      switch(export,
             ice_flags = select(model_data, date, ice),
             pb0_predictions = select(model_data, date, contains('temp_')),
             clarity = select(model_data, date, kd),
             irradiance = select(model_data, date, rad_0)) %>%
        write_csv(path = fileout)
    }

    setwd(tempdir())

    zip(zippath, files = these_files$out_file)
    unlink(these_files$out_file)
    setwd(cd)
    data_files <- c(data_files, zipfile)
  }
  scipiper::sc_indicate(outfile, data_file = data_files)

}


zip_pgdl_prediction_groups <- function(outfile, predictions_df, site_groups){

  model_npzs <- inner_join(predictions_df, site_groups, by = 'site_id') %>%
    select(-site_id)

  cd <- getwd()
  on.exit(setwd(cd))

  np <- reticulate::import('numpy')
  groups <- rev(sort(unique(model_npzs$group_id)))
  data_files <- c()
  for (group in groups){
    zipfile <- paste0('tmp/pgdl_predictions_', group, '.zip')
    these_files <- model_npzs %>% filter(group_id == !!group)

    zippath <- file.path(getwd(), zipfile)
    if (file.exists(zippath)){
      unlink(zippath) #seems it was adding to the zip as opposed to wiping and starting fresh...
    }
    for (i in 1:nrow(these_files)){
      filein <- these_files$source_filepath[i]
      fileout <- file.path(tempdir(), these_files$out_file[i])

      preds_list <- np$load(filein)
      preds_list$f$preds_best %>%
        as_tibble(.name_repair='minimal') %>%
        setNames(preds_list$f$pred_dates) %>%
        mutate(depth = sprintf('temp_%g', preds_list$f$depths) %>% ordered(., levels=.)) %>%
        tidyr::gather(date, temp_C, -depth) %>%
        tidyr::spread(depth, temp_C) %>%
        write_csv(path = fileout)
    }

    setwd(tempdir())

    zip(zippath, files = these_files$out_file)
    unlink(these_files$out_file)
    setwd(cd)
    data_files <- c(data_files, zipfile)
  }
  scipiper::sc_indicate(outfile, data_file = data_files)
}

filter_feather_obs <- function(outfile, obs_feather, site_ids, obs_start, obs_stop){
  feather::read_feather(obs_feather) %>%
    filter(site_id %in% site_ids) %>%
    filter(date >= obs_start & date <= obs_stop) %>%
    saveRDS(file = outfile)
}

zip_this <- function(outfile, .object){

  if ('data.frame' %in% class(.object)){
    filepath <- basename(outfile) %>% tools::file_path_sans_ext() %>% paste0('.csv') %>% file.path(tempdir(), .)
    write_csv(.object, path = filepath)
    zip_this(outfile = outfile, .object = filepath)
  } else if (class(.object) == 'character' & file.exists(.object)){
    # for multiple files?
    curdir <- getwd()
    on.exit(setwd(curdir))
    setwd(dirname(.object))
    zip(file.path(curdir, outfile), files = basename(.object))
  } else {
    stop("don't know how to zip ", .object)
  }
}

zip_filter_obs <- function(outfile, in_file){

  zip_this(outfile, .object = readRDS(in_file))

}
