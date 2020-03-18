
extract_model_ids <- function(results_dir, pattern, dummy){
  warning('this is a temporary function and should be replaced with an inventory of **modeled** lakes **and** w/ hypso')
  
  tibble(file = dir(results_dir)) %>% 
    filter(str_detect(file, pattern)) %>% pull(file) %>% str_remove("_temp_opt.feather")
}

create_metadata_file <- function(fileout, sites, table, lakes_sf, lat_lon_fl, meteo_fl, gnis_names_fl){
  sdf <- sf::st_transform(lakes_sf, 2811) %>% 
    mutate(perim = lwgeom::st_perimeter_2d(Shape), area = sf::st_area(Shape), circle_perim = 2*pi*sqrt(area/pi), SDF = perim/circle_perim) %>% 
    sf::st_drop_geometry() %>% select(site_id, SDF) 

  sites %>% inner_join((readRDS(lat_lon_fl)), by = 'site_id') %>% 
    inner_join(sdf, by = 'site_id') %>% 
    rename(centroid_lon = longitude, centroid_lat = latitude) %>% 
    inner_join(table, by = 'site_id') %>% 
    inner_join(readRDS(meteo_fl), by = 'site_id') %>% 
    inner_join((readRDS(gnis_names_fl)), by = 'site_id') %>% rename(lake_name = GNIS_Name, meteo_filename = meteo_fl) %>% 
    write_csv(fileout)
  
}
bundle_nml_files <- function(json_filename, lake_ids, nml_ind){
  
  
  prep_proj_dir <- paste(str_split(nml_ind, '/')[[1]][1:2], collapse = '/')
  nml_files <- file.path(prep_proj_dir, names(yaml.load_file(nml_ind)))
  file_bases <- tibble(file = basename(nml_files)) %>% 
    mutate(filebase = str_remove(file, 'pball_|transfer_')) %>% pull(filebase)
  out_list <- vector("list", length = length(lake_ids)) %>% setNames(lake_ids)
  
  for (id in names(out_list)){
    this_nml_file <- nml_files[file_bases == paste0(id, '.nml')]
    if (!file.exists(this_nml_file)){
      
      stop(this_nml_file, " doesn't exist")
    }
    nml <- read_nml(nml_file = this_nml_file) %>% unclass()
    out_list[[id]] <- nml
  }
  
  RJSONIO::toJSON(out_list, pretty = TRUE) %>% write(json_filename)
}

zip_nml_files <- function(zipfile, lake_ids, nml_ind){
  
  cd <- getwd()
  on.exit(setwd(cd))
  zippath <- file.path(getwd(), zipfile)
  
  prep_proj_dir <- paste(str_split(nml_ind, '/')[[1]][1:2], collapse = '/')
  
  nml_files <- tibble(file = file.path(prep_proj_dir, names(yaml.load_file(nml_ind)))) %>% 
    filter(basename(file) %in% paste0(lake_ids, '.nml')) %>% pull(file)
  
  setwd(unique(dirname(nml_files))[1])
  if (file.exists(zippath)){
    unlink(zippath)
  }
  zip(zippath, files = basename(nml_files))
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

zip_meteo_groups <- function(outfile, grouped_meteo_fls){

  cd <- getwd()
  on.exit(setwd(cd))
  
  groups <- unique(grouped_meteo_fls$group_id)
  data_files <- c()
  for (group in groups){
    zipfile <- paste0('tmp/inputs_', group, '.zip')
    these_files <- grouped_meteo_fls %>% filter(group_id == !!group) %>% pull(meteo_filepath)
    
    zippath <- file.path(getwd(), zipfile)
    
    meteo_dir <- dirname(these_files) %>% unique()
    
    setwd(meteo_dir)
    zip(zippath, files = basename(these_files))
    setwd(cd)
    data_files <- c(data_files, zipfile)
  }
  scipiper::sc_indicate(outfile, data_file = data_files)
}

build_clarity_df <- function(site_ids = model_export_ids){
  tibble(site_id = site_ids) %>% 
    mutate(out_file = paste0('gam_',site_id, '_clarity.csv')) 
}

build_predict_df <- function(site_ids, model_dir, prefix, suffix, dummy){

  tibble(source_file = dir(model_dir)) %>% 
    filter(str_detect(source_file, '_temp_opt.feather$')) %>% 
    extract(source_file, c('site_id','suffix'), "(.*)_(temp_opt.feather)", remove = FALSE) %>% 
    filter(site_id %in% site_ids) %>% select(-suffix) %>% 
    mutate(out_file = paste0(prefix, '_', site_id, '_', suffix, '.csv'), 
           source_filepath = file.path(model_dir, source_file)) %>% select(-source_file)
    
}

zip_clarity_groups <- function(outfile, clarity_df, site_groups, kw_data_file){
  
  kw_data <- readRDS(kw_data_file)
  
  clarity_feathers <- inner_join(clarity_df, site_groups, by = 'site_id') %>% 
    select(-site_id)
  
  cd <- getwd()
  on.exit(setwd(cd))
  
  groups <- rev(sort(unique(clarity_feathers$group_id)))
  data_files <- c()
  for (group in groups){
    zipfile <- paste0('tmp/clarity_', group, '.zip')
    these_files <- clarity_feathers %>% filter(group_id == !!group)
    
    zippath <- file.path(getwd(), zipfile)
    if (file.exists(zippath)){
      unlink(zippath) #seems it was adding to the zip as opposed to wiping and starting fresh...
    }
    for (i in 1:nrow(these_files)){
      fileout <- file.path(tempdir(), these_files$out_file[i])
      this_id <- basename(fileout) %>% str_remove("_clarity.csv") %>% str_remove('gam_')
      kw_data %>% filter(site_id == !!this_id) %>% 
        select(date = time, Kd) %>%
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
  
zip_prediction_groups <- function(outfile, predictions_df, site_groups){
  
  model_feathers <- inner_join(predictions_df, site_groups, by = 'site_id') %>% 
    select(-site_id)
  
  cd <- getwd()
  on.exit(setwd(cd))
  
  groups <- rev(sort(unique(model_feathers$group_id)))
  data_files <- c()
  for (group in groups){
    zipfile <- paste0('tmp/predictions_', group, '.zip')
    these_files <- model_feathers %>% filter(group_id == !!group)
    
    zippath <- file.path(getwd(), zipfile)
    if (file.exists(zippath)){
      unlink(zippath) #seems it was adding to the zip as opposed to wiping and starting fresh...
    }
    for (i in 1:nrow(these_files)){
      fileout <- file.path(tempdir(), these_files$out_file[i])
      feather::read_feather(these_files$source_filepath[i]) %>%
        select(-ice, -rad_0, date = time) %>%
        mutate(date = as.Date(lubridate::ceiling_date(date, 'days'))) %>%
        write_csv(path = fileout)
      fileout
    }
    
    setwd(tempdir())
    
    zip(zippath, files = these_files$out_file)
    unlink(these_files$out_file)
    setwd(cd)
    data_files <- c(data_files, zipfile)
  }
  scipiper::sc_indicate(outfile, data_file = data_files)
}

zip_irradiance_groups <- function(outfile, output_df, site_groups){
  
  model_feathers <- inner_join(output_df, site_groups, by = 'site_id') %>% 
    select(-site_id)
  
  cd <- getwd()
  on.exit(setwd(cd))
  
  groups <- rev(sort(unique(model_feathers$group_id)))
  data_files <- c()
  for (group in groups){
    zipfile <- paste0('tmp/irradiance_', group, '.zip')
    these_files <- model_feathers %>% filter(group_id == !!group)
    
    zippath <- file.path(getwd(), zipfile)
    if (file.exists(zippath)){
      unlink(zippath) #seems it was adding to the zip as opposed to wiping and starting fresh...
    }
    for (i in 1:nrow(these_files)){
      fileout <- file.path(tempdir(), these_files$out_file[i])
      feather::read_feather(these_files$source_filepath[i]) %>%
        select(date = time, rad_0) %>%
        mutate(date = as.Date(lubridate::ceiling_date(date, 'days'))) %>%
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

#' function is nearly identical to `zip_prediction_groups`. We should modify and merge...but right now that build takes a long time
#' and I am avoiding merging these two functions for that reason. 
#' 
#' How to modify and merge: 
#' 1) could make `build_predict_df` more generic and allow us to specify the `out_file` suffix
#' 2) could add an argument to a generic function `zip_file_groups` that tells us what to do (will need to export differently too)
zip_ice_flags_groups <- function(outfile, file_info_df, site_groups){
  
  model_feathers <- inner_join(file_info_df, site_groups, by = 'site_id') %>% 
    select(-site_id)
  
  
  cd <- getwd()
  on.exit(setwd(cd))
  
  groups <- rev(sort(unique(model_feathers$group_id)))
  data_files <- c()
  for (group in groups){
    zipfile <- paste0('tmp/ice_flags_', group, '.zip')
    
    these_files <- model_feathers %>% filter(group_id == !!group)
    
    zippath <- file.path(getwd(), zipfile)
    if (file.exists(zippath)){
      unlink(zippath) #seems it was adding to the zip as opposed to wiping and starting fresh...
    }
    for (i in 1:nrow(these_files)){
      fileout <- file.path(tempdir(), these_files$out_file[i])
      feather::read_feather(these_files$source_filepath[i]) %>%
        select(date = time, ice) %>% # <- note this line also differs from the temperature export
        mutate(date = as.Date(lubridate::ceiling_date(date, 'days'))) %>%
        write_csv(path = fileout)
      fileout
    }
    
    setwd(tempdir())
    
    zip(zippath, files = these_files$out_file)
    unlink(these_files$out_file)
    setwd(cd)
    data_files <- c(data_files, zipfile)
  }
  scipiper::sc_indicate(outfile, data_file = data_files)
}

zip_temp_obs <- function(outfile, temp_feather){

  cd <- getwd()
  on.exit(setwd(cd))
  
  zippath <- file.path(getwd(), outfile)
  csv_file <- paste0(tools::file_path_sans_ext(basename(outfile)) ,'.csv')
  csv_path <- file.path(tempdir(), csv_file)
  
  feather::read_feather(temp_feather) %>% 
    write_csv(path = csv_path)
  
  setwd(dirname(csv_path))
  zip(zipfile = zippath, files = csv_file)
  setwd(cd)
  
}