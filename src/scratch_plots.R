

plot_overall_rmse <- function(fileout, GLM_kw_fl, GLM_fl, PGDL_fl){

  GLM_kw <- read_csv(GLM_kw_fl) %>% mutate(type = 'GLM3 time varying Kd')
  GLM <- read_csv(GLM_fl) %>% mutate(type = 'GLM3 static Kd')
  PGDL <- read_csv(PGDL_fl) %>% mutate(type = 'PGDL time varying Kd')

  plot_data <- rbind(GLM_kw, GLM) %>% rbind(PGDL) %>%
    filter(site_id %in% PGDL$site_id)

  ggplot(plot_data, aes(x=type, y=rmse, fill=type, color = type)) +
    geom_violin(trim=FALSE) +
    geom_boxplot(width=0.1, fill="white", color = 'black') + theme_minimal() +
    theme(legend.position="none")
  browser()
}

plot_facet_rmse <- function(fileout, GLM_kw, GLM, PGDL_fl){
  PGDL <- read_csv(PGDL_fl) %>% mutate(type = 'PGDL time varying Kd') %>% select(-source_id)
  GLM_kw <- mutate(GLM_kw, type = 'GLM3 time varying Kd')
  GLM <- mutate(GLM, type = 'GLM3 static Kd')

  get_slope <- function(x,y){
    model <- lm(x~y)
    slope <- model$coefficients[2]
    p_val <- summary(model)$coefficients[2,4]
    if (p_val < 0.05){
      slope = slope
    } else {
      slope = NA
    }
    return(slope)
  }

  kw_data <- readRDS("../lake-temperature-model-prep/4_params_munge/out/toha_varying_kw.rds") %>%
    filter(site_id %in% PGDL$site_id) %>%
    mutate(year = lubridate::year(time)) %>%
    group_by(site_id, year) %>% summarize(Kd = median(Kd)) %>%
    group_by(site_id) %>% summarize(slope = get_slope(x = year, y = Kd))

  z_bins <- c(0, 0.5, 2, 5, 10, 20, 70)
  doy_bins <- c(-12, 78, 172, 264, 355)
  model_data <- rbind(GLM_kw, GLM) %>% rbind(PGDL) %>%
    filter(site_id %in% PGDL$site_id) %>%
    left_join(kw_data, by = 'site_id') %>%
    mutate(z_bin = cut(depth, z_bins, right = FALSE)) %>%
    mutate(z_bin = as.character(z_bin)) %>%
    filter(!is.na(z_bin)) %>%
    mutate(doy = lubridate::yday(date), doy = case_when(
      doy > 355 ~ 355 - doy,
      TRUE ~ doy
    )) %>%
    mutate(season = as.character(cut(doy, doy_bins, right = FALSE)), season = case_when(
      season == '[-12,78)' ~ 'winter',
      season == '[78,172)' ~ 'spring',
      season == '[172,264)' ~ 'summer',
      season == '[264,355)' ~ 'fall'
    )) %>%
    filter(!is.na(season)) %>%
    arrange(z_bin) %>%
    mutate(title = case_when(
      z_bin == '[0,0.5)' ~ '0-0.5m',
      z_bin == '[0.5,2)' ~ '0.5-2m',
      z_bin == '[2,5)' ~ '2-5m',
      z_bin == '[5,10)' ~ '5-10m',
      z_bin == '[10,20)' ~ '10-20m',
      z_bin == '[20,70)' ~ '20-70m'
    ))

  browser()
  model_data %>% mutate(t_factor = factor(title, levels=c('0-0.5m','0.5-2m','2-5m','5-10m','10-20m','20-70m'))) %>%
    group_by(type, t_factor, site_id) %>% summarize(rmse = sqrt(mean((pred-obs)^2, na.rm=TRUE)), n = length(pred)) %>%
    filter(n > 9) %>%
    ggplot(data = ., aes(x=type, y=rmse, fill=type, color = type)) +
    geom_violin(trim=FALSE) +
    geom_boxplot(width=0.1, fill="white", color = 'black') + theme_minimal() +
    theme(legend.position="none", axis.text.x=element_blank(), axis.title.x=element_blank()) +
    facet_wrap(~ t_factor, scales = "free_y") %>% print()

  model_data %>% mutate(s_factor = factor(season, levels=c('winter','spring','summer','fall'))) %>%
    group_by(type, s_factor, site_id) %>% summarize(rmse = sqrt(mean((pred-obs)^2, na.rm=TRUE)), n = length(pred)) %>%
    filter(n > 9) %>%
    ggplot(data = ., aes(x=type, y=rmse, fill=type, color = type)) +
    geom_violin(trim=FALSE) +
    geom_boxplot(width=0.1, fill="white", color = 'black') + theme_minimal() +
    theme(legend.position="none", axis.text.x=element_blank(), axis.title.x=element_blank()) +
    facet_grid(~s_factor, scales = "free_y") %>% print()

  model_data %>% mutate(s_factor = factor(season, levels=c('winter','spring','summer','fall'))) %>%
    mutate(title = case_when(
      title %in% c('0-0.5m', '0.5-2m', '2-5m') ~ '0-5m',
      title %in% c('5-10m', '10-20m') ~ '5-20m',
      TRUE ~ 'none'
    )) %>%
    group_by(type, title, s_factor, site_id) %>% summarize(rmse = sqrt(mean((pred-obs)^2, na.rm=TRUE)), n = length(pred)) %>%
    filter(n > 9, title != 'none') %>%
    ggplot(data = ., aes(x=type, y=rmse, fill=type, color = type)) +
    geom_violin(trim=FALSE) +
    geom_boxplot(width=0.1, fill="white", color = 'black') + theme_minimal() +
    theme(legend.position="none", axis.text.x=element_blank(), axis.title.x=element_blank()) +
    facet_grid(title ~ s_factor, scales = "free") %>% print()

  model_data %>% filter(season %in% c('summer','fall')) %>%
    mutate(slope = case_when(
      is.na(slope) ~ 'none',
      slope < -50 ~ 'negative',
      slope > 50 ~ 'positive',
      TRUE ~ 'none'
    )) %>%
    mutate(title = case_when(
      title %in% c('0-0.5m', '0.5-2m', '2-5m') ~ '0-5m',
      title %in% c('5-10m', '10-20m') ~ '5-20m',
      TRUE ~ 'none'
    )) %>%
    group_by(type, slope, title, site_id) %>% summarize(rmse = sqrt(mean((pred-obs)^2, na.rm=TRUE)), n = length(pred)) %>%
    filter(n > 9, title != 'none') %>%
    ggplot(data = ., aes(x=type, y=rmse, fill=type, color = type)) +
    geom_violin(trim=FALSE) +
    geom_boxplot(width=0.1, fill="white", color = 'black') + theme_minimal() +
    theme(legend.position="none", axis.text.x=element_blank(), axis.title.x=element_blank()) +
    scale_y_continuous(limits = c(0, 8)) +
    facet_grid(title ~ slope, scales = "free") %>% print()

}


