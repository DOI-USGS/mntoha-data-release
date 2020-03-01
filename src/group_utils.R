
generate_lat_lon_groups <- function(x0, y0, x_num, y_num, cell_res){
  ldas_crs <- "+init=epsg:4326"
  
  ldas_grid_sfc <- sf::st_make_grid(cellsize = cell_res, n = c(x_num, y_num),
                                    offset = c(x0-cell_res/2, y0-cell_res/2), crs = ldas_crs)
  # cells count left to right, then next row, then left to right
  
  groups <- data.frame(group_id = rep(NA_character_, length(ldas_grid_sfc)), stringsAsFactors = FALSE)
  for (i in 1:length(ldas_grid_sfc)){
    # format of N46.125-48.625_W86.5-89.25
    this_box <- st_bbox(ldas_grid_sfc[i])
    groups$group_id[i] <- sprintf("N%1.2f-%1.2f_W%1.2f-%1.2f", this_box$ymin, this_box$ymax, -this_box$xmax, -this_box$xmin)
  }
  
  ldas_grid <- st_sf(groups, ldas_grid_sfc)
  return(ldas_grid)
}

generate_group_rects <- function(){
  unit_cell <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,-1), c(0,-1), c(0,0))))
  shift_scale <- function(shift = c(0,0), scale = 1){
    (unit_cell * scale)+shift
  }
  group_rects <- st_sfc(crs = "+init=epsg:4326",
    shift_scale(c(-98,51), c(3,4)),
    shift_scale(c(-95,51), c(3.5,3)),
    shift_scale(c(-91.5,49), c(2.5,1)),
    shift_scale(c(-95,48)),
    shift_scale(c(-94,48), c(1, 0.5)),
    shift_scale(c(-94,47.5), c(1, 0.5)),
    shift_scale(c(-93,48), c(1, 2)),
    shift_scale(c(-92,48)),
    shift_scale(c(-91,48), c(1, 2)),
    shift_scale(c(-90,48), c(1,2)),
    shift_scale(c(-89,47.5), c(5.5, 1.5)),
    shift_scale(c(-98,47), c(2, 3)), #12
    shift_scale(c(-96,47), c(1.5,0.5)),
    shift_scale(c(-96,46.5), c(1.5,0.5)),
    shift_scale(c(-94.5,47), c(0.5,1)),
    shift_scale(c(-94,47)),
    shift_scale(c(-92,47)),
    shift_scale(c(-92,46)),
    shift_scale(c(-91,46), c(1.5,0.5)),
    shift_scale(c(-96,45), c(2.5,1)), #20
    shift_scale(c(-98,44), c(7,1.5)),
    shift_scale(c(-91,45.5), c(1.5,5)),
    shift_scale(c(-89.5,46), c(0.5,1)),
    shift_scale(c(-89,46)),
    shift_scale(c(-88,46), c(3,3)),
    shift_scale(c(-85,46), c(2,3)),
    shift_scale(c(-89.5,45), c(1.5,2)),
    shift_scale(c(-89.5,43), c(4,1.5)),
    shift_scale(c(-85.5,43), c(3,1.5)),
    shift_scale(c(-89.5,41.5), c(5,1)),
    shift_scale(c(-96,46), c(1.5,1)),
    shift_scale(c(-94.5,46)),
    shift_scale(c(-93.5,46)),
    shift_scale(c(-92.5,46), c(0.5,1)),
    shift_scale(c(-93.5,45), c(2.5,1)))
    
  groups <- data.frame(group_id = rep(NA_character_, length(group_rects)), stringsAsFactors = FALSE)
  for (i in 1:length(group_rects)){
    # format of N46.125-48.625_W86.5-89.25
    this_box <- st_bbox(group_rects[i])
    groups$group_id[i] <- sprintf("%02d_N%1.2f-%1.2f_W%1.2f-%1.2f", i, this_box$ymin, this_box$ymax, -this_box$xmax, -this_box$xmin)
  }
  
  return(st_sf(groups, group_rects))
                        
}

assign_group_id <- function(points, polygons){
  points %>% mutate(group_id = {st_intersects(x = points, y = polygons) %>% unlist %>% polygons$group_id[.]}) %>% 
    st_drop_geometry()
}