plot_trap_on_habitat <- function(vil_name) {
  ras_palette <- structure(c("#d9d9d9", "#00441b", "#006d2c", "#238b45", "#99d8c9", 
                             "#ccece6", "#045a8d", "#a6bddb", "#fee391", "#fec44f", "#ec7014", 
                             "#662506", "#7a0177"), .Names = c("Missing", "Forest - lowland", 
                                                               "Forest", "Forest - montane", "Shrubland", "Shrubland - high altitude", 
                                                               "Wetlands", "Arable land", "Pastureland", "Plantations", "Rural Gardens", 
                                                               "Urban Areas", NA))
  
  vil_raster <- get(paste0(vil_name, "_landuse"), envir = .GlobalEnv)
  vil_bbox <- get(paste0(vil_name, "_bbox"), envir = .GlobalEnv)
  
  vil_points <- traps %>%
    filter(village == vil_name) %>%
    st_as_sf(coords = c("lon", "lat")) %>%
    distinct(visit, grid_number, trap_number, rodent_id, geometry)
  
  plot <- tm_shape(vil_raster,
                   bbox = vil_bbox,
                   raster.warp = F) +
    tm_raster(col = "landuse",
              breaks = c(0, as.numeric(ras_landuse)[1:12], 1600),
              labels = c("Missing", names(ras_landuse)[1:12]),
              palette = ras_palette,
              legend.show = F) +
    tm_shape(vil_points) +
    tm_dots(col = "white") +
    tm_layout(legend.outside = T)
}
