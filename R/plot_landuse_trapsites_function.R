plot_landuse_trapsites <- function(raster_data = sle_raster, trap_data = final_cleaned_trap_data$spatial_data) {

  village_landuse_trap_plots <- function(village_name) {
    
    trap_extent <- trap_data %>%
      filter(village == village_name) %>%
      distinct(geometry) %>%
      st_buffer(dist = 500) %>%
      st_bbox()
    
    village_buffer <- trap_extent
    
    village_crop <- crop(raster_data$raster, village_buffer)
    
    trap_plot <- trap_data %>%
      filter(village == village_name) %>%
      distinct(village, visit, grid_number, trap_number, geometry)
    
    village_trap_raster <- tm_shape(village_crop) +
      tm_raster(col = "layer",
                breaks = c(as.numeric(raster_data$raster_labels$sle_landuse), 1600),
                labels = c(raster_data$raster_labels$label),
                palette = raster_data$raster_labels$palette,
                title = "") +
      tm_shape(villages %>%
                 filter(village == village_name)) +
      tm_symbols(alpha = 0, 
                 border.col = "white",
                 border.lwd = 2) +
      tm_shape(trap_plot) +
      tm_dots(col = "black") +
      tm_layout(legend.outside = TRUE) +
      tm_scale_bar(bg.color = "white")
    
    village_trap_waffle <- trap_plot %>%
      mutate(layer = terra::extract(rast(village_crop), vect(trap_plot), fun = "median")[, 2]) %>%
      drop_na() %>%
      group_by(layer) %>%
      summarise(count_layer = n()) %>%
      left_join(., raster_data$raster_labels %>%
                  dplyr::select(sle_landuse, label, palette),
                by = c("layer" = "sle_landuse")) %>%
      ggplot(aes(fill = label, values = count_layer)) +
      geom_waffle(color = "white", make_proportional = TRUE, size = 1.2) +
      scale_fill_manual(values = regional_landuse_palette) +
      coord_flip() +
      theme_minimal() +
      labs(fill = paste0(str_to_sentence(village_name), " land use"),
           caption = "Each rectangle represents ~1% of land area") +
      theme_enhance_waffle()
    
    combined_village_trap_landuse <-  plot_grid(village_trap_waffle +
                                             theme(legend.position = "none"),
                                           tmap_grob(village_trap_raster),
                                           rel_heights = c(0.8, 1), rel_widths = c(1, 2), align = "hv",
                                           labels = c(paste0("Proportional land use (", str_to_sentence(village_name), ")."),
                                                      paste0("Land use map (", str_to_sentence(village_name), ").")))
    
    return(list("village_trap_raster" = village_trap_raster,
                "village_trap_waffle" = village_trap_waffle,
                "combined_village_trap_landuse" = combined_village_trap_landuse))
    
  }
  
  lalehun <- village_landuse_trap_plots("lalehun")
  seilama <- village_landuse_trap_plots("seilama")
  baiama <- village_landuse_trap_plots("baiama")
  # bambawo <- village_landuse_trap_plots("bambawo")
  lambayama <- village_landuse_trap_plots("lambayama")
  
  return(list("lalehun" = lalehun,
              "seilama" = seilama,
              "baiama" = baiama,
              # "bambawo" = bambawo,
              "lambayama" = lambayama))
}
