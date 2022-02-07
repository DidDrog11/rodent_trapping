save_landuse_plots <- function(plot_list = landuse_plots, combined_only = FALSE) {
  
  if(combined_only == FALSE) {
    
    for(n in 1:length(plot_list)) {
      
      y <- plot_list[[n]]
      
      village_name = names(plot_list[n])
      
      tmap_fig <- plot_list[[n]][[1]]
      
      tmap_save(tm = tmap_fig, filename = here("output", "figures", "landuse_plots", paste0(village_name, "_landuse_raster.png")),
                width = 12, height = 10)
      
      save_plot(plot = plot_list[[n]][[2]], filename = here("output", "figures", "landuse_plots", paste0(village_name, "_landuse_waffle.png")),
                base_width = 8, base_height = 8)
      
      save_plot(plot = plot_list[[n]][[3]], filename = here("output", "figures", "landuse_plots", paste0(village_name, "_combined_landuse_plots.png")),
                base_width = 16, base_height = 12)
    }
  }
  
  if(combined_only == TRUE) {
    
    for(n in 1:length(plot_list)) {
      
      y <- plot_list[[n]]
      
      village_name = names(plot_list[n])
      
      save_plot(plot = plot_list[[n]][[3]], filename = here("output", "figures", "landuse_plots", paste0(village_name, "_combined_landuse_trap_plots.png")),
                base_width = 16, base_height = 12)
    }
  }
}
