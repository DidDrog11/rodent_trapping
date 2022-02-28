create_fig_1 <- function(data = final_cleaned_trap_data$clean_sites, spatial_data = final_cleaned_trap_data$spatial_data) {
  
  data %>%
    group_by(village, visit, grid_number) %>%
    summarise(n_trapnights = n(),
              n_captures = sum(!is.na(rodent_uid))) %>%
    mutate(proportion_success = n_captures/n_trapnights * 100)
  
  first_visit_trapid <- data %>%
    group_by(village, grid_number) %>%
    filter(as.numeric(visit) == min(as.numeric(visit))) %>%
    pull(trap_uid)
  
  trap_site_polygons <- spatial_data %>%
    filter(trap_uid %in% first_visit_trapid) %>%
    group_by(village, grid_number) %>%
    summarise(geometry = st_union(geometry)) %>%
    st_convex_hull() %>%
    ungroup()

  trap_site_centroids <- spatial_data %>%
    filter(trap_uid %in% first_visit_trapid) %>%
    group_by(village, grid_number) %>%
    summarise(geometry = st_union(geometry)) %>%
    st_convex_hull() %>%
    st_centroid()
  
  plot_traps <- spatial_data %>%
    group_by(village, grid_number) %>%
    mutate(trap_success = sum(!is.na(rodent_uid))/n() * 100) %>%
    distinct(village, grid_number, trap_success, geometry) %>%
    mutate(grid_number = case_when(as.numeric(grid_number) == 7 ~ 6,
                                   TRUE ~ as.numeric(grid_number)),
           grid_number = factor(grid_number, levels = c(1, 2, 3, 4, 5, 6), labels = c("1", "2", "3", "4", "5", "6")),
           village = str_to_sentence(village))
  
  village_sites <- villages %>%
    mutate(village = str_to_sentence(village))
  
  sierra_leone <- read_rds(here("data", "spatial", "gadm36_SLE_0_sp.rds")) %>%
    st_as_sf() %>%
    st_set_crs(value = project_crs)
  
  eastern_province <- read_rds(here("data", "spatial", "gadm36_SLE_1_sp.rds")) %>%
    st_as_sf() %>%
    filter(NAME_1 == "Eastern") %>%
    st_set_crs(value = project_crs)
  
  osm_sl <- read_osm(sierra_leone, ext = 1)
  
  fig_1a <- tm_shape(osm_sl) +
    tm_rgb() +
    tm_shape(sierra_leone) +
    tm_view(bbox = st_bbox(sierra_leone),
            set.zoom.limits = c(7, 12)) +
    tm_borders() +
    tm_shape(eastern_province) +
    tm_borders(col = "blue", lwd = 2) +
    tm_shape(village_sites) +
    tm_symbols(shape = 18,
               col = "village",
               palette = village_palette,
               title.col = "Village",
               border.col = "white",
               border.lwd = 1) +
    tm_layout(legend.bg.color = "white",
              legend.frame = TRUE,
              legend.text.size = 1)
  
  tmap_save(fig_1a, here("output", "figures", "fig_1a.png"), dpi = 320, height = 8, width = 9)
  
  trapping_location_centroid <- lapply(village_bbox[names(village_bbox) != "bambawo"], function(x) st_centroid(x))
  zoom = c(17, 17, 17, 15)
  fig_1b_e <- list()
  
  for(i in 1:length(trapping_location_centroid)) {  
    village_map <- get_map(location = trapping_location_centroid[[i]] %>%
                             st_coordinates(geometry), zoom = zoom[i], maptype = "satellite", source = "google")
    
    map_bbox <- setNames(unlist(attr(village_map, "bb")), 
                         c("ymin", "xmin", "ymax", "xmax"))
    bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
    attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
    attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
    attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
    attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
    
    data <- plot_traps %>%
      filter(village == str_to_sentence(names(trapping_location_centroid[i])))
    
    fig_1b_e[[i]] <- ggmap(village_map) + 
      coord_sf(crs = st_crs(3857)) + 
      geom_sf(data = data, aes(colour = trap_success), inherit.aes = FALSE) +
      scale_colour_viridis_c(direction = -1, limits = c(0, 11), option = "magma") +
      theme_minimal() +
      labs(x = element_blank(),
           y = element_blank(),
           colour = "Trap success %",
           title = str_to_sentence(names(trapping_location_centroid[i])))
  }
  
  legend <- get_legend(fig_1b_e[[1]] + theme(legend.direction = "vertical"))
  top_row <- plot_grid(tmap_grob(fig_1a), labels = c("A"))
  map_rows <- plot_grid(plotlist = list(plot_grid(plotlist = list(fig_1b_e[[1]] + theme(legend.position = "none"),
                                        fig_1b_e[[2]] + theme(legend.position = "none"),
                                        fig_1b_e[[3]] + theme(legend.position = "none"),
                                        fig_1b_e[[4]] + theme(legend.position = "none")),
                        align = "vh", labels = c("B", "C", "D", "E")),
                        legend), rel_widths = c(3, 0.4))
  
  save_plot(plot = plot_grid(top_row, map_rows, rel_widths = c(2, 4)), filename = here("output", "figures", "fig1_combined.png"), base_height = 12, base_width = 16)
}