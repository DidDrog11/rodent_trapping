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
              legend.text.size = 1,
              title = "A)")
  
  tmap_save(fig_1a, here("output", "figures", "fig_1a.png"), dpi = 320, height = 8, width = 9)
  
  baiama_map <- get_map(location = villages %>%
                          filter(village == "baiama") %>%
                          st_coordinates(geometry), zoom = 14, maptype = "satellite", source = "google")
  
  map_bbox <- setNames(unlist(attr(baiama_map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  
  baiama_data <- plot_traps %>%
    filter(village == "Baiama")
  
  ggmap(baiama_map) + 
    coord_sf(crs = st_crs(3857)) + 
    geom_sf(data = baiama_data, aes(colour = trap_success), inherit.aes = FALSE) +
    scale_colour_viridis_c(direction = -1) +
    facet_wrap(~ village) +
    theme_minimal()
}