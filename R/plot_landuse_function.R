plot_landuse <- function(data = sle_raster) {
  
  ### Region landuse
  
  missing = tibble(sle_landuse = 0, label = "Missing", group = "Missing", palette = "#d9d9d9")
  
  data$raster_labels <- data$raster_labels %>%
    bind_rows(missing) %>%
    arrange(sle_landuse)
  
  region_landuse_raster <- tm_shape(data$raster) +
    tm_raster(col = "layer",
              breaks = c(as.numeric(data$raster_labels$sle_landuse), 1600),
              labels = c(data$raster_labels$label),
              palette = data$raster_labels$palette,
              title = "") +
    tm_layout(legend.outside = TRUE,
              main.title = "Land use in Eastern Province, \nSierra Leone") +
    tm_compass() +
    tm_scale_bar()
  
  regional_landuse_palette <- data$raster_labels %>%
    filter(sle_landuse %in% c(108, 109, 1402, 1403, 306, 307, 1405)) %>%
    pull(palette)
  
  names(regional_landuse_palette) <- data$raster_labels %>%
    filter(sle_landuse %in% c(108, 109, 1402, 1403, 306, 307, 1405)) %>%
    pull(label)
  
  region_landuse_waffle <- as.data.frame(data$raster) %>%
    tibble() %>%
    drop_na() %>%
    group_by(layer) %>%
    summarise(count_layer = n()) %>%
    left_join(., data$raster_labels %>%
                dplyr::select(sle_landuse, label, palette),
              by = c("layer" = "sle_landuse")) %>%
    ggplot(aes(fill = label, values = count_layer)) +
    geom_waffle(color = "white", make_proportional = TRUE, size = 1.2) +
    scale_fill_manual(values = regional_landuse_palette) +
    coord_flip() +
    theme_minimal() +
    labs(fill = "Eastern province land use",
         caption = "Each rectangle represents ~1% of land area") +
    theme_enhance_waffle()
  
  combined_eastern_province_landuse <- plot_grid(tmap_grob(region_landuse_raster), region_landuse_waffle +
                                                   theme(legend.position = "none"),
                                                 rel_heights = c(1, 0.8), rel_widths = c(2, 1))
  
  eastern_province_landuse <- list(eastern_province_raster = region_landuse_raster,
                                   eastern_province_waffle = region_landuse_waffle)
  
  ### Study area landuse
  
  village_landuse_plots <- function(village_name) {
  
  village_buffer <- villages %>%
    filter(village == village_name) %>%
    st_buffer(dist = 2000)
  
  village_crop <- crop(data$raster, village_buffer)
  
  village_raster <- tm_shape(village_crop) +
    tm_raster(col = "layer",
              breaks = c(as.numeric(data$raster_labels$sle_landuse), 1600),
              labels = c(data$raster_labels$label),
              palette = data$raster_labels$palette,
              title = "") +
    tm_shape(villages %>%
               filter(village == village_name)) +
    tm_symbols(alpha = 0, 
               border.col = "black",
               border.lwd = 2) +
    tm_layout(legend.outside = TRUE,
              main.title = paste0("Land use in ", str_to_sentence(village_name))) +
    tm_scale_bar()
  
  village_waffle <- as.data.frame(village_crop) %>%
    tibble() %>%
    drop_na() %>%
    group_by(layer) %>%
    summarise(count_layer = n()) %>%
    left_join(., data$raster_labels %>%
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
  
  return(list("village_raster" = village_raster,
              "village_waffle" = village_waffle))
    
    }
  
  lalehun <- village_landuse_plots("lalehun")
  seilama <- village_landuse_plots("seilama")
  baiama <- village_landuse_plots("baiama")
  bambawo <- village_landuse_plots("bambawo")
  lambayama <- village_landuse_plots("lambayama")
  
  return(list("eastern_province" = eastern_province_landuse,
              "lalehun" = lalehun,
              "seilama" = seilama,
              "baiama" = baiama,
              "bambawo" = bambawo,
              "lambayama" = lambayama))
}
