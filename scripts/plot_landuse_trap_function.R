plot_landuse_trap <- function(vil_name) {
  
  trap_village <- trap_habitat %>%
    filter(village == vil_name) %>%
    st_transform(4326)
  
  trap_habitat <- st_crop(crop_raster, trap_village)
  
  landuse_traps_df <- as.data.frame(trap_habitat, xy = T) %>%
    left_join(., labels_raster %>%
                rename("landuse" = "value"),
              by = "landuse") %>%
    label_raster() %>%
    group_by(group) %>%
    mutate(group_n = n()) # convert to a dataframe to allow plotting
  
  landuse_traps_plot <- landuse_traps_df %>%
    drop_na() %>%
    group_by(label, group, group_n) %>%
    tally %>%
    ungroup() %>%
    mutate(pct = n/sum(n)) %>%
    ggplot() +
    geom_col(aes(x = reorder(label, group_n), y = pct*100, fill = group)) +
    scale_fill_manual(values = plot_palette) +
    coord_flip() +
    theme_minimal() +
    xlab(NULL) +
    ylab("Percentage land use (100m buffer around trap)") +
    labs(fill = "Land use",
         title = snakecase::to_sentence_case(vil_name)) +
    theme(legend.position = "none")
}