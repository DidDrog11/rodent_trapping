plot_landuse_waffle <- function(dataframe) {
  dataframe %>%
    drop_na() %>%
    group_by(label, group, group_n) %>%
    tally %>%
    ungroup() %>%
    mutate(pct = n/sum(n)*100) %>%
    ggplot(aes(fill = label, values = n)) +
    geom_waffle(color = "white", make_proportional = TRUE, size = 1.2) +
    scale_fill_manual(values = ras_palette_sl) +
    coord_flip() +
    theme_minimal() +
    labs(fill = "Land use") +
    theme_enhance_waffle()
}