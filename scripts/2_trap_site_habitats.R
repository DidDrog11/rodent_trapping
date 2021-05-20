source(here::here("scripts", "0_project_library.R"))
source(here("scripts", "0_label_raster.R"))

geo_sle_raster <- read_rds(here("data", "satellite", "east_sierra_leone_landuse.rds"))

ras_landuse <- read_rds(here("data", "satellite", "raster_landuse.rds"))
ras_palette <- read_rds(here("data", "satellite", "raster_palette.rds"))
labels_raster <- read_rds(here("data", "satellite", "labels_raster.rds"))
plot_palette <- c("#ec7014", "#00441b", "#99d8c9", "#7a0177", "#045a8d")
ras_palette_sl <- read_rds(here("data", "satellite", "raster_palette_sl.rds"))

villages <- tibble(village = c("lalehun", "seilama"),
                   x  = c(lalehun_coords[1],
                          seilama_coords[1]),
                   y = c(lalehun_coords[2],
                         seilama_coords[2])) %>%
  st_as_sf(coords = c("x", "y")) %>%
  st_set_crs(4326)

village_poly <- st_transform(villages, 2162) %>%
  st_buffer(dist = 2000)

village_poly <- st_transform(village_poly, 4326)

# Lalehun landuse ---------------------------------------------------------

lalehun <- village_poly %>%
  filter(village == "lalehun")

crop_raster <- st_as_stars(geo_sle_raster)

landuse_lalehun <- st_crop(crop_raster, lalehun)

write_rds(landuse_lalehun, here("data", "satellite", "lalehun_landuse.rds"))

landuse_lalehun_df <- as.data.frame(landuse_lalehun, xy = T) %>%
  drop_na(landuse) %>%
  left_join(., labels_raster %>%
              rename("landuse" = "value"),
            by = "landuse") %>%
  label_raster() %>%
  group_by(group) %>%
  mutate(group_n = n(),
         label = recode(label,
                        "Urban Areas" = "Urban areas")) # convert to a dataframe to allow plotting

lalehun_landuse_plot <- landuse_lalehun_df %>%
  drop_na() %>%
  group_by(label, group, group_n) %>%
  tally %>%
  ungroup() %>%
  mutate(pct = n/sum(n)*100)

lalehun_landuse_waffle <- ggplot(lalehun_landuse_plot, aes(fill = label, values = n)) +
  geom_waffle(color = "white", make_proportional = TRUE, size = 1.2) +
  scale_fill_manual(values = ras_palette_sl) +
  coord_flip() +
  theme_minimal() +
  labs(fill = "Land use") +
  theme_enhance_waffle()

lalehun_raster_plot <- tm_shape(landuse_lalehun,
         raster.warp = F) +
  tm_raster(col = "landuse",
            breaks = c(0, as.numeric(ras_landuse)[1:12], 1600),
            labels = c("Missing", names(ras_landuse)[1:12]),
            palette = ras_palette,
            legend.show = F)

lalehun_plots <- plot_grid(tmap_grob(lalehun_raster_plot), lalehun_landuse_waffle)
save_plot(here("reports", "figures", "lalehun_plots.png"), lalehun_plots, ncol = 2)


# Seilama landuse ---------------------------------------------------------

seilama <- village_poly %>%
  filter(village == "seilama")

landuse_seilama <- st_crop(crop_raster, seilama)

#Change raster values to match ground truth
landuse_seilama[[1]][20, 21] = 106 #Urban to Forest
landuse_seilama[[1]][21, 21] = 1405 #Plantation to Urban

write_rds(landuse_seilama, here("data", "satellite", "seilama_landuse.rds"))

landuse_seilama_df <- as.data.frame(landuse_seilama, xy = T) %>%
  drop_na(landuse) %>%
  left_join(., labels_raster %>%
              rename("landuse" = "value"),
            by = "landuse") %>%
  label_raster() %>%
  group_by(group) %>%
  mutate(group_n = n(),
         label = recode(label,
                        "Urban Areas" = "Urban areas")) # convert to a dataframe to allow plotting

seilama_landuse_plot <- landuse_seilama_df %>%
  drop_na() %>%
  group_by(label, group, group_n) %>%
  tally %>%
  ungroup() %>%
  mutate(pct = n/sum(n)*100)

seilama_landuse_waffle <- ggplot(seilama_landuse_plot, aes(fill = label, values = n)) +
  geom_waffle(color = "white", make_proportional = TRUE, show.legend = F, size = 1.2) +
  scale_fill_manual(values = ras_palette_sl) +
  coord_flip() +
  theme_minimal() +
  labs(caption = "Each rectangle represents ~1% of land area") +
  theme_enhance_waffle()

seilama_raster_plot <- tm_shape(landuse_seilama,
                                raster.warp = F) +
  tm_raster(col = "landuse",
            breaks = c(0, as.numeric(ras_landuse)[1:12], 1600),
            labels = c("Missing", names(ras_landuse)[1:12]),
            palette = ras_palette,
            legend.show = F)

seilama_plots <- plot_grid(tmap_grob(seilama_raster_plot), seilama_landuse_waffle)
save_plot(here("reports", "figures", "seilama_plots.png"), seilama_plots, ncol = 2)

village_landuse_waffle <- rbind(lalehun_landuse_plot %>%
                                  mutate(village = "Lalehun"),
                                seilama_landuse_plot %>%
                                  mutate(village = "Seilama")) %>%
  ggplot(aes(fill = label, values = n)) +
  geom_waffle(color = "white", make_proportional = TRUE, size = 1.2) +
  facet_wrap(~ village, ncol = 1) +
  scale_fill_manual(values = ras_palette_sl) +
  coord_flip() +
  theme_minimal() +
  labs(fill = "Land use") +
  theme_enhance_waffle()

tmap_plots <- plot_grid(tmap_grob(lalehun_raster_plot), tmap_grob(seilama_raster_plot),
                        nrow = 2,
                        labels = c("Lalehun", "Seilama"))
villages_plots <- plot_grid(tmap_plots, village_landuse_waffle, nrow = 1)
save_plot(here("reports", "figures", "villages_plots.png"), villages_plots, ncol = 2)

# Locations of traps ------------------------------------------------------

trap_sites <- read_csv(here("data", "trap_sites.csv")) 

trap_habitat <- trap_sites %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  dplyr::select(village, visit, grid_number, trap_number, trap_uid) %>%
  distinct(visit, grid_number, trap_number, .keep_all = T) %>%
  st_transform(2162) %>%
  st_buffer(dist = 100)

trap_lalehun <- trap_habitat %>%
  filter(village == "lalehun" & visit == 1) %>%
  st_transform(4326)

lalehun_trap_habitat <- st_crop(crop_raster, trap_lalehun)

landuse_traps_lalehun_df <- as.data.frame(lalehun_trap_habitat, xy = T) %>%
  left_join(., labels_raster %>%
              rename("landuse" = "value"),
            by = "landuse") %>%
  label_raster() %>%
  group_by(group) %>%
  mutate(group_n = n()) # convert to a dataframe to allow plotting

lalehun_landuse_traps_plot <- landuse_traps_lalehun_df %>%
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
  ylab("Percentage land use") +
  labs(fill = "Land use")

trap_seilama <- trap_habitat %>%
  filter(village == "seilama" & visit == 1) %>%
  st_transform(4326)

seilama_trap_habitat <- st_crop(crop_raster, trap_seilama)

landuse_traps_seilama_df <- as.data.frame(seilama_trap_habitat, xy = T) %>%
  left_join(., labels_raster %>%
              rename("landuse" = "value"),
            by = "landuse") %>%
  label_raster() %>%
  group_by(group) %>%
  mutate(group_n = n()) # convert to a dataframe to allow plotting

seilama_landuse_traps_plot <- landuse_traps_seilama_df %>%
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
  ylab("Percentage land use") +
  labs(fill = "Land use")
