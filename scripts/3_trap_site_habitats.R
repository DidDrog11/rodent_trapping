source(here::here("scripts", "0_project_library.R"))

geo_sle_raster <- read_rds(here("data", "satellite", "east_sierra_leone_landuse.rds"))

ras_landuse <- read_rds(here("data", "satellite", "raster_landuse.rds"))
ras_palette <- read_rds(here("data", "satellite", "raster_palette.rds"))
labels_raster <- read_rds(here("data", "satellite", "labels_raster.rds"))
plot_palette <- c("#ec7014", "#00441b", "#99d8c9", "#7a0177", "#045a8d")
names(plot_palette) <- c("Farmland", "Forest", "Shrubland", "Urban", "Wetlands")

ras_palette_sl <- read_rds(here("data", "satellite", "raster_palette_sl.rds"))
sle_plot <- read_rds(here("data", "plots", "sle_proportional.rds"))

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

lalehun_landuse_waffle <- plot_landuse_waffle(landuse_lalehun_df)

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

seilama_landuse_waffle <- plot_landuse_waffle(landuse_seilama_df)

seilama_raster_plot <- tm_shape(landuse_seilama,
                                raster.warp = F) +
  tm_raster(col = "landuse",
            breaks = c(0, as.numeric(ras_landuse)[1:12], 1600),
            labels = c("Missing", names(ras_landuse)[1:12]),
            palette = ras_palette,
            legend.show = F)

seilama_plots <- plot_grid(tmap_grob(seilama_raster_plot), seilama_landuse_waffle)
save_plot(here("reports", "figures", "seilama_plots.png"), seilama_plots, ncol = 2)

# lambayama landuse ---------------------------------------------------------

lambayama <- village_poly %>%
  filter(village == "lambayama")

crop_raster <- st_as_stars(geo_sle_raster)

landuse_lambayama <- st_crop(crop_raster, lambayama)

write_rds(landuse_lambayama, here("data", "satellite", "lambayama_landuse.rds"))

landuse_lambayama_df <- as.data.frame(landuse_lambayama, xy = T) %>%
  drop_na(landuse) %>%
  left_join(., labels_raster %>%
              rename("landuse" = "value"),
            by = "landuse") %>%
  label_raster() %>%
  group_by(group) %>%
  mutate(group_n = n(),
         label = recode(label,
                        "Urban Areas" = "Urban areas")) # convert to a dataframe to allow plotting

lambayama_landuse_waffle <- plot_landuse_waffle(landuse_lambayama_df)

lambayama_raster_plot <- tm_shape(landuse_lambayama,
                                raster.warp = F) +
  tm_raster(col = "landuse",
            breaks = c(0, as.numeric(ras_landuse)[1:12], 1600),
            labels = c("Missing", names(ras_landuse)[1:12]),
            palette = ras_palette,
            legend.show = F)

lambayama_plots <- plot_grid(tmap_grob(lambayama_raster_plot), lambayama_landuse_waffle)
save_plot(here("reports", "figures", "lambayama_plots.png"), lambayama_plots, ncol = 2)

# bambawo landuse ---------------------------------------------------------

bambawo <- village_poly %>%
  filter(village == "bambawo")

crop_raster <- st_as_stars(geo_sle_raster)

landuse_bambawo <- st_crop(crop_raster, bambawo)

write_rds(landuse_bambawo, here("data", "satellite", "bambawo_landuse.rds"))

landuse_bambawo_df <- as.data.frame(landuse_bambawo, xy = T) %>%
  drop_na(landuse) %>%
  left_join(., labels_raster %>%
              rename("landuse" = "value"),
            by = "landuse") %>%
  label_raster() %>%
  group_by(group) %>%
  mutate(group_n = n(),
         label = recode(label,
                        "Urban Areas" = "Urban areas")) # convert to a dataframe to allow plotting

bambawo_landuse_waffle <- plot_landuse_waffle(landuse_bambawo_df)

bambawo_raster_plot <- tm_shape(landuse_bambawo,
                                  raster.warp = F) +
  tm_raster(col = "landuse",
            breaks = c(0, as.numeric(ras_landuse)[1:12], 1600),
            labels = c("Missing", names(ras_landuse)[1:12]),
            palette = ras_palette,
            legend.show = F)

bambawo_plots <- plot_grid(tmap_grob(bambawo_raster_plot), bambawo_landuse_waffle)
save_plot(here("reports", "figures", "bambawo_plots.png"), bambawo_plots, ncol = 2)

# baiama landuse ---------------------------------------------------------

baiama <- village_poly %>%
  filter(village == "baiama")

crop_raster <- st_as_stars(geo_sle_raster)

landuse_baiama <- st_crop(crop_raster, baiama)

write_rds(landuse_baiama, here("data", "satellite", "baiama_landuse.rds"))

landuse_baiama_df <- as.data.frame(landuse_baiama, xy = T) %>%
  drop_na(landuse) %>%
  left_join(., labels_raster %>%
              rename("landuse" = "value"),
            by = "landuse") %>%
  label_raster() %>%
  group_by(group) %>%
  mutate(group_n = n(),
         label = recode(label,
                        "Urban Areas" = "Urban areas")) # convert to a dataframe to allow plotting

baiama_landuse_waffle <- plot_landuse_waffle(landuse_baiama_df)

baiama_raster_plot <- tm_shape(landuse_baiama,
                                  raster.warp = F) +
  tm_raster(col = "landuse",
            breaks = c(0, as.numeric(ras_landuse)[1:12], 1600),
            labels = c("Missing", names(ras_landuse)[1:12]),
            palette = ras_palette,
            legend.show = F)

baiama_plots <- plot_grid(tmap_grob(baiama_raster_plot), baiama_landuse_waffle)
save_plot(here("reports", "figures", "baiama_plots.png"), baiama_plots, ncol = 2)

# all villages ------------------------------------------------------------

village_landuse_waffle <- rbind(landuse_lalehun_df %>%
                                  mutate(village = "Lalehun"),
                                landuse_seilama_df %>%
                                  mutate(village = "Seilama"),
                                landuse_bambawo_df %>%
                                  mutate(village = "Bambawo"),
                                landuse_lambayama_df %>%
                                  mutate(village = "Lambayama"),
                                landuse_baiama_df %>%
                                  mutate(village = "Baiama")) %>%
  group_by(village, label) %>%
  summarise(n = n()) %>%
  ggplot(aes(fill = label, values = n)) +
  geom_waffle(color = "white", make_proportional = TRUE, size = 1.2) +
  facet_wrap(~ village, ncol = 1) +
  scale_fill_manual(values = ras_palette_sl) +
  coord_flip() +
  theme_minimal() +
  labs(fill = "Land use") +
  theme_enhance_waffle()

tmap_plots <- plot_grid(tmap_grob(bambawo_raster_plot), 
                        tmap_grob(lalehun_raster_plot),
                        tmap_grob(lambayama_raster_plot),
                        tmap_grob(seilama_raster_plot),
                        tmap_grob(baiama_raster_plot),
                        ncol = 2,
                        labels = c("Bambawo", "Lalehun",
                                   "Lambayama", "Seilama", 
                                   "Baiama"),
                        label_size = 10,
                        align = "v")
villages_plots <- plot_grid(tmap_plots, village_landuse_waffle, nrow = 1)
save_plot(here("reports", "figures", "villages_plots.png"), villages_plots, ncol = 2, base_height = 8, base_width = 12)

# Locations of traps ------------------------------------------------------

trap_sites <- latest_data("trap_sites")

trap_habitat <- trap_sites %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  dplyr::select(village, visit, grid_number, trap_number, trap_uid) %>%
  distinct(village, visit, grid_number, trap_number, .keep_all = T) %>%
  st_transform(2162) %>%
  st_buffer(dist = 100) # Create buffer areas around individual trap locations

bambawo_trap <- plot_landuse_trap("bambawo")
lalehun_trap <- plot_landuse_trap("lalehun")
lambayama_trap <- plot_landuse_trap("lambayama")
seilama_trap <- plot_landuse_trap("seilama")
baiama_trap <- plot_landuse_trap("baiama")

trap_habitats <- plot_grid(bambawo_trap,
                          lalehun_trap,
                          lambayama_trap,
                          seilama_trap,
                          baiama_trap,
                          sle_plot,
                          nrow = 2)
save_plot(here("reports", "figures", "trap_plots.png"), trap_habitats, base_width = 12, base_height = 8)
