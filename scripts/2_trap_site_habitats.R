source(here::here("scripts", "0_project_library.R"))
source(here("scripts", "0_label_raster.R"))
source(here("scripts",  "plot_landuse_waffle_function.R"))
source(here("scripts",  "plot_landuse_trap_function.R"))

geo_sle_raster <- read_rds(here("data", "satellite", "east_sierra_leone_landuse.rds"))

ras_landuse <- read_rds(here("data", "satellite", "raster_landuse.rds"))
ras_palette <- read_rds(here("data", "satellite", "raster_palette.rds"))
labels_raster <- read_rds(here("data", "satellite", "labels_raster.rds"))
plot_palette <- c("#ec7014", "#00441b", "#99d8c9", "#7a0177", "#045a8d")
ras_palette_sl <- read_rds(here("data", "satellite", "raster_palette_sl.rds"))

villages <- tibble(village = c("lalehun", "seilama", "lambeyama", "bambawo", "w_kenema"),
                   x  = c(lalehun_coords[1],
                          seilama_coords[1],
                          lambeyama_coords[1],
                          bambawo_coords[1],
                          west_kenema_coords[1]),
                   y = c(lalehun_coords[2],
                         seilama_coords[2],
                         lambeyama_coords[2],
                         bambawo_coords[2],
                         west_kenema_coords[2])) %>%
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

# Lambeyama landuse ---------------------------------------------------------

lambeyama <- village_poly %>%
  filter(village == "lambeyama")

crop_raster <- st_as_stars(geo_sle_raster)

landuse_lambeyama <- st_crop(crop_raster, lambeyama)

write_rds(landuse_lambeyama, here("data", "satellite", "lambeyama_landuse.rds"))

landuse_lambeyama_df <- as.data.frame(landuse_lambeyama, xy = T) %>%
  drop_na(landuse) %>%
  left_join(., labels_raster %>%
              rename("landuse" = "value"),
            by = "landuse") %>%
  label_raster() %>%
  group_by(group) %>%
  mutate(group_n = n(),
         label = recode(label,
                        "Urban Areas" = "Urban areas")) # convert to a dataframe to allow plotting

lambeyama_landuse_waffle <- plot_landuse_waffle(landuse_lambeyama_df)

lambeyama_raster_plot <- tm_shape(landuse_lambeyama,
                                raster.warp = F) +
  tm_raster(col = "landuse",
            breaks = c(0, as.numeric(ras_landuse)[1:12], 1600),
            labels = c("Missing", names(ras_landuse)[1:12]),
            palette = ras_palette,
            legend.show = F)

lambeyama_plots <- plot_grid(tmap_grob(lambeyama_raster_plot), lambeyama_landuse_waffle)
save_plot(here("reports", "figures", "lambeyama_plots.png"), lambeyama_plots, ncol = 2)

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

# West Kenema landuse ---------------------------------------------------------

w_kenema <- village_poly %>%
  filter(village == "w_kenema")

crop_raster <- st_as_stars(geo_sle_raster)

landuse_w_kenema <- st_crop(crop_raster, w_kenema)

write_rds(landuse_w_kenema, here("data", "satellite", "w_kenema_landuse.rds"))

landuse_w_kenema_df <- as.data.frame(landuse_w_kenema, xy = T) %>%
  drop_na(landuse) %>%
  left_join(., labels_raster %>%
              rename("landuse" = "value"),
            by = "landuse") %>%
  label_raster() %>%
  group_by(group) %>%
  mutate(group_n = n(),
         label = recode(label,
                        "Urban Areas" = "Urban areas")) # convert to a dataframe to allow plotting

w_kenema_landuse_waffle <- plot_landuse_waffle(landuse_w_kenema_df)

w_kenema_raster_plot <- tm_shape(landuse_w_kenema,
                                  raster.warp = F) +
  tm_raster(col = "landuse",
            breaks = c(0, as.numeric(ras_landuse)[1:12], 1600),
            labels = c("Missing", names(ras_landuse)[1:12]),
            palette = ras_palette,
            legend.show = F)

w_kenema_plots <- plot_grid(tmap_grob(w_kenema_raster_plot), w_kenema_landuse_waffle)
save_plot(here("reports", "figures", "w_kenema_plots.png"), w_kenema_plots, ncol = 2)

# all villages ------------------------------------------------------------

village_landuse_waffle <- rbind(landuse_lalehun_df %>%
                                  mutate(village = "Lalehun"),
                                landuse_seilama_df %>%
                                  mutate(village = "Seilama"),
                                landuse_bambawo_df %>%
                                  mutate(village = "Bambawo"),
                                landuse_lambeyama_df %>%
                                  mutate(village = "Lambeyama")) %>%
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
                        tmap_grob(lambeyama_raster_plot),
                        tmap_grob(seilama_raster_plot),
                        ncol = 2,
                        labels = c("Bambawo", "Lalehun",
                                   "Lambeyama", "Seilama"))
villages_plots <- plot_grid(tmap_plots, village_landuse_waffle, nrow = 1)
save_plot(here("reports", "figures", "villages_plots.png"), villages_plots, ncol = 2)

# Locations of traps ------------------------------------------------------

trap_sites <- read_csv(here("data", "trap_sites.csv")) 

trap_habitat <- trap_sites %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  dplyr::select(village, visit, grid_number, trap_number, trap_uid) %>%
  distinct(visit, grid_number, trap_number, .keep_all = T) %>%
  st_transform(2162) %>%
  st_buffer(dist = 100) # Create buffer areas around individual trap locations

bambawo_trap <- plot_landuse_trap("bambawo")
lalehun_trap <- plot_landuse_trap("lalehun")
#lambeyama_trap <- plot_landuse_trap("lambeyama")
seilama_trap <- plot_landuse_trap("seilama")

trap_habitats <- plot_grid(bambawo_trap,
                          lalehun_trap,
                          #lambeyama_trap,
                          seilama_trap,
                          nrow = 2)
save_plot(here("reports", "figures", "trap_plots.png"), trap_habitats, base_width = 10, base_height = 8)
