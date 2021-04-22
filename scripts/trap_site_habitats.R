source(here::here("scripts", "project_library.R"))
source(here("scripts", "DdM_to_decimal_degrees.R"))
source(here("scripts", "label_raster.R"))

geo_sle_raster <- read_rds(here("data", "satellite", "eastern_province.rds"))
ras_landuse <- read_rds(here("data", "satellite", "raster_landuse.rds"))
ras_palette <- read_rds(here("data", "satellite", "raster_palette.rds"))
labels_raster <- read_rds(here("data", "satellite", "labels_raster.rds"))
plot_palette <- c("#ec7014", "#00441b", "#99d8c9", "#7a0177", "#045a8d")

villages <- tibble(village = c("lalehun", "seilama"),
                   x  = c(lalehun_coords[1],
                          seilama_coords[1]),
                   y = c(lalehun_coords[2],
                         seilama_coords[2])) %>%
  st_as_sf(coords = c("x", "y")) %>%
  st_set_crs(4326)

village_poly <- st_transform(villages, 2162) %>%
  st_buffer(dist = 2000)

lalehun <- village_poly %>%
  filter(village == "lalehun")
lalehun_bbox <- st_bbox(lalehun)

landuse_lalehun <- st_crop(new, lalehun, crop = T) %>%
  st_transform(4326)

landuse_lalehun_df <- as.data.frame(landuse_lalehun, xy = T) %>%
  drop_na(landuse) %>%
  left_join(., labels_raster %>%
              rename("landuse" = "value"),
            by = "landuse") %>%
  label_raster() %>%
  group_by(group) %>%
  mutate(group_n = n()) # convert to a dataframe to allow plotting

lalehun_landuse_plot <- landuse_lalehun_df %>%
  drop_na() %>%
  group_by(label, group, group_n) %>%
  tally %>%
  ungroup() %>%
  mutate(pct = n/sum(n))

ggplot(lalehun_landuse_plot) +
  geom_col(aes(x = reorder(label, group_n), y = pct*100, fill = group)) +
  scale_fill_manual(values = plot_palette) +
  coord_flip() +
  theme_minimal() +
  xlab(NULL) +
  ylab("Percentage land use") +
  labs(fill = "Land use")

lalehun_raster_plot <- tm_shape(landuse_lalehun,
         bbox = lalehun_bbox,
         raster.warp = F) +
  tm_raster(col = "landuse",
            breaks = c(0, as.numeric(ras_landuse)[1:12], 1600),
            labels = c("Missing", names(ras_landuse)[1:12]),
            palette = ras_palette,
            title = "Lalehun Land use",
            drop.levels = T) +
  tm_layout(legend.outside = T) +
  tm_basemap() 

tmap_save(lalehun_raster_plot, here("reports", "figures", "lalehun_habitats.png"))

seilama <- village_poly %>%
  filter(village == "seilama")
seilama_bbox <- st_bbox(seilama)

landuse_seilama <- st_crop(geo_sle_raster, seilama) %>%
  st_transform(4326)

seilama_raster <- tm_shape(landuse_seilama,
                           bbox = seilama_bbox,
                           raster.warp = F) +
  tm_raster(col = "landuse",
            breaks = c(0, as.numeric(ras_landuse)[1:12], 1600),
            labels = c("Missing", names(ras_landuse)[1:12]),
            palette = ras_palette,
            title = "Seilama Land use",
            drop.levels = T) +
  tm_layout(legend.outside = T) +
  tm_basemap()

tmap_save(seilama_raster, here("reports", "figures", "seilama_habitats.png"))

landuse_seilama_df <- as.data.frame(landuse_seilama, xy = T) %>%
  left_join(., labels_raster %>%
              rename("landuse" = "value"),
            by = "landuse") %>%
  label_raster() %>%
  group_by(group) %>%
  mutate(group_n = n()) # convert to a dataframe to allow plotting

seilama_landuse_plot <- landuse_seilama_df %>%
  drop_na() %>%
  group_by(label, group, group_n) %>%
  tally %>%
  ungroup() %>%
  mutate(pct = n/sum(n))

ggplot(seilama_landuse_plot) +
  geom_col(aes(x = reorder(label, group_n), y = pct*100, fill = group)) +
  scale_fill_manual(values = plot_palette) +
  coord_flip() +
  theme_minimal() +
  xlab(NULL) +
  ylab("Percentage land use") +
  labs(fill = "Land use")

combined_landuse <- bind_rows(lalehun_landuse_plot %>%
                                mutate(village = "Lalehun"),
                              seilama_landuse_plot %>%
                                mutate(village = "Seilama")) %>%
  ggplot() +
  geom_col(aes(x = reorder(label, group_n), y = pct*100, fill = group)) +
  scale_fill_manual(values = plot_palette) +
  coord_flip() +
  theme_minimal() +
  xlab(NULL) +
  ylab("Percentage land use") +
  facet_wrap(~ village) +
  labs(fill = "Land use") +
  ggsave(here("reports", "figures", "village_landuse.png"))

# Locations of traps ------------------------------------------------------

trap_sites <- read_csv(here("data", "trap_sites.csv")) 

trap_habitat <- trap_sites %>%
  mutate(lon_DdM = paste(paste(lon_degree, lon_dec, sep = "_"), "'", sep = ""),
         lat_DdM = paste(paste(lat_degree, lat_dec, sep = "_"), "'", sep = ""),
         lon = -1 * dg2dec(var = lon_DdM, Dg = "_", Min = "'"),
         lat = dg2dec(var = lat_DdM, Dg = "_", Min = "'")) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  dplyr::select(village, trap_uid) %>%
  distinct(geometry, .keep_all = T) %>%
  st_transform(2162) %>%
  st_buffer(dist = 100)

trap_lalehun <- trap_habitat %>%
  filter(village == "lalehun")

lalehun_trap_habitat <- st_crop(geo_sle_raster, trap_lalehun)

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
  filter(village == "seilama")

seilama_trap_habitat <- st_crop(geo_sle_raster, trap_seilama)

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
