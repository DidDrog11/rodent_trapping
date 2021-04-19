source(here::here("scripts", "project_library.R"))
source(here("scripts", "label_raster.R"))

SLE_0 <- getData("GADM", country = "SL", level = 0, path = here("data", "spatial")) %>%
  st_as_sf()
SLE_1 <- getData("GADM", country = "SL", level = 1, path = here("data", "spatial")) %>%
  st_as_sf()
SLE_2 <- getData("GADM", country = "SL", level = 2, path = here("data", "spatial")) %>%
  st_as_sf()


# Lassa cases -------------------------------------------------------------

data <- read_csv(here("data", "lassa", "lassa_seroprevalence.csv")) %>%
  mutate(Rodent_or_human = ifelse(is.na(Rodent_or_human), "Missing", Rodent_or_human),
         Rodent_or_human = case_when(Rodent_or_human == "human" ~ "Human",
                                     Rodent_or_human == "Rodents" ~ "Rodent",
                                     TRUE ~ Rodent_or_human),
         Rodent_or_human = factor(Rodent_or_human))

sierra_leone <- data %>%
  filter(Country == "Sierra Leone") %>%
  drop_na(c("Longitude", "Latitude", "Number_acutecases")) %>%
  mutate(Number_acutecases = as.numeric(Number_acutecases)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

villages <- tibble(village = c("lalehun", "seilama"),
                   x  = c(lalehun_coords[1],
                          seilama_coords[1]),
                   y = c(lalehun_coords[2],
                         seilama_coords[2])) %>%
  st_as_sf(coords = c("x", "y")) %>%
  st_set_crs(4326)

sle_osm <- tmaptools::read_osm(SLE_0, ext = 1.1)

provinces_lassa <- st_join(SLE_2, sierra_leone, left = F)

lassa_sl <- tm_shape(sle_osm) +
  tm_rgb() +
  tm_shape(sierra_leone) +
  tm_bubbles(size = 0.2,
             title.size = "Lassa infections",
             legend.size.is.portrait = T,
             col = "Year_end",
             title.col = "Year",
             jitter = 0.2,
             alpha = 0.7) +
  tm_legend(legend.format = list(fun=function(x) formatC(x, digits=0, format="d")),
            legend.outside = F)
  
tmap_save(lassa_sl, here("reports", "figures", "lassa_SL.png"))

e_prov <-  SLE_2 %>%
  filter(NAME_1 == "Eastern" & GID_2 != "SLE.1.3_1")

eprov_lassa <- st_intersection(sierra_leone, e_prov)
eprov_osm <- tmaptools::read_osm(st_bbox(eprov_lassa), ext = 1.1, type = c("osm"))

lassa_e_prov <- tm_shape(eprov_osm) +
  tm_rgb() +
  tm_shape(eprov_lassa) +
  tm_bubbles(size = 0.2,
             col = "Year_end",
             title.col = "Year",
             shape = "Rodent_or_human",
             title.shape = "Species",
             jitter = 0.01,
             alpha = 0.7) +
  tm_shape(villages) +
  tm_bubbles(size = 0.2,
             col = "black") +
  tm_legend(legend.format = list(fun=function(x) formatC(x, digits=0, format="d")),
            legend.outside = F,
            legend.position = c("right", "bottom"))

tmap_save(lassa_e_prov, here("reports", "figures", "lassa_panguma.png"))

# Landuse -----------------------------------------------------------------

# landuse <- raster(here("data", "satellite", "landuse.tif")) # this is the complete global 2.5Gb tiff

# The following crops and masks the global raster to SL and Eastern SL respectively
# landuse_sl <- crop(landuse, SLE_0)
# landuse_sl <- mask(landuse_sl, SLE_0)
# write_rds(landuse_sl, here("data", "satellite", "sierra_leone_landuse.rds"))
# landuse_sle <- crop(landuse, e_prov)
# landuse_sle <- mask(landuse_sle, e_prov)
# write_rds(landuse_sle, here("data", "satellite", "east_sierra_leone_landuse.rds"))

landuse_sl <- read_rds(here("data", "satellite", "sierra_leone_landuse.rds"))
landuse_sle <- read_rds(here("data", "satellite", "east_sierra_leone_landuse.rds"))

raster_dataframe <- as.data.frame(landuse_sl, xy = T) 
raster_dataframe_east <- as.data.frame(landuse_sle, xy = T) 

# These are the associated category labels for the raster RGB values
labels_raster <- xml2::read_xml(here("data", "satellite", "lvl2_style.qml")) %>%
  rvest::html_nodes('paletteEntry') %>%
  {data.frame(value = rvest::html_attr(.,'value'),
              label = rvest::html_attr(.,'label'))} %>%
  dplyr::mutate(value = readr::parse_number(as.character(value)))

# This binds those values to labels
landuse_sl <- raster_dataframe %>%
  left_join(., labels_raster %>%
              rename("landuse" = "value"),
            by = "landuse") %>%
  label_raster() %>%
  group_by(group) %>%
  mutate(group_n = n())

landuse_sle <- raster_dataframe_east %>%
  left_join(., labels_raster %>%
              rename("landuse" = "value"),
            by = "landuse") %>%
  label_raster() %>%
  group_by(group) %>%
  mutate(group_n = n())

# This section supports the labelling and colours of the categories
ras_landuse <- landuse_sl %>%
  ungroup() %>%
  distinct(label, landuse) %>%
  arrange(landuse)

ras_landuse_e <- landuse_sle %>%
  ungroup() %>%
  distinct(label, landuse) %>%
  arrange(landuse)

ras_landuse_sl <- setNames(as.character(ras_landuse$landuse), ras_landuse$label)
names(ras_landuse_sl) <- case_when(names(ras_landuse_sl) == "Forest - lowland" ~ "Forest",
                                   names(ras_landuse_sl) == "Forest - montane" ~ "Forest",
                                   names(ras_landuse_sl) == "Shrubland - high altitude" ~ "Shrubland",
                                   TRUE ~ names(ras_landuse_sl))
ras_landuse_sle <- setNames(as.character(ras_landuse_e$landuse), ras_landuse_e$label)

ras_palette_sl <- c("#d9d9d9", "#00441b", "#006d2c", "#238b45", "#99d8c9", "#ccece6", "#045a8d", "#a6bddb", "#fee391", "#fec44f", "#ec7014", "#662506", "#7a0177")
ras_palette_sle <- c("#d9d9d9", "#00441b", "#006d2c", "#238b45", "#99d8c9", "#ccece6", "#045a8d", "#a6bddb", "#fee391", "#fec44f", "#ec7014", "#662506", "#7a0177")

plot_palette_a <- c("#ec7014", "#00441b", "#a6d96a", "#2166ac", "#fee08b", "#99d8c9", "#7a0177", "#92c5de")
plot_palette_e <- c("#ec7014", "#00441b", "#99d8c9", "#7a0177", "#045a8d")

all_sl_landuse <- ggplot(landuse_sl %>%
         drop_na()) +
  geom_bar(aes(x = reorder(label, group_n), y = ..count../sum(..count..)*100, fill = group)) +
  scale_fill_manual(values = plot_palette_a) +
  coord_flip() +
  theme_minimal() +
  xlab(NULL) +
  ylab("Percentage land use") +
  labs(fill = "Land use")

all_sle_landuse <- ggplot(landuse_sle %>%
                           drop_na()) +
  geom_bar(aes(x = reorder(label, group_n), y = ..count../sum(..count..)*100, fill = group)) +
  scale_fill_manual(values = plot_palette_e) +
  coord_flip() +
  theme_minimal() +
  xlab(NULL) +
  ylab("Percentage land use") +
  labs(fill = "Land use")
ggsave(all_sle_landuse, here("reports", "figures", "sle_proportional.png"))

sl_raster <- landuse_sl %>%
  ungroup() %>%
  dplyr::select(x, y, landuse) %>%
  st_as_sf(coords = c("x", "y")) %>%
  st_rasterize()

st_crs(sl_raster) <- 4326 # mercator version
geo_sl_raster <- st_transform(sl_raster, 2162) # convert to geocentric

sl_raster_plot <- tm_shape(sl_raster) +
  tm_raster(col = "landuse",
            breaks = c(0,100, 109, 201, 307, 407, 515, 1207, 1401, 1402, 1403, 1404, 1405),
            labels = c("Missing", unique(names(ras_landuse_sl))[1:11]),
            palette = ras_palette_sl,
            title = "Land use") +
  tm_layout(legend.outside = T)
tmap_save(sl_raster_plot, here("reports", "figures", "sl_raster.png"))

sle_raster <- landuse_sle %>%
  ungroup() %>%
  dplyr::select(x, y, landuse) %>%
  st_as_sf(coords = c("x", "y")) %>%
  st_rasterize()

st_crs(sle_raster) <- 4326 # mercator version
geo_sle_raster <- st_transform(sle_raster, 2162) # convert to geocentric

eastern_raster <- tm_shape(sle_raster) +
  tm_raster(col = "landuse",
            breaks = c(0, as.numeric(ras_landuse_sle)[1:12], 1600),
            labels = c("Missing", names(ras_landuse_sle)[1:12]),
            palette = ras_palette_sle,
            title = "Land use") +
  tm_layout(legend.outside = T)
tmap_save(eastern_raster, here("reports", "figures", "sle_raster.png"))

tmap_leaflet(eastern_raster)

# Land use around previous cases ------------------------------------------

hist_lassa <- st_intersection(sierra_leone, e_prov) # only keep reported cases from eastern Sierra Leone

hist_lassa <- st_transform(hist_lassa, 2162)
eastern_prov <- st_transform(e_prov, 2162)

lassa_circles <- st_buffer(hist_lassa, dist = 2000) # buffer around the points 5km

landuse_lassa <- st_crop(geo_sle_raster, lassa_circles) # crop the landuse raster to the buffered points

landuse_lassa_df <- as.data.frame(landuse_lassa, xy = T) %>%
  left_join(., labels_raster %>%
              rename("landuse" = "value"),
            by = "landuse") %>%
  label_raster() %>%
  group_by(group) %>%
  mutate(group_n = n()) # convert to a dataframe to allow plotting

sle_landuse <- landuse_lassa_df %>%
  drop_na() %>%
  mutate(plot = "Lassa - 2km") %>%
  bind_rows(landuse_sl %>%
              mutate(plot = "All SL")) %>%
  drop_na() %>%
  group_by(label, group, group_n, plot) %>%
  tally %>%
  group_by(plot) %>%
  mutate(pct = n/sum(n))
  
ggplot(sle_landuse) +
  geom_col(aes(x = reorder(label, group_n), y = pct*100, fill = group)) +
  scale_fill_manual(values = plot_palette) +
  coord_flip() +
  theme_minimal() +
  xlab(NULL) +
  ylab("Percentage land use") +
  facet_wrap(~ plot) +
  labs(fill = "Land use") +
  ggsave(here("reports", "figures", "sl_lassa_landuse.png"))
