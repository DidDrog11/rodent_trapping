source(here::here("scripts", "0_project_library.R"))
source(here("scripts", "0_label_raster.R"))

# Landuse -----------------------------------------------------------------

#The if statement below checks if the rasters have been converted to dataframes and correctly labelled.
#If not it will read in the global raster and crop it to the study area before labelling

if(!file.exists(here("data", "satellite", "landuse_sl.rds"))) {
  landuse <- raster(here("data", "satellite", "landuse.tif")) # this is the complete global 2.5Gb tiff
  
  #The following crops and masks the global raster to SL and Eastern SL respectively
  
  SLE_0 <- read_rds(here("data", "spatial", "gadm36_SLE_0_sp.rds"))
  e_prov <- read_rds(here("data", "spatial", "gadm36_SLE_1_sp.rds")) %>% st_as_sf() %>% filter(NAME_1 == "Eastern")
  landuse_sl <- crop(landuse, SLE_0)
  landuse_sl <- mask(landuse_sl, SLE_0)
  write_rds(landuse_sl, here("data", "satellite", "sierra_leone_landuse.rds"))
  landuse_sle <- crop(landuse, e_prov)
  landuse_sle <- mask(landuse_sle, e_prov)
  write_rds(landuse_sle, here("data", "satellite", "east_sierra_leone_landuse.rds"))
  
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
  write_rds(labels_raster, here("data", "satellite", "labels_raster.rds"))
  
  # This binds those values to labels
  landuse_sl <- raster_dataframe %>%
    left_join(., labels_raster %>%
                rename("landuse" = "value"),
              by = "landuse") %>%
    label_raster() %>%
    group_by(group) %>%
    mutate(group_n = n(),
           label = recode(label,
                          "Wetlands - seasonal" = "Wetlands"),
           group = recode(group,
                          "Wetlands - seasonal" = "Wetlands"))
  
  landuse_sle <- raster_dataframe_east %>%
    left_join(., labels_raster %>%
                rename("landuse" = "value"),
              by = "landuse") %>%
    label_raster() %>%
    group_by(group) %>%
    mutate(group_n = n(),
           label = recode(label,
                          "Wetlands - seasonal" = "Wetlands"),
           group = recode(group,
                          "Wetlands - seasonal" = "Wetlands"))
  
  # This section supports the labelling and colours of the categories
  ras_landuse <- landuse_sl %>%
    ungroup() %>%
    distinct(label, landuse) %>%
    arrange(landuse)
  
  ras_landuse_e <- landuse_sle %>%
    ungroup() %>%
    distinct(label, landuse) %>%
    arrange(landuse)
  
  ras_landuse_sl <- setNames(as.numeric(ras_landuse$landuse), ras_landuse$label)
  names(ras_landuse_sl) <- case_when(names(ras_landuse_sl) == "Forest - lowland" ~ "Forest",
                                     TRUE ~ names(ras_landuse_sl))
  ras_landuse_sle <- setNames(as.character(ras_landuse_e$landuse), ras_landuse_e$label)
  
  write_rds(ras_landuse_sl, here("data", "satellite", "ras_landuse_sl.rds"))
  write_rds(ras_landuse_sle, here("data", "satellite", "las_landuse_sle.rds"))
  
  write_rds(landuse_sl, here("data", "satellite", "landuse_sl.rds"))
  write_rds(landuse_sle, here("data", "satellite", "landuse_sle.rds"))
  
} else {
  
  landuse_sle <- read_rds(here("data", "satellite", "landuse_sl.rds"))
  landuse_sle <- read_rds(here("data", "satellite", "landuse_sle.rds"))
  
  ras_landuse_sle <- read_rds(here("data", "satellite", "landuse_sl.rds"))
  ras_landuse_sl <- read_rds( here("data", "satellite", "landuse_sle.rds"))
}

ras_palette_sl <- c("Missing" = "#d9d9d9",
                    "Forest" = "#00441b",
                    "Forest - montane" = "#238b45",
                    "Savanna - Dry" = "#e8e88e", 
                    "Shrubland" =  "#a1d99b",
                    "Shrubland - high altitude" = "#ffffcc",
                    "Grassland" = "#68b85f",
                    "Wetlands" = "#99d8c9",
                    "Marine" =  "#253494",
                    "Arable land" = "#fee391",
                    "Pastureland" = "#fec44f",
                    "Plantations" = "#ec7014",
                    "Rural gardens" = "#662506",
                    "Urban areas" = "#7a0177")

write_rds(ras_palette_sl, here("data", "satellite", "raster_palette_sl.rds"))

ras_palette_sle <- c("#d9d9d9", "#00441b", "#006d2c", "#238b45", "#99d8c9", "#ccece6", "#045a8d", "#a6bddb", "#fee391", "#fec44f", "#ec7014", "#662506", "#7a0177")
names(ras_palette_sle) <- c("Missing", "Forest - lowland", "Forest", "Forest - montane", "Shrubland", "Shrubland - high altitude", "Wetlands",
                            "Arable land", "Pastureland", "Plantations", "Rural Gardens", "Urban Areas")
write_rds(ras_palette_sle, here("data", "satellite", "raster_palette.rds"))

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
  labs(fill = element_blank(),
       title = "Sierra Leone landuse")

all_sl_landuse

all_sle_landuse <- ggplot(landuse_sle %>%
                           drop_na()) +
  geom_bar(aes(x = reorder(label, group_n), y = ..count../sum(..count..)*100, fill = group)) +
  scale_fill_manual(values = plot_palette_e) +
  coord_flip() +
  theme_minimal() +
  xlab(NULL) +
  ylab("Percentage land use") +
  labs(title = "Eastern Sierra Leone landuse",
       fill = element_blank())

write_rds(all_sle_landuse, here("data", "plots", "sle_proportional.rds"))

#ggsave(filename = "sle_proportional.png", plot = all_sle_landuse, path = here("reports", "figures"), dpi = 300)

sl_raster <- landuse_sl %>%
  ungroup() %>%
  dplyr::select(x, y, landuse) %>%
  st_as_sf(coords = c("x", "y")) %>%
  st_rasterize()

st_crs(sl_raster) <- 4326 # mercator version
geo_sl_raster <- st_transform(sl_raster, 2162) # convert to geocentric

ras_landuse_sl
breaks_sl_plot <- c(0, 99, 109, 200, 299, 307, 405, 500, 900, 1401, 1402, 1403, 1404, 1405, 1600)

sl_raster_plot <- tm_shape(sl_raster) +
  tm_raster(col = "landuse",
            breaks = breaks_sl_plot,
            labels = c(names(ras_palette_sl)),
            palette = ras_palette_sl,
            title = "Land use") +
  tm_layout(legend.outside = T)

sl_raster_plot

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

eastern_raster

tmap_save(eastern_raster, here("reports", "figures", "sle_raster.png"))

# Land use around previous cases ------------------------------------------

hist_lassa <- st_intersection(sierra_leone, e_prov) # only keep reported cases from eastern Sierra Leone

hist_lassa <- st_transform(hist_lassa, 2162)
eastern_prov <- st_transform(e_prov, 2162)

geo_sle_raster %<>%
  st_as_stars() %>%
  st_transform(2162)

lassa_circles <- st_buffer(hist_lassa, dist = 2000) # buffer around the points 5km
landuse_lassa <- st_crop(geo_sle_raster, lassa_circles) # crop the landuse raster to the buffered points

landuse_lassa_df <- as.data.frame(landuse_lassa, xy = T) %>%
  left_join(., labels_raster %>%
              rename("landuse" = "value"),
            by = "landuse") %>%
  label_raster() %>%
  group_by(group) %>%
  mutate(group_n = n()) # convert to a dataframe to allow plotting

landuse_lassa <- landuse_lassa_df %>%
  ungroup() %>%
  dplyr::select(x, y, landuse) %>%
  st_as_sf(coords = c("x", "y")) %>%
  st_rasterize() %>%
  st_set_crs(value = st_crs(lassa_circles))

lassa_case_raster <- tm_shape(eastern_prov) +
  tm_polygons(lwd = 0.2) +
  tm_shape(landuse_lassa) +
  tm_raster(col = "landuse",
            breaks = c(0, as.numeric(ras_landuse_sle)[1:12], 1600),
            labels = c("Missing", names(ras_landuse_sle)[1:12]),
            palette = ras_palette_sle,
            title = "Land use") +
  tm_layout(legend.outside = T) # Map showing locations of cases and habitat type in 5km boundaery

sle_landuse <- landuse_lassa_df %>%
  drop_na() %>%
  mutate(plot = "Lassa - 2km") %>%
  bind_rows(landuse_sl %>%
              mutate(plot = "All SL")) %>%
  bind_rows(landuse_sle %>%
              mutate(plot = "Eastern SL")) %>%
  drop_na() %>%
  group_by(label, group, group_n, plot) %>%
  tally %>%
  group_by(plot) %>%
  mutate(waffle = round(n/sum(n)*1000, 0),
         label = recode(label, 
                        "Urban Areas" = "Urban areas")) %>%
  arrange(-group_n, .by_group = T)

ggplot(sle_landuse, aes(fill = label, values = waffle)) +
  geom_waffle(color = "white", make_proportional = TRUE, size = 1.2)  +
  facet_wrap(~ plot, ncol = 1) +
  scale_fill_manual(values = ras_palette_sl) +
  coord_flip() +
  theme_minimal() +
  labs(fill = "Land use",
       caption = "Each rectangle represents ~1% of land area") +
  theme_enhance_waffle()

ggsave(plot = last_plot(), filename = here("reports", "figures", "sl_lassa_landuse.png"))
