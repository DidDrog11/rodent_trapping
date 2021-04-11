source(here::here("scripts", "project_library.R"))


SLE_0 <- getData("GADM", country = "SL", level = 0, path = here("data", "spatial")) %>%
  st_as_sf()
SLE_1 <- getData("GADM", country = "SL", level = 1, path = here("data", "spatial")) %>%
  st_as_sf()
SLE_2 <- getData("GADM", country = "SL", level = 2, path = here("data", "spatial")) %>%
  st_as_sf()

data <- read_csv(here("data", "lassa", "lassa_seroprevalence.csv")) %>%
  mutate(Rodent_or_human = ifelse(is.na(Rodent_or_human), "Missing", Rodent_or_human),
         Rodent_or_human = factor(Rodent_or_human))

sierra_leone <- data %>%
  filter(Country == "Sierra Leone") %>%
  drop_na(c("Longitude", "Latitude", "Number_acutecases")) %>%
  mutate(Number_acutecases = as.numeric(Number_acutecases)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

provinces_lassa <- st_join(SLE_2, sierra_leone, left = F)

tm_shape(provinces_lassa) +
  tm_polygons() +
  tm_shape(SLE_2) +
  tm_polygons(alpha = 0) +
  tm_shape(sierra_leone) +
  tm_bubbles(size = "Number_acutecases",
             title.size = "Number of infections",
             legend.size.is.portrait = T)

landuse <- raster(here("data", "satellite", "landuse.tif")) 

e_prov <-  SLE_2 %>%
  filter(NAME_1 == "Eastern" & GID_2 != "SLE.1.3_1")

landuse_sle <- crop(landuse, e_prov)
landuse_sle <- mask(landuse_sle, e_prov)

raster_dataframe <- as.data.frame(landuse_sle, xy = T) 

labels_raster <- xml2::read_xml(here("data", "satellite", "lvl2_style.qml")) %>%
  rvest::html_nodes('paletteEntry') %>%
  {data.frame(value = rvest::html_attr(.,'value'),
              label = rvest::html_attr(.,'label'))} %>%
  dplyr::mutate(value = readr::parse_number(as.character(value)))

landuse_sl <- raster_dataframe %>%
  left_join(., labels_raster %>%
              rename("landuse" = "value"),
            by = "landuse") %>%
  mutate(group = recode(label,
                        "Arable land" = "Farmland",
                        "Plantations" = "Farmland",
                        "Plantations" = "Farmland",
                        "Pastureland" = "Farmland",
                        "Rural Gardens" = "Farmland",
                        "Shrubland - Subtropical-tropical high altitude" = "Shrubland",
                        "Shrubland - Subtropical-tropical moist" = "Shrubland",
                        "Wetlands (inland)" = "Wetlands",
                        "Wetlands (inland) - Seasonal/intermittent/irregular rivers/streams/creeks" = "Wetlands",
                        "Forest - Subtropical-tropical moist montane" = "Forest",
                        "Forest - Subtropical-tropical moist lowland" = "Forest",
                        "Forest - Subtropical-tropical swamp" = "Forest",
                        "Urban Areas" = "Urban"),
         label = recode(label, 
                        "Shrubland - Subtropical-tropical high altitude" = "Shrubland - high altitude",
                        "Shrubland - Subtropical-tropical moist" = "Shrubland - moist",
                        "Wetlands (inland)" = "Wetlands",
                        "Wetlands (inland) - Seasonal/intermittent/irregular rivers/streams/creeks" = "Wetlands - seasonal",
                        "Forest - Subtropical-tropical moist montane" = "Forest - moist montane",
                        "Forest - Subtropical-tropical moist lowland" = "Forest - moist lowland",
                        "Forest - Subtropical-tropical swamp" = "Forest - swamp",)) %>%
  group_by(group) %>%
  mutate(group_n = n())


ggplot(landuse_sl %>%
         drop_na()) +
  geom_bar(aes(x = reorder(label, group_n), fill = group)) +
  scale_fill_discrete(type = c("#0C8708", "#5CBF59", "#B9BB4A", "Black", "#364BDF")) +
  coord_flip()

landuse_sl %>%
  st_as_sf(coords = c("x", "y")) %>%
  raster(value = landuse) %>%
  plot()


  