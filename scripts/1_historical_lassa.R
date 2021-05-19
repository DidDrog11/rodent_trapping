source(here::here("scripts", "0_project_library.R"))

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

lassa_plot <- tm_shape(sle_osm) +
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

lassa_plot

tmap_save(lassa_plot, here("reports", "figures", "lassa_SL.png"))

e_prov <-  SLE_2 %>%
  filter(GID_2 == "SLE.1.2_1")

eprov_lassa <- st_intersection(sierra_leone %>%
                                 filter(Town != "Joru"),
                               e_prov)
eprov_lassa$Rodent_or_human <- droplevels(eprov_lassa$Rodent_or_human)

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
            legend.position = c("right", "bottom")) +
  tm_scale_bar(position = c("left", "top")) +
  tm_compass(position = c("left", "top"))

lassa_e_prov

tmap_save(lassa_e_prov, here("reports", "figures", "lassa_panguma.png"), height = 15, units = "cm")