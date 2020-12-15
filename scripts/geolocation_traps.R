library("tidyverse")
library("here")
library("rgdal")
library("sf")
library("ggmap")
library("ggspatial")
library("OpenStreetMap")
library("googledrive")
source(here("scripts", "DdM_to_decimal_degrees.R"))

google_api <- rstudioapi::askForSecret("Google API Key")
register_google(google_api)
drive_download("https://drive.google.com/file/d/1kxpH6RvWgAMwhpqZ4yoH_6SYso06uuDa/view?usp=sharing", path = here("data", "trap_sites_all.xlsx"), overwrite = T)

readxl::read_xlsx(path = here("data", "trap_sites_all.xlsx"), sheet = 2) %>%
  write_csv(here("data", "trap_sites.csv")) #Read the data file from excel document and save within the repo as csv
trapped_rodents <- readxl::read_xlsx(path = here("data", "trap_sites_all.xlsx"), sheet = 3) %>%
  write_csv(here("data", "rodents_trapped.csv"))
rodent_ids <- readxl::read_xlsx(path = here("data", "trap_sites_all.xlsx"), sheet = 4) %>%
  write_csv(here("data", "rodent_ids.csv"))

trap_sites <- read_csv(here("data", "trap_sites.csv"))
location_rodents <- trapped_rodents %>%
  select(rodent_id, trap_night, trap_id, initial_species_id) %>%
  left_join(., trap_sites, 
            by = c("rodent_id", "trap_night")) %>%
  select(rodent_id, trap_night, initial_species_id, village, habitat)

# lalehun -----------------------------------------------------------------

lalehun_traps <- trap_sites %>%
  mutate(lon_DdM = paste(paste(lon_degree, lon_dec, sep = "_"), "'", sep = ""),
         lat_DdM = paste(paste(lat_degree, lat_dec, sep = "_"), "'", sep = ""),
         lon = -1 * dg2dec(var = lon_DdM, Dg = "_", Min = "'"),
         lat = dg2dec(var = lat_DdM, Dg = "_", Min = "'")) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  filter(village == "lalehun",
         empty_morning != "na") %>%
  drop_na(empty_morning)

lalehun_rodents <- location_rodents %>%
  filter(village == "lalehun") %>%
  ggplot() +
  geom_bar(aes(x = trap_night))
  
# seilama -----------------------------------------------------------------

seilama_traps <- trap_sites %>%
  mutate(lon_DdM = paste(paste(lon_degree, lon_dec, sep = "_"), "'", sep = ""),
         lat_DdM = paste(paste(lat_degree, lat_dec, sep = "_"), "'", sep = ""),
         lon = -1 * dg2dec(var = lon_DdM, Dg = "_", Min = "'"),
         lat = dg2dec(var = lat_DdM, Dg = "_", Min = "'")) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  filter(village == "seilama",
         empty_morning != "na") %>%
  drop_na(empty_morning)

# lalehun ggmap -------------------------------------------------------------------

lalehun_17 <- get_googlemap(center = c(-11.0803, 8.197533), zoom = 17, maptype = "hybrid")
lalehun_16 <- get_googlemap(center = c(-11.0803, 8.197533), zoom = 16, maptype = "hybrid")

traps_lalehun_17 <- ggmap(lalehun_17) +
  geom_sf(data = lalehun_traps,
          aes(geometry = geometry,
              colour = rodent_trapped,
              alpha = 0.2),
          inherit.aes = F) +
  labs(title = "Trap locations Lalehun near Panguma",
       color = "Trap success",
       alpha = NULL,
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  guides(alpha = F)

traps_lalehun_16 <- ggmap(lalehun_16) +
  geom_sf(data = lalehun_traps,
          aes(geometry = geometry,
              colour = rodent_trapped,
              alpha = 0.2),
          inherit.aes = F) +
  coord_sf(xlim = c(-11.083, -11.077), ylim = c(8.193, 8.202)) +
  scale_colour_manual(values = c("orange", "purple")) +
  labs(title = "Trap locations Lalehun near Panguma",
       color = "Trap success",
       alpha = NULL,
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  guides(alpha = F)

# seilama ggmap -------------------------------------------------------------------

seilama_17 <- get_googlemap(center = c(-11.1932, 8.122), zoom = 17, maptype = "hybrid")
seilama_16 <- get_googlemap(center = c(-11.1932, 8.122), zoom = 16, maptype = "hybrid")

traps_seilama_17 <- ggmap(seilama_17) +
  geom_sf(data = seilama_traps,
          aes(geometry = geometry,
              colour = rodent_trapped,
              alpha = 0.2),
          inherit.aes = F) +
  labs(title = "Trap locations Seilama near Panguma",
       color = "Trap success",
       alpha = NULL,
       x = "Longitude",
       y = "Latitude") +
  scale_colour_manual(values = c("yellow", "purple")) +
  theme_minimal() +
  guides(alpha = F)

traps_seilama_16 <- ggmap(seilama_16) +
  geom_sf(data = seilama_traps,
          aes(geometry = geometry,
              colour = rodent_trapped,
              alpha = 0.2),
          inherit.aes = F) +
  coord_sf(xlim = c(-11.199, -11.19), ylim = c(8.119, 8.125)) +
  scale_colour_manual(values = c("orange", "purple")) +
  labs(title = "Trap locations Seilama near Panguma",
       color = "Trap success",
       alpha = NULL,
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  guides(alpha = F)

# lalehun OSM ---------------------------------------------------------------------

lalehun_1_ul <- c(8.197, -11.08)
lalehun_1_lr <- c(8.195, -11.078)
lalehun_osm_bing_1 <- openmap(lalehun_1_ul, lalehun_1_lr, type = c("bing"))
lalehun_osm_1 <- openproj(lalehun_osm_bing_1, projection = "+proj=longlat") 

autoplot(lalehun_osm_1) +
  geom_sf(data = test,
          aes(geometry = geometry,
              colour = rodent_trapped,
              alpha = 0.2),
          inherit.aes = F) +
  coord_sf(crs = st_crs(4326))
