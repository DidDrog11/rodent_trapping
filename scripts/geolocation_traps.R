library("here")
source(here("scripts", "project_library.R"))
source(here("scripts", "DdM_to_decimal_degrees.R"))

trap_sites <- read_csv(here("data", "trap_sites.csv"))
trapped_rodents <- read_csv(here("data", "rodents_trapped.csv"))
location_rodents <- trapped_rodents %>%
  dplyr::select(rodent_id, trap_night, trap_id, initial_species_id) %>%
  left_join(., trap_sites, 
            by = c("rodent_id", "trap_night")) %>%
  dplyr::select(rodent_id, trap_night, initial_species_id, village, habitat)

# lalehun -----------------------------------------------------------------

lalehun_traps <- trap_sites %>%
  mutate(lon_DdM = paste(paste(lon_degree, lon_dec, sep = "_"), "'", sep = ""),
         lat_DdM = paste(paste(lat_degree, lat_dec, sep = "_"), "'", sep = ""),
         lon = -1 * dg2dec(var = lon_DdM, Dg = "_", Min = "'"),
         lat = dg2dec(var = lat_DdM, Dg = "_", Min = "'")) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  filter(village == "lalehun",
         visit == 1) %>%
  mutate(rodent_trapped = recode(rodent_trapped,
                                 "y" = "Yes",
                                 "n" = "No"))

lalehun_rodents <- location_rodents %>%
  filter(village == "lalehun") %>%
  ggplot() +
  geom_bar(aes(x = trap_night))

pal <- colorFactor(palette = c("#f1a340", "#998ec3"), domain = lalehun_traps$rodent_trapped)

distinct_lal_traps <- lalehun_traps %>%
  distinct(grid_number, trap_number, rodent_id, .keep_all = T)

leaflet(distinct_lal_traps) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircleMarkers(color = ~pal(rodent_trapped),
                   radius = 3,
                   stroke = F,
                   fillOpacity = 1,
                   popup = paste(distinct_lal_traps$trap_number),
                   popupOptions = popupOptions(closeOnClick = T))  %>%
  addLegend("topright", pal = pal, values = ~rodent_trapped,
            title = "Successful capture") %>%
  mapshot(file = here("reports", "figures", "lalehun_traps.png"), remove_controls = c("zoomControl"))


# seilama -----------------------------------------------------------------

seilama_traps <- trap_sites %>%
  mutate(lon_DdM = paste(paste(lon_degree, lon_dec, sep = "_"), "'", sep = ""),
         lat_DdM = paste(paste(lat_degree, lat_dec, sep = "_"), "'", sep = ""),
         lon = -1 * dg2dec(var = lon_DdM, Dg = "_", Min = "'"),
         lat = dg2dec(var = lat_DdM, Dg = "_", Min = "'")) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  filter(village == "seilama",
         visit == 1) %>%
  mutate(rodent_trapped = recode(rodent_trapped,
                                 "y" = "Yes",
                                 "n" = "No"))

seilama_rodents <- location_rodents %>%
  filter(village == "seilama") %>%
  ggplot() +
  geom_bar(aes(x = trap_night))

pal <- colorFactor(palette = c("#f1a340", "#998ec3"), domain = seilama_traps$rodent_trapped)

distinct_sei_traps <- seilama_traps %>%
  distinct(grid_number, trap_number, rodent_id, .keep_all = T)

leaflet(distinct_sei_traps %>%
          filter(visit == 1)) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircleMarkers(color = ~pal(rodent_trapped),
                   radius = 3,
                   stroke = F,
                   fillOpacity = 1,
                   popup = paste(distinct_lal_traps$trap_number),
                   popupOptions = popupOptions(closeOnClick = T))  %>%
  addLegend("topright", pal = pal, values = ~rodent_trapped,
            title = "Successful capture") %>%
  mapshot(file = here("reports", "figures", "seilama_traps.png"), remove_controls = c("zoomControl"))
# lalehun ggmap -------------------------------------------------------------------


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
              shape = grid_number,
              alpha = 0.2),
          inherit.aes = F) +
  coord_sf(xlim = c(-11.199, -11.19), ylim = c(8.119, 8.125)) +
  scale_colour_manual(values = c("orange", "purple")) +
  scale_shape_manual(values = c(15, 16, 17, 18, 19, 7, 10)) +
  labs(title = "Trap locations Seilama near Panguma",
       color = "Trap success",
       shape = "Grid number",
       alpha = NULL,
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  guides(alpha = F)

ggsave(plot = traps_seilama_16, path = here("reports", "figures"), filename = "seilama_traps.png")

seilama_19 <- get_googlemap(center = c(-11.193628469657279, 8.122285428353395), zoom = 19, maptype = "hybrid", color = "bw")
ggmap(seilama_19) +
  scale_x_continuous(n.breaks = 8) +
  scale_y_continuous(n.breaks = 8) +
  theme(panel.grid.minor = element_line(colour = "white"),
        panel.ontop=TRUE, panel.background = element_rect(fill = NA))
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

write_rds(lalehun_traps, here("data", "lalehun_traps.rds"))
write_rds(seilama_traps, here("data", "seilama_traps.rds"))
