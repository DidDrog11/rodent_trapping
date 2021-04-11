library("here")
source(here("scripts", "project_library.R"))
source(here("scripts", "DdM_to_decimal_degrees.R"))
source(here("scripts", "download_data.R"))

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
  filter(village == "lalehun")

lalehun_rodents <- location_rodents %>%
  filter(village == "lalehun") %>%
  ggplot() +
  geom_bar(aes(x = trap_night))

lalehun_test <- trap_sites %>%
  mutate(lon_DdM = paste(paste(lon_degree, lon_dec, sep = "_"), "'", sep = ""),
         lat_DdM = paste(paste(lat_degree, lat_dec, sep = "_"), "'", sep = ""),
         lon = dg2dec(var = lon_DdM, Dg = "_", Min = "'"),
         lat = dg2dec(var = lat_DdM, Dg = "_", Min = "'")) 

coordinates(lalehun_test) <- c("lat", "lon")
proj4string(lalehun_test) <- CRS("+proj=longlat +datum=WGS84")  ## for example

res <- spTransform(lalehun_test, CRS("+proj=utm +zone=29 ellps=WGS84"))
res
  
pal <- colorFactor(c("red", "blue"), domain = c(1, 2))

distinct_lal_traps <- lalehun_traps %>%
  distinct(geometry, .keep_all = T)

leaflet(distinct_lal_traps) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(radius = ~ifelse(visit == 2, 3, 2),
                   color = ~pal(visit),
                   stroke = F,
                   fillOpacity = 0.5,
                   popup = paste(distinct_lal_traps$trap_number),
                   popupOptions = popupOptions(closeOnClick = T))  %>%
  addLegend("topright", pal = pal, values = ~visit,
            title = "Study visit")


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




ggsave(plot = traps_lalehun_16, path = here("reports", "figures"), filename = "lalehun_traps.png")

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

lalehun_18 <- get_googlemap(center = c(-11.0803, 8.197533), zoom = 18, maptype = "hybrid", color = "bw")
ggmap(lalehun_18) +
  scale_x_continuous(n.breaks = 8) +
  scale_y_continuous(n.breaks = 8) +
  theme(panel.grid.minor = element_line(colour = "white"),
        panel.ontop=TRUE, panel.background = element_rect(fill = NA))
ggsave(plot = last_plot(), path = here("reports", "figures"), filename = "lalehun_grid.png")
