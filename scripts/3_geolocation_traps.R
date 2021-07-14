source(here::here("scripts", "0_project_library.R"))

latest_rodent <- latest_data("rodents")
latest_trapsite <- latest_data("trap_sites")

trap_sites <- latest_trapsite %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  mutate(rodent_trapped = case_when(!is.na(rodent_id) ~ "Yes",
                                    TRUE ~ "No"))

location_rodents <- latest_rodent %>%
  dplyr::select(rodent_id, trap_night, trap_uid, initial_species_id) %>%
  left_join(., trap_sites, 
            by = c("rodent_id", "trap_night")) %>%
  dplyr::select(rodent_id, visit, grid_number, trap_night, initial_species_id, village, habitat) %>%
  mutate(across(.cols = everything(), .fns = factor))

site_palette <- c("#e41a1c", "#377eb8", "#4daf4a",
                  "#984ea3", "#ff7f00", "#ffff33",
                  "#a65628")

# lalehun -----------------------------------------------------------------

lalehun_traps <- trap_sites  %>%
  filter(village == "lalehun")

pal <- colorFactor(palette = c("#f1a340", "#998ec3"), domain = lalehun_traps$rodent_trapped)

location_rodents %>%
  filter(village == "lalehun") %>%
  ggplot() +
  geom_bar(aes(x = trap_night, fill = grid_number)) +
  facet_grid(~ visit) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_fill_manual(values = site_palette) +
  labs(title = "Lalehun",
       x = "Trap night",
       y = "Number of captures",
       fill = "Grid") +
  theme_minimal()

distinct_lal_traps <- lalehun_traps %>%
  distinct(grid_number, trap_number, rodent_id, .keep_all = T)

leaflet(distinct_lal_traps) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircleMarkers(color = ~pal(rodent_trapped),
                   radius = 3,
                   stroke = F,
                   fillOpacity = 1,
                   popup = paste(distinct_lal_traps$trap_uid),
                   popupOptions = popupOptions(closeOnClick = T))  %>%
  addLegend("topright", pal = pal, values = ~rodent_trapped,
            title = "Successful capture")

# mapshot(file = here("reports", "figures", "lalehun_traps.png"), remove_controls = c("zoomControl"))

# seilama -----------------------------------------------------------------

seilama_traps <- trap_sites %>%
  filter(village == "seilama")

location_rodents %>%
  filter(village == "seilama") %>%
  ggplot() +
  geom_bar(aes(x = trap_night, fill = grid_number)) +
  facet_grid(~ visit) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_fill_manual(values = site_palette) +
  labs(title = "Seilama",
       x = "Trap night",
       y = "Number of captures",
       fill = "Grid") +
  theme_minimal()

distinct_sei_traps <- seilama_traps %>%
  distinct(grid_number, trap_number, geometry, rodent_id, .keep_all = T)

leaflet(distinct_sei_traps) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircleMarkers(color = ~pal(rodent_trapped),
                   radius = 3,
                   stroke = F,
                   fillOpacity = 1,
                   popup = paste(distinct_sei_traps$trap_number),
                   popupOptions = popupOptions(closeOnClick = T))  %>%
  addLegend("topright", pal = pal, values = ~rodent_trapped,
            title = "Successful capture")

# bambawo -----------------------------------------------------------------

bambawo_traps <- trap_sites %>%
  filter(village == "bambawo")

location_rodents %>%
  filter(village == "bambawo") %>%
  ggplot() +
  geom_bar(aes(x = trap_night, fill = grid_number)) +
  facet_grid(~ visit) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_fill_manual(values = site_palette) +
  labs(title = "Bambawo",
       x = "Trap night",
       y = "Number of captures",
       fill = "Grid") +
  theme_minimal()

distinct_bambawo_traps <- bambawo_traps %>%
  distinct(grid_number, trap_number, geometry, rodent_id, .keep_all = T)

leaflet(distinct_bambawo_traps) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircleMarkers(color = ~pal(rodent_trapped),
                   radius = 3,
                   stroke = F,
                   fillOpacity = 1,
                   popup = paste(distinct_bambawo_traps$trap_number),
                   popupOptions = popupOptions(closeOnClick = T))  %>%
  addLegend("topright", pal = pal, values = ~rodent_trapped,
            title = "Successful capture")

# mapshot(file = here("reports", "figures", "bambawo_traps.png"), remove_controls = c("zoomControl"))

# lambayama -----------------------------------------------------------------

lambayama_traps <- trap_sites %>%
  filter(village == "lambayama")

location_rodents %>%
  filter(village == "lambayama") %>%
  ggplot() +
  geom_bar(aes(x = trap_night, fill = grid_number)) +
  facet_grid(~ visit) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_fill_manual(values = site_palette) +
  labs(title = "Lambayama",
       x = "Trap night",
       y = "Number of captures",
       fill = "Grid") +
  theme_minimal()

distinct_lambayama_traps <- lambayama_traps %>%
  distinct(grid_number, trap_number, geometry, rodent_id, .keep_all = T)

leaflet(distinct_lambayama_traps) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircleMarkers(color = ~pal(rodent_trapped),
                   radius = 3,
                   stroke = F,
                   fillOpacity = 1,
                   popup = paste(distinct_lambayama_traps$trap_number),
                   popupOptions = popupOptions(closeOnClick = T))  %>%
  addLegend("topright", pal = pal, values = ~rodent_trapped,
            title = "Successful capture")

#mapshot(file = here("reports", "figures", "lambayama_traps.png"), remove_controls = c("zoomControl"))

# baiama -----------------------------------------------------------------

baiama_traps <- trap_sites  %>%
  filter(village == "baiama")

location_rodents %>%
  filter(village == "baiama") %>%
  ggplot() +
  geom_bar(aes(x = trap_night, fill = grid_number)) +
  facet_grid(~ visit) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_fill_manual(values = site_palette) +
  labs(title = "baiama",
       x = "Trap night",
       y = "Number of captures",
       fill = "Grid") +
  theme_minimal()

distinct_baiama_traps <- baiama_traps %>%
  distinct(grid_number, trap_number, rodent_id, .keep_all = T)

leaflet(distinct_baiama_traps) %>%
  addTiles() %>%
  addCircleMarkers(color = ~pal(rodent_trapped),
                   radius = 3,
                   stroke = F,
                   fillOpacity = 1,
                   popup = paste(distinct_lal_traps$trap_number),
                   popupOptions = popupOptions(closeOnClick = T))  %>%
  addLegend("topright", pal = pal, values = ~rodent_trapped,
            title = "Successful capture")

# mapshot(file = here("reports", "figures", "baiama_traps.png"), remove_controls = c("zoomControl"))

write_rds(lalehun_traps, here("data", "clean_data", "site_specific", "lalehun_traps.rds"))
write_rds(seilama_traps, here("data", "clean_data", "site_specific", "seilama_traps.rds"))
write_rds(bambawo_traps, here("data", "clean_data", "site_specific", "bambawo_traps.rds"))
write_rds(lambayama_traps, here("data", "clean_data", "site_specific", "lambayama_traps.rds"))
write_rds(baiama_traps, here("data", "clean_data", "site_specific", "baiama_traps.rds"))
