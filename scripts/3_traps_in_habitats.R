source(here::here("scripts", "0_project_library.R"))

lalehun_landuse <- read_rds(here("data", "satellite", "lalehun_landuse.rds"))
seilama_landuse <- read_rds(here("data", "satellite", "seilama_landuse.rds"))
ras_landuse <- read_rds(here("data", "satellite", "raster_landuse.rds"))
ras_palette <- read_rds(here("data", "satellite", "raster_palette.rds"))

traps <- read_csv(here("data", "trap_sites.csv"))

villages <- tibble(village = c("lalehun", "seilama"),
                   x  = c(lalehun_coords[1],
                          seilama_coords[1]),
                   y = c(lalehun_coords[2],
                         seilama_coords[2])) %>%
  st_as_sf(coords = c("x", "y")) %>%
  st_set_crs(4326)

village_poly <- st_transform(villages, 2162) %>%
  st_buffer(dist = 2000)

lalehun_bbox <- st_bbox(village_poly %>%
                          filter(village == "lalehun"))
seilama_bbox <- st_bbox(village_poly %>%
                          filter(village == "seilama"))

lalehun_traps <- traps %>%
  filter(village == "lalehun") %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  distinct(visit, grid_number, trap_number, rodent_id, geometry)

lalehun_trap_locations <- tm_shape(lalehun_landuse,
           bbox = lalehun_bbox,
           raster.warp = F) +
  tm_raster(col = "landuse",
            breaks = c(0, as.numeric(ras_landuse)[1:12], 1600),
            labels = c("Missing", names(ras_landuse)[1:12]),
            palette = ras_palette,
            legend.show = F) +
  tm_shape(lalehun_traps) +
  tm_dots(col = "white") +
  tm_layout(legend.outside = T) +
  tmap_save(filename = here("reports", "figures", "lalehun_trap_habitats.png"))


seilama_traps <- traps %>%
  filter(village == "seilama") %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  distinct(visit, grid_number, trap_number, rodent_id, geometry)

seilama_trap_locations <- tm_shape(seilama_landuse,
         bbox = seilama_bbox,
         raster.warp = F) +
  tm_raster(col = "landuse",
            breaks = c(0, as.numeric(ras_landuse)[1:12], 1600),
            labels = c("Missing", names(ras_landuse)[1:12]),
            palette = ras_palette,
            legend.show = F) +
  tm_shape(seilama_traps) +
  tm_dots(col = "white") +
  tmap_save(filename = here("reports", "figures", "lalehun_trap_habitats.png"))

save_plot(here("reports","figures", "sites_in_habitats_c.png"), plot_grid(tmap_grob(lalehun_trap_locations), tmap_grob(seilama_trap_locations),
          nrow = 1,
          labels = c("Lalehun", "Seilama")))
