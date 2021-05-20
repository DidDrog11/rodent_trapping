source(here::here("scripts", "0_project_library.R"))

lalehun_landuse <- read_rds(here("data", "satellite", "lalehun_landuse.rds"))
seilama_landuse <- read_rds(here("data", "satellite", "seilama_landuse.rds"))
ras_landuse <- read_rds(here("data", "satellite", "raster_landuse.rds"))
traps <- read_csv(here("data", "trap_sites.csv")) %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(value = st_crs(lalehun_landuse))


landuse_lal = st_extract(lalehun_landuse, traps %>%
                       filter(village == "lalehun") %>%
                       distinct(geometry)) %>%
  as_tibble()

landuse_sei = st_extract(seilama_landuse, traps %>%
                           filter(village == "seilama") %>%
                           distinct(geometry)) %>%
  as_tibble()

landuse <- bind_rows(landuse_lal, landuse_sei)

traps <- traps %>%
  left_join(., landuse, by = "geometry") %>%
  mutate(landuse = recode(landuse, !!!ras_landuse[1:12]))
  
