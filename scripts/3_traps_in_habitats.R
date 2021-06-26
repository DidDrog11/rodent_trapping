source(here::here("scripts", "0_project_library.R"))

bambawo_landuse <- read_rds(here("data", "satellite", "bambawo_landuse.rds"))
lalehun_landuse <- read_rds(here("data", "satellite", "lalehun_landuse.rds"))
lambeyama_landuse <- read_rds(here("data", "satellite", "lambeyama_landuse.rds"))
seilama_landuse <- read_rds(here("data", "satellite", "seilama_landuse.rds"))
ras_landuse <- read_rds(here("data", "satellite", "raster_landuse.rds"))
ras_palette <- read_rds(here("data", "satellite", "raster_palette.rds"))

traps <- read_csv(here("data", "trap_sites.csv"))

village_poly <- st_transform(villages, 2162) %>%
  st_buffer(dist = 2000)

bambawo_bbox <- st_bbox(village_poly %>%
                          filter(village == "bambawo"))
lalehun_bbox <- st_bbox(village_poly %>%
                          filter(village == "lalehun"))
lambeyama_bbox <- st_bbox(village_poly %>%
                          filter(village == "lambeyama"))
seilama_bbox <- st_bbox(village_poly %>%
                          filter(village == "seilama"))

bambawo_trap_plot <- plot_trap_on_habitat("bambawo")
lalehun_trap_plot <- plot_trap_on_habitat("lalehun")
#lambeyama_trap_plot <- plot_trap_on_habitat("lambeyama")
seilama_trap_plot <- plot_trap_on_habitat("seilama")

save_plot(here("reports","figures", "sites_in_habitats_c.png"), plot_grid(tmap_grob(bambawo_trap_plot),
                                                                          tmap_grob(lalehun_trap_plot),
                                                                          tmap_grob(seilama_trap_plot),
          nrow = 2,
          labels = c("Bambawo", "Lalehun", "Seilama")))
