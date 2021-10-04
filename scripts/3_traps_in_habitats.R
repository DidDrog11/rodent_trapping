source(here::here("scripts", "0_project_library.R"))

bambawo_landuse <- read_rds(here("data", "satellite", "bambawo_landuse.rds"))
baiama_landuse <- read_rds(here("data", "satellite", "baiama_landuse.rds"))
lalehun_landuse <- read_rds(here("data", "satellite", "lalehun_landuse.rds"))
lambayama_landuse <- read_rds(here("data", "satellite", "lambayama_landuse.rds"))
seilama_landuse <- read_rds(here("data", "satellite", "seilama_landuse.rds"))
ras_landuse <- read_rds(here("data", "satellite", "raster_landuse.rds"))
ras_palette <- read_rds(here("data", "satellite", "raster_palette.rds"))

traps <- latest_data("trap_sites")

village_poly <- st_transform(villages, 2162) %>%
  st_buffer(dist = 2000)

bambawo_bbox <- st_bbox(village_poly %>%
                          filter(village == "bambawo"))
lalehun_bbox <- st_bbox(village_poly %>%
                          filter(village == "lalehun"))
lambayama_bbox <- st_bbox(village_poly %>%
                          filter(village == "lambayama"))
seilama_bbox <- st_bbox(village_poly %>%
                          filter(village == "seilama"))
baiama_bbox <- st_bbox(village_poly %>%
                         filter(village == "baiama"))


bambawo_trap_plot <- plot_trap_on_habitat("bambawo")
lalehun_trap_plot <- plot_trap_on_habitat("lalehun")
lambayama_trap_plot <- plot_trap_on_habitat("lambayama")
seilama_trap_plot <- plot_trap_on_habitat("seilama")
baiama_trap_plot <- plot_trap_on_habitat("baiama")

save_plot(here("reports","figures", "sites_in_habitats_c.png"), plot_grid(tmap_grob(bambawo_trap_plot),
                                                                          tmap_grob(lalehun_trap_plot),
                                                                          tmap_grob(seilama_trap_plot),
                                                                          tmap_grob(lambayama_trap_plot),
                                                                          tmap_grob(baiama_trap_plot),
          nrow = 3,
          labels = c("Bambawo", "Lalehun", "Seilama", "Lambayama", "Baiama"),
          label_size = 8))
