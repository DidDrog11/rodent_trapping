lalehun_coords <- c(-11.0803, 8.197533)
seilama_coords <- c(-11.193628469657279, 8.122285428353395)
lambayama_coords <- c(-11.198249, 7.854131)
bambawo_coords <- c(-11.130369, 8.009122)
baiama_coords <- c(-11.268454, 7.83708)

villages <- tibble(village = c("lalehun", "seilama", "lambayama", "bambawo", "baiama"),
                   x  = c(lalehun_coords[1],
                          seilama_coords[1],
                          lambayama_coords[1],
                          bambawo_coords[1],
                          baiama_coords[1]),
                   y = c(lalehun_coords[2],
                         seilama_coords[2],
                         lambayama_coords[2],
                         bambawo_coords[2],
                         baiama_coords[2])) %>%
  st_as_sf(coords = c("x", "y")) %>%
  st_set_crs(4326)

village_bbox <- list("lalehun" = read_rds(here("data", "spatial", "lal_bbox.rds")),
                     "seilama" = read_rds(here("data", "spatial", "sei_bbox.rds")),
                     "lambayama" = read_rds(here("data", "spatial", "lam_bbox.rds")),
                     "bambawo" = read_rds(here("data", "spatial", "bam_bbox.rds")),
                     "baiama" = read_rds(here("data", "spatial", "bai_bbox.rds")))

ras_palette_sl <- c("Missing" = "#d9d9d9",
                    "Forest" = "#00441b",
                    "Forest - montane" = "#238b45",
                    "Savanna - Dry" = "#e8e88e", 
                    "Shrubland" =  "#ffff8c",
                    "Shrubland - high altitude" = "#ffffcc",
                    "Grassland" = "#68b85f",
                    "Wetlands" = "#99d8c9",
                    "Marine" =  "#253494",
                    "Arable land" = "#fee391",
                    "Pastureland" = "#fec44f",
                    "Plantations" = "#ec7014",
                    "Rural gardens" = "#662506",
                    "Urban areas" = "#7a0177")

village_palette <- c("#7a0177", "#fec44f", "#ec7014", "#005120", "#253494")
names(village_palette) <-  c("Lalehun", "Seilama", "Bambawo", "Lambayama", "Baiama")
