if (!require("pacman")) install.packages("pacman")
pkgs =
  c("here",
    "tidyverse",
    "magrittr",
    "bib2df",
    "knitr",
    "sf"
  )
pacman::p_load(pkgs, character.only = T)

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

village_palette <- c("#7a0177", "#fec44f", "#ec7014", "#005120", "#253494")
names(village_palette) <-  c("Lalehun", "Seilama", "Bambawo", "Lambayama", "Baiama")
trap_palette <- c("#7a0177","#fee391", "#fec44f",  "#00441b")
names(trap_palette) <- c("Village", "Distal agriculture", "Proximal agriculture", "Forest/fallow land")

factor_vars <- c("village", "visit", "trap_night", "grid_number", "line_number", "trap_number",
                 "bait_type", "trap_uid", "empty_morning", "bait_present", "trap_sprung", "rodent_trapped",
                 "rodent_id", "weather", "initial_species_id", "group", "sex", "testes", "seminal_vesicles",
                 "vagina_perforate", "teats_visible", "photos_taken", "visit", "all_samples", "cut_tail", "genus",
                 "habitat_group")

sapply(list.files(path = here("scripts"), pattern = "function", full.names = T), source)