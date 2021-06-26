if (!require("pacman")) install.packages("pacman")
pkgs =
  c("here",
    "rgeos",
    "ggspatial",
    "rgdal",
    "grid",
    "tidyverse",
    "magrittr",
    "bib2df",
    "googledrive",
    "readxl",
    "countrycode",
    "ggimage",
    "sf",
    "OpenStreetMap",
    "tmap",
    "maptools",
    "raster",
    "ggmap",
    "taxize",
    "distill",
    "magrittr",
    "rvest",
    "tmap",
    "stars",
    "leaflet",
    "leaflet.opacity",
    "vegan",
    "knitr",
    "waffle",
    "cowplot",
    "iNEXT",
    "cubelyr",
    "lubridate"
  )
pacman::p_load(pkgs, character.only = T)

lalehun_coords <- c(-11.0803, 8.197533)
seilama_coords <- c(-11.193628469657279, 8.122285428353395)
lambeyama_coords <- c(-11.1924089940328, 7.852313504629802)
bambawo_coords <- c(-11.130369, 8.009122)


villages <- tibble(village = c("lalehun", "seilama", "lambeyama", "bambawo"),
                   x  = c(lalehun_coords[1],
                          seilama_coords[1],
                          lambeyama_coords[1],
                          bambawo_coords[1]),
                   y = c(lalehun_coords[2],
                         seilama_coords[2],
                         lambeyama_coords[2],
                         bambawo_coords[2])) %>%
  st_as_sf(coords = c("x", "y")) %>%
  st_set_crs(4326)
