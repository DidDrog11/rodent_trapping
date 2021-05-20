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
    "cubelyr"
  )
pacman::p_load(pkgs, character.only = T)

lalehun_coords <- c(-11.0803, 8.197533)
seilama_coords <- c(-11.193628469657279, 8.122285428353395)
