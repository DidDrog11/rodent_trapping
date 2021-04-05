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
    "mapview",
    "ggmap",
    "taxize",
    "distill",
    "magrittr",
    "rvest"
  )
pacman::p_load(pkgs, character.only = T)
