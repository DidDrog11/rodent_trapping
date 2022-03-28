devtools::install_github("hrbrmstr/waffle")

if (!require("pacman")) install.packages("pacman")
pkgs =
  c("countrycode",
    "cowplot",
    "exactextractr",
    "fs",
    "ggmap",
    "googledrive",
    "here",
    "iNEXT",
    "janitor",
    "leaflet",
    "lubridate",
    "purr",
    "readr",
    "readxl",
    "RhpcBLASctl",
    "ruODK",
    "sf",
    "snakecase",
    "stringr",
    "targets",
    "terra",
    "tidyverse",
    "tmap",
    "tmaptools",
    "waffle"
  )
pacman::p_load(pkgs, character.only = T)

