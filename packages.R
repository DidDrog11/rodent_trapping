devtools::install_github("hrbrmstr/waffle")

if (!require("pacman")) install.packages("pacman")
pkgs =
  c("caret",
    "countrycode",
    "cowplot",
    "exactextractr",
    "fs",
    "ggmap",
    "googledrive",
    "here",
    "htmltools",
    "iNEXT",
    "janitor",
    "leaflet",
    "lubridate",
    "magrittr",
    "purrr",
    "readr",
    "readxl",
    "RhpcBLASctl",
    "rsample",
    "ruODK",
    "sf",
    "slickR",
    "snakecase",
    "stringr",
    "targets",
    "terra",
    "tidyverse",
    "tmap",
    "tmaptools",
    "waffle",
    "widgetframe"
  )
pacman::p_load(pkgs, character.only = T)

