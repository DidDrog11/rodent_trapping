generate_raster <- function() {
  
  if(!file.exists(here("data", "satellite", "eastern_province_landuse.rds"))){
    
    landuse <- rast(here("data", "satellite", "landuse.tif"))
    
    eastern_province <- raster::getData(name = "GADM", country = "SLE", level = 1,
                                        path = here("data", "spatial")) %>%
      st_as_sf() %>%
      filter(NAME_1 == "Eastern")
    
    eastern_province_landuse <- crop(landuse, eastern_province)
    
    write_rds(eastern_province_landuse, here("data", "satellite", "eastern_province_landuse.rds"))
    
    df_eastern_province <- as.data.frame(eastern_province_landuse, xy = T) %>%
      rename("sle_landuse" = "layer")
    
    # These are the associated category labels for the raster RGB values
    labels_raster <- xml2::read_xml(here("data", "satellite", "lvl2_style.qml")) %>%
      rvest::html_nodes('paletteEntry') %>%
      {data.frame(value = rvest::html_attr(.,'value'),
                  label = rvest::html_attr(.,'label'))} %>%
      dplyr::mutate(value = readr::parse_number(as.character(value)))
    write_rds(labels_raster, here("data", "satellite", "labels_raster.rds"))
    
    eastern_province_landuse <- df_eastern_province %>%
      mutate(sle_landuse = case_when(sle_landuse == 106 ~ 108,
                                     sle_landuse == 500 ~ 502,
                                     TRUE ~ sle_landuse)) %>%
      left_join(., labels_raster %>%
                  rename("sle_landuse" = "value"),
                by = "sle_landuse") %>%
      label_raster() %>%
      group_by(group) %>%
      mutate(group_n = n(),
             label = recode(label,
                            "Wetlands - seasonal" = "Wetlands"),
             group = recode(group,
                            "Wetlands - seasonal" = "Wetlands")) %>%
      ungroup()
    
    ras_landuse_e <- eastern_province_landuse %>%
      ungroup() %>%
      distinct(label, sle_landuse) %>%
      arrange(sle_landuse) %>%
      left_join(., eastern_province_landuse %>%
                  select(sle_landuse, group) %>%
                  distinct(),
                by = "sle_landuse") %>%
      mutate(palette = recode(str_to_sentence(label), !!!ras_palette_sl)) %>%
      drop_na()
    
    eastern_province_landuse_raster <- eastern_province_landuse %>%
      dplyr::select(-c("group_n", "label", "group")) %>%
      stars::st_as_stars(., coords = c("x", "y")) %>%
      as(., "Raster")
    
    write_rds(eastern_province_landuse_raster, here("data", "satellite", "eastern_province_landuse.rds"))
    write_rds(ras_landuse_e, here("data", "satellite", "eastern_province_landuse_labels.rds"))
    
  }
  
  eastern_province_landuse_raster <- read_rds(here("data", "satellite", "eastern_province_landuse.rds")) 
  crs(eastern_province_landuse_raster) <- project_crs
  ras_landuse_e <- read_rds(here("data", "satellite", "eastern_province_landuse_labels.rds"))
  
  return(list("raster" = eastern_province_landuse_raster,
              "raster_labels" = ras_landuse_e))
}