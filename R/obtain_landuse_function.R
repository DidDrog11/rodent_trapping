obtain_landuse <- function(trap_data = final_cleaned_trap_data$clean_sites, site_data = ODK_sites$site_habitats) {
  
  landuse <- trap_data %>%
    select(date_set, village, visit, grid_number, trap_number, trap_uid, site_use, intensity, crop_type, habitat, proximity, trap_land_type, roof, walls, floor, lon, lat, elevation) %>%
    left_join(., site_data %>%
                rename("visit" = visit_number,
                       "grid_number" = site,
                       "grid_habitat" = habitat) %>%
                mutate(visit = factor(visit),
                       grid_number = factor(grid_number)),
              by = c("village", "visit", "grid_number")) %>%
    mutate(crop = case_when(str_detect(site_use, "corn") ~ "corn",
                            str_detect(site_use, "banana") ~ "banana",
                            str_detect(site_use, "rice") ~ "rice",
                            str_detect(site_use, "vegetables|^Ve") ~ "vegetables",
                            str_detect(site_use, "palm") ~ "palm",
                            str_detect(site_use, "cocoa") ~ "cocoa",
                            str_detect(site_use, "fallow") ~ "fallow",
                            TRUE ~ "none"))
  
}