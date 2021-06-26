clean_odk_sites <- function(type = "trap") {
  not_needed_vars <- c("other_village_name", "consecutive_traps", "meta-instanceID", "submitterID", "SubmitterName", "AttachmentsPresent",
                       "AttachmentsExpected", "Status", "ReviewState", "DeviceID", "Edits")
  
  trap_sites <- read_csv(here("data", paste0("ODK_", type, "_sites.csv"))) %>%
    filter(ReviewState != "rejected" | is.na(ReviewState)) %>%
    mutate(village_name = case_when(!is.na(village_name) ~ village_name,
                                    TRUE ~ as.character(other_village_name)),
           study_site = case_when(study_site == 1 & KEY == "uuid:5bedb731-5e12-49e2-8aef-fca6a1647690" ~ 2, # A trap site was miscoded as 1 instead of 2, this corrects it.
                                  TRUE ~ study_site),
           date_set = as.Date(ymd_hms(SubmissionDate))) %>%
    rename("key" = "KEY") %>%
    select(-any_of(not_needed_vars), -starts_with("site_images")) 
  
  trap_locations <- read_csv(here("data", paste0("ODK_", type, "_locations.csv"))) %>%
    rename("trap_number" = "site_grouping-trap_number",
           "longitude" = "site_grouping-longitude",
           "latitude" = "site_grouping-latitude",
           "elevation" = "site_grouping-elevation",
           "habitat" = "site_grouping-selected_habitat_trap",
           "proximity" = "site_grouping-selected_proximity",
           "trap_land_type" = "site_grouping-selected_land_type",
           "bait_type" = "site_grouping-bait_type",
           "key" = "PARENT_KEY") %>%
    select(-c("KEY", "site_grouping-trap_image"))
  
  full_trap_locations <- full_join(trap_sites, trap_locations, by = "key") %>%
    drop_na(trap_number) %>%
    rename("village" = "village_name",
           "visit" = "visit_number",
           "grid_number" = "study_site",
           "lon_dec" = "longitude",
           "lat_dec" = "latitude") %>%
    group_by(trap_number) %>%
    mutate(trap_number = paste(trap_number, row_number(), sep = "_"),
           trap_number = str_remove(trap_number, "_1"),
           trap_number = case_when(trap_number == "85_2" ~ "95",
                                   trap_number == "184_2" ~ "187",
                                   TRUE ~ trap_number),
           lat_dec = case_when(lat_dec == 5458.0000 ~ 0.5458, # fixing misrecorded coord
                               TRUE ~ lat_dec),
           lon_dec = case_when(lon_dec == 8.85905 ~ 8.5905, # fixing misrecorded coord
                               TRUE ~ lon_dec),
           lon_degree = 11,
           lat_degree = 8,
           lon_DdM = paste(paste(lon_degree, lon_dec, sep = "_"), "'", sep = ""),
           lat_DdM = paste(paste(lat_degree, lat_dec, sep = "_"), "'", sep = ""),
           lon = -1 * dg2dec(var = lon_DdM, Dg = "_", Min = "'"),
           lat = dg2dec(var = lat_DdM, Dg = "_", Min = "'"),
           grid_number = as.character(grid_number),
           trap_land_type = str_replace_all(
             case_when(is.na(proximity) ~ paste(trap_land_type, sep = " "),
                       is.na(trap_land_type) & !is.na(proximity) ~ paste(proximity, sep = " "),
                       TRUE ~ paste(trap_land_type, proximity, sep = "_")),
             " ", ", "),
           habitat = case_when(habitat == "agricultural" ~ habitat_type,
                               TRUE ~ habitat))
  
  trap_nos <- filter(full_trap_locations, grepl("_2", trap_number))
  
  message(cat(
    paste(
    paste0("There are ", nrow(trap_nos), " duplicated trap numbers:"),
    paste0("They are trap numbers: ", knitr::combine_words(trap_nos$trap_number)),
    paste0("The second trap with the same number is currently removed"),
    sep = "\n")))
    
  full_trap_locations <- full_trap_locations %>%
    group_by_all() %>%
    expand(trap_night = 1:4) %>%
    ungroup() %>%
    mutate(trap_uid = paste0(village, "_", visit, "_", trap_night, "_", grid_number, "_", trap_number))
  
  full_trap_locations <- full_trap_locations %>%
    select(village, visit, date_set, trap_night, grid_number, trap_number, bait_type, habitat, trap_land_type, elevation,
           trap_uid, lon, lat) %>%
    filter(!trap_number %in% trap_nos$trap_number) %>%
    mutate(trap_number = as.numeric(trap_number))
  
  return(full_trap_locations)
}
