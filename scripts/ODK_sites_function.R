clean_odk_sites <- function(type = "trap") {
  
  all_files <- list.files(here("data", "raw_odk", "trap_sites"), full.names = T)
  
  not_needed_vars <- c("other_village_name", "consecutive_traps", "meta-instanceID", "submitterID", "SubmitterName", "AttachmentsPresent",
                       "AttachmentsExpected", "Status", "ReviewState", "DeviceID", "Edits", "non_village-consecutive_traps", "non_village-starting_number",
                       "non_village-highest", "non_village-ending_number", "non_village-trap_count", "within_houses-consecutive_traps", "swapped_lat", "swapped_lon")
  
  trap_sites <- read_csv(all_files[5], show_col_types = FALSE) %>%
    filter(ReviewState != "rejected" | is.na(ReviewState)) %>%
    mutate(other_village_name = case_when(other_village_name == "Lambeyama" ~ "Lambayama", # Correct the spelling of a village site
                                          TRUE ~ other_village_name),
           village_name = case_when(village_name != "other" ~ village_name,
                                    TRUE ~ tolower(as.character(other_village_name))),
           study_site = case_when(study_site == 1 & KEY == "uuid:5bedb731-5e12-49e2-8aef-fca6a1647690" ~ 2, # A trap site was miscoded as 1 instead of 2, this corrects it.
                                  site_use == "Forest" & village_name == "seilama" & visit_number == 3 & study_site == 4 ~ 5,
                                  TRUE ~ study_site)) %>%
    group_by(village_name, visit_number) %>%
    mutate(date_set = as.Date(ymd_hms(min(form_entry)))) %>%
    rename("crop_type" = "non_village-crop_type_new_site",
           "number_traps" = "non_village-number_of_traps", 
           "number_houses" = "within_houses-number_of_houses",
           "house_count" = "within_houses-house_count",
           "key" = "KEY") %>%
    dplyr::select(-any_of(not_needed_vars), -starts_with("site_images"))
  
  bambawo_trap_locations <- read_csv(all_files[1], show_col_types = FALSE)
  # due to a change in the the ODK form structure this data is read in from a previously saved file
  
  trap_locations <- read_csv(all_files[3], show_col_types = FALSE) %>%
    bind_rows(., bambawo_trap_locations) %>%
    rename("trap_number" = "site_grouping-trap_number",
           "longitude" = "site_grouping-longitude",
           "latitude" = "site_grouping-latitude",
           "elevation" = "site_grouping-elevation",
           "habitat" = "site_grouping-selected_habitat_trap",
           "proximity" = "site_grouping-selected_proximity",
           "trap_land_type" = "site_grouping-selected_land_type",
           "key" = "PARENT_KEY") %>%
    dplyr::select(-c("KEY", "site_grouping-trap_image"))
  
  house_sites <- read_csv(all_files[2], show_col_types = FALSE) %>%
    rename("key" = "PARENT_KEY") %>%
    dplyr::select(-KEY)  %>%
    mutate(latitude = case_when(latitude == 118702.0000 ~ 11.8702,
                                TRUE ~ latitude)) %>%
    rename("number_traps" = "number_of_traps_in_house") 
  
  house_traps <- read_csv(all_files[4], show_col_types = FALSE) %>%
    rename("key" = "PARENT_KEY") %>%
    mutate(key = gsub("\\/within.*", "", key)) %>%
    dplyr::select(-KEY)
  
  indoor_traps <- tibble(t_1 = c(246, 250, 254, 258, 262, 140, 131, 146, 135, 130),
                         t_2 = c(247, 251, 255, 259, 263, 142, 132, 147, 136, 139),
                         t_3 = c(248, 252, 256, 260, 264, 143, 133, 127, 137, 295),
                         t_4 = c(249, 253, 257, 261, 265, 144, 134, 145, 138, 221))
  
  v3_houses <- house_sites %>%
    left_join(., house_traps, by = "key") %>%
    bind_cols(., indoor_traps) %>%
    dplyr::select(-trap_number) %>%
    pivot_longer(cols = c(t_1, t_2, t_3, t_4), values_to = "trap_number") %>%
    dplyr::select(-name)
  
  houses <- house_sites %>%
    left_join(., house_traps, by = "key") %>%
    filter(!key %in% v3_houses$key) %>%
    bind_rows(., v3_houses)
    
  
  full_trap_locations <- full_join(trap_sites, trap_locations, by = "key") %>%
    left_join(., houses, by = "key") %>%
    mutate(longitude = case_when(is.na(longitude.x) ~ longitude.y,
                                 TRUE ~ longitude.x),
           latitude = case_when(is.na(latitude.x) ~ latitude.y,
                                 TRUE ~ latitude.x),
           elevation = case_when(is.na(elevation.x) ~ elevation.y,
                                 TRUE ~ elevation.x),
           trap_number = case_when(is.na(trap_number.x) ~ trap_number.y,
                                 TRUE ~ trap_number.x),
           number_traps = case_when(is.na(number_traps.x) ~ number_traps.y,
                                 TRUE ~ number_traps.x) ) %>%
    dplyr::select(-any_of(ends_with(c(".x", ".y")))) %>%
    drop_na(trap_number) %>%
    rename("village" = "village_name",
           "visit" = "visit_number",
           "grid_number" = "study_site",
           "lon_dec" = "longitude",
           "lat_dec" = "latitude") %>%
    # manage houses
    group_by(village, visit, grid_number) %>%
    mutate(site_use = case_when(habitat_type == "village_inside" ~ "housing",
                                TRUE ~ site_use),
           intensity = case_when(habitat_type == "village_inside" ~ "intense",
                                 TRUE ~ intensity),
           house_count = case_when(!is.na(house_count) ~ row_number(key)),
           habitat = case_when(habitat_type == "village_inside" ~ "houses",
                                 TRUE ~ habitat),
           grid_number = case_when(site_use == "Forest" & village == "seilama" & visit == 3 & grid_number == "4" ~ 5,
                                   site_use == "village_inside" ~ 7,
                                   village == "lalehun" & visit == "3" & as.numeric(trap_number) < 99 & as.numeric(trap_number) >= 50 ~ 3,
                                   TRUE ~ grid_number)) %>%
    ungroup() %>%
    # sort misrecorded gps
    group_by(village, trap_number) %>%
    mutate(trap_number = paste(trap_number, row_number(), sep = "_"),
           trap_number = str_remove(trap_number, "_1"),
           trap_number = case_when(trap_number == "77_2" & visit == "1" & village == "lambayama" ~ "78", # Some trap sites have been misrecorded
                                   trap_number == "85_2" & visit == "1" & village == "bambawo" ~ "95",
                                   trap_number == "184_2" & visit == "1" & village == "bambawo" ~ "187",
                                   trap_number == "146" & visit == "1" & village == "baiama" ~ "145",
                                   trap_number == "146_2" & visit == "1" & village == "baiama" ~ "146",
                                   trap_number == "141_2" & visit == "1" & village == "baiama" ~ "101",
                                   trap_number == "14" & visit == "3" & village == "lalehun" ~ "13",
                                   trap_number == "14_2" & visit == "3" & village == "lalehun" ~ "14",
                                   trap_number == "20_2" & visit == "3" & village == "lalehun" ~ "26",
                                   trap_number == "221_2" & visit == "3" & village == "lalehun" ~ "295",
                                   trap_number == "123" & visit == "3" & village == "seilama" ~ "23",
                                   trap_number == "123_2" & visit == "3" & village == "seilama" ~ "123",
                                   TRUE ~ trap_number),
           swapped_lat = lat_dec,
           swapped_lon = lon_dec,
           lat_dec = case_when(lat_dec >= 811000 & lat_dec <= 990000 ~ lat_dec/100000, # Correcting multiple miscoded
                               village == "lalehun" & visit == 3 & grid_number %in% c(2, 4, 5) ~ (lat_dec - 8) * 100,
                               village == "seilama" & visit == 3 & grid_number == 2 ~ (lat_dec-8) * 100,
                               village == "seilama" & visit == 3 & grid_number == 3 ~ (lat_dec-8) * 100,
                               village == "seilama" & visit == 3 & grid_number == 4 ~ (lat_dec-8) * 100,
                               village == "seilama" & visit == 3 & grid_number == 5 ~ (lat_dec-8) * 100,
                               str_starts(lat_dec, "15") ~ swapped_lon,
                               TRUE ~ lat_dec),
           lat_dec = case_when(lat_dec == 5458.0000 ~ 0.5458, # fixing individual misrecorded coord
                               lat_dec == 50.5851 ~ 50.9581,
                               village == "lambayama" & grid_number == "2" & visit == "1" & trap_number == "50" ~ 51.0729,
                               village == "lalehun" & visit == 3 & trap_number == 159 ~ 11.925,
                               village == "lalehun" & visit == 3 & trap_number == 188 ~ 11.915,
                               village == "lalehun" & visit == 3 & trap_number == 239 ~ 11.633,
                               village == "seilama" & visit == 3 & trap_number == 208 ~ 7.424,
                               village == "seilama" & visit == 3 & trap_number == 203 ~ 7.412,
                               village == "lalehun" & visit == 3 & trap_number == 218 ~ 11.628,
                               village == "lalehun" & visit == 3 & trap_number == 10 ~ 11.7907,
                               village == "lalehun" & visit == 3 & trap_number == 35 ~ 11.7751,
                               village == "seilama" & visit == 3 & trap_number == 112 ~ 7.2447,
                               village == "seilama" & visit == 3 & trap_number == 203 ~ 8.07412,
                               village == "seilama" & visit == 3 & trap_number == 208 ~ 8.07424,
                               village == "seilama" & visit == 3 & trap_number == 12 ~ 11.53200,
                               TRUE ~ lat_dec),
           lon_dec = case_when(lon_dec >= -1000000 & lon_dec <= -180 ~ lon_dec/100000,
                               village == "lalehun" & visit == 3 & grid_number %in% c(2, 4, 5) ~ (lon_dec - 11) * 100,
                               village == "seilama" & visit == 3 & grid_number == 2 ~ (lon_dec-11) * 100,
                               village == "seilama" & visit == 3 & grid_number == 3 ~ (lon_dec-11) * 100,
                               village == "seilama" & visit == 3 & grid_number == 4 ~ (lon_dec-11) * 100,
                               village == "seilama" & visit == 3 & grid_number == 5 ~ (lon_dec-11) * 100,
                               str_starts(lon_dec, "49") ~ swapped_lat,
                               TRUE ~ lon_dec),
           lon_dec = case_when(lon_dec == 8.85905 ~ 8.5905, # fixing individual misrecorded coord
                               lon_dec == 1104813 ~ 11.04813,
                               village == "lalehun" & grid_number == "5" & visit == 3 & trap_number == 204 ~ 4.810,
                               village == "lalehun" & visit == 3 & trap_number == 35 ~ 4.7621,
                               village == "baiama" & grid_number == "4" & visit == 1 & trap_number == 195 ~ 15.9879,
                               village == "lambayama" & grid_number == "2" & visit == 1 & trap_number == 50 ~ 11.6820,
                               village == "seilama" & visit == 3 & trap_number == 228 ~ 11.866,
                               TRUE ~ lon_dec),
           lon_degree = 11,
           lat_degree = case_when(village == "lambayama" ~ 7,
                                  village == "baiama" ~ 7,
                                  TRUE ~ 8),
           lon_DdM = paste(paste(lon_degree, lon_dec, sep = "_"), "'", sep = ""),
           lat_DdM = paste(paste(lat_degree, lat_dec, sep = "_"), "'", sep = ""),
           lon = -1 * dg2dec(var = lon_DdM, Dg = "_", Min = "'"),
           lat = dg2dec(var = lat_DdM, Dg = "_", Min = "'"),
           grid_number = as.character(grid_number),
           trap_land_type = str_replace_all(
             case_when(is.na(proximity) ~ paste(trap_land_type, sep = " "),
                       is.na(trap_land_type) & !is.na(proximity) ~ paste(proximity, sep = " "),
                       TRUE ~ paste(trap_land_type, proximity, sep = ",")),
             " ", ", "),
           trap_land_type = snakecase::to_sentence_case(trap_land_type),
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
    dplyr::select(village, visit, date_set, trap_night, grid_number, trap_number, habitat, trap_land_type, elevation,
           trap_uid, lon, lat) %>%
    filter(!trap_number %in% trap_nos$trap_number) %>%
    mutate(trap_number = as.numeric(trap_number),
           across(any_of(factor_vars), ~ as_factor(.)),
           elevation = case_when(elevation >= 2000 ~ elevation/10,
                                 elevation <= 100 ~ elevation*10,
                                 TRUE ~ elevation),
           date_set = date_set + (as.numeric(trap_night)-1),
           trap_uid = paste0(village, "_", visit, "_", trap_night, "_", grid_number, "_", trap_number))
  
  coord_check <- full_trap_locations %>%
    distinct(village, visit, trap_number, .keep_all = T) %>%
    st_as_sf(coords = c("lon", "lat")) %>%
    st_set_crs(., value = 4326)
  
  coord_error <- bind_rows(coord_check %>%
                             filter(village == "lalehun") %>%
                             .[st_as_sf(village_bbox[["lalehun"]]), , op = st_disjoint],
                           coord_check %>%
                             filter(village == "seilama") %>%
                             .[st_as_sf(village_bbox[["seilama"]]), , op = st_disjoint],
                           coord_check %>%
                             filter(village == "bambawo") %>%
                             .[st_as_sf(village_bbox[["bambawo"]]), , op = st_disjoint],
                           coord_check %>%
                             filter(village == "baiama") %>%
                             .[st_as_sf(village_bbox[["baiama"]]), , op = st_disjoint],
                           coord_check %>%
                             filter(village == "lambayama") %>%
                             .[st_as_sf(village_bbox[["lambayama"]]), , op = st_disjoint])
  
  message(
    cat(
    paste(
      paste0("Using the study site bounding coordinates from the first visit there are potentially ", nrow(coord_error), " misspecified trap locations:"),
      paste0("These locations are contained in the coord_error dataframe"),
      paste0("The unique ID of the trap for the first trap night are ", knitr::combine_words(coord_error$trap_uid)),
      sep = "\n"))
  )
  
  coord_error <- full_trap_locations %>%
    distinct(village, visit, grid_number, trap_number, .keep_all = T) %>%
    mutate(coord ="Likely correct") %>%
    filter(!trap_uid %in% coord_error$trap_uid)  %>%
    st_as_sf(coords = c("lon", "lat")) %>%
    st_set_crs(., value = 4326) %>%
    bind_rows(coord_error %>%
                mutate(coord = "Likely incorrect")) %>%
    mutate(lon = st_coordinates(.)[,1],
           lat = st_coordinates(.)[,2])
  
  return_list <- list("odk_data" = full_trap_locations,
                      "coord_error" = coord_error)
                           
  
  return(return_list)
}
