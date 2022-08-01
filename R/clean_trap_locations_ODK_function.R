clean_trap_locations_ODK <- function(trap_sites = ODK_sites$trap_sites){
  
  source(here("R", "DdM_to_decimal_degrees_function.R"))
  
  all_files <- list.files(here("data", "raw_odk", paste0("trap_sites", "_", Sys.Date())), full.names = T)
  
  if(identical(all_files, character(0))) all_files <- list.files(tail(sort(list.files(here("data", "raw_odk"), pattern = "trap_sites_", full.names = TRUE)), 1), full.names = TRUE)
  
  bambawo_trap_locations <- read_csv(here("data", "raw_odk", "ODK_trap_bambawo_locations.csv"), show_col_types = FALSE)
  # due to a change in the the ODK form structure this data is read in from a previously saved file
  
  trap_locations <- read_csv(all_files[2], show_col_types = FALSE) %>%
    bind_rows(., bambawo_trap_locations) %>%
    rename("trap_number" = "site_grouping-trap_number",
           "longitude" = "site_grouping-longitude",
           "latitude" = "site_grouping-latitude",
           "elevation" = "site_grouping-elevation",
           "habitat" = "site_grouping-selected_habitat_trap",
           "proximity" = "site_grouping-selected_proximity",
           "trap_land_type" = "site_grouping-selected_land_type",
           "key" = "PARENT_KEY") %>%
    dplyr::select(-c("KEY", "site_grouping-trap_image", "site_grouping-bait_type"))
  
  house_sites <- read_csv(all_files[1], show_col_types = FALSE) %>%
    rename("visit_key" = "PARENT_KEY",
           "site_key" = "KEY") %>%
    mutate(latitude = case_when(latitude == 118702.0000 ~ 11.8702,
                                latitude == 11.31840 & trap_image == "1642601179101.jpg" ~ 7.31840,
                                TRUE ~ latitude)) %>%
    rename("number_traps" = "number_of_traps_in_house") %>%
    left_join(., trap_sites %>%
                dplyr::select(village_name, visit_number, study_site, habitat_type, key),
              by = c("visit_key" = "key"))
  
  house_traps <- read_csv(all_files[3], show_col_types = FALSE) %>%
    rename("site_key" = "PARENT_KEY") %>%
    mutate(visit_key = gsub("\\/within.*", "", site_key)) %>%
    dplyr::select(-KEY) %>%
    left_join(., trap_sites %>%
                dplyr::select(village_name, visit_number, study_site, habitat_type, key),
              by = c("visit_key" = "key"))
  
  indoor_traps <- tibble(t_1 = c(246, 250, 254, 258, 262, 140, 131, 146, 135, 130),
                         t_2 = c(247, 251, 255, 259, 263, 142, 132, 147, 136, 139),
                         t_3 = c(248, 252, 256, 260, 264, 143, 133, 127, 137, 295),
                         t_4 = c(249, 253, 257, 261, 265, 144, 134, 145, 138, 221))
  
  visit_3_houses <- house_sites %>% # The recording of which traps were within houses was not correctly completed for visit 3 in Lalehun and Seilama
    filter(visit_number == 3 & village_name %in% c("lalehun", "seilama")) %>%
    dplyr::select(-visit_key) %>%
    left_join(., house_traps %>%
                filter(visit_number == 3 & village_name %in% c("lalehun", "seilama")),
              by = c("site_key", "village_name", "visit_number", "study_site", "habitat_type")) %>%
    bind_cols(., indoor_traps) %>%
    dplyr::select(-trap_number) %>%
    pivot_longer(cols = c(t_1, t_2, t_3, t_4), values_to = "trap_number") %>%
    dplyr::select(-name) %>%
    mutate(visit_key = gsub("\\/within.*", "", site_key))
  
  houses <- house_traps %>%
    left_join(., house_sites,
              by = c("visit_key", "site_key", "village_name", "visit_number", "study_site", "habitat_type")) %>%
    filter(!site_key %in% visit_3_houses$site_key) %>%
    bind_rows(., visit_3_houses)
  
  full_trap_locations <- full_join(trap_sites, trap_locations, by = "key") %>%
    full_join(., houses,
              by = c("key" = "visit_key")) %>%
    mutate(longitude = coalesce(longitude.x, longitude.y),
           latitude = coalesce(latitude.x, latitude.y),
           elevation = coalesce(elevation.x, elevation.y),
           trap_number = coalesce(trap_number.x, trap_number.y),
           number_traps = coalesce(number_traps.x, number_traps.y),
           habitat_type = coalesce(habitat_type.x, habitat_type.y),
           study_site = coalesce(study_site.x, study_site.y),
           trap_number = coalesce(trap_number.x, trap_number.y),
           village_name = coalesce(village_name.x, village_name.y),
           visit_number = coalesce(visit_number.x, visit_number.y),
           key = factor(key)) %>%
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
           habitat = case_when(habitat_type == "village_inside" ~ "houses",
                               TRUE ~ habitat)) %>%
    ungroup() %>%
    # sort misrecorded study_sites 
    mutate(grid_number = case_when(site_use == "Forest" & village == "seilama" & visit == 3 & grid_number == "4" ~ 5,
                                   site_use == "village_inside" ~ 7,
                                   village == "lalehun" & visit == "3" & as.numeric(trap_number) < 99 & as.numeric(trap_number) >= 50 ~ 3,
                                   village == "lambayama" & visit == "2" & grid_number == "1" ~ 4,
                                   village == "lambayama" & visit == "2" & grid_number == "2" ~ 1,
                                   village == "lambayama" & visit == "2" & grid_number == "3" ~ 2,
                                   village == "lambayama" & visit == "2" & grid_number == "4" ~ 3,
                                   village == "lambayama" & visit == "4" & trap_number %in% c(148:197) ~ 4,
                                   TRUE ~ grid_number)) %>%
    # sort misrecorded trap numbers
    group_by(village, visit, trap_number, key) %>%
    mutate(trap_number = paste(trap_number, row_number(), sep = "_"),
           trap_number = str_remove(trap_number, "_1"),
           trap_number = case_when(trap_number == "77_2" & visit == "1" & village == "lambayama" ~ "78", # Some trap sites have been misrecorded
                                   
                                   trap_number == "85_2" & visit == "1" & village == "bambawo" ~ "95",
                                   trap_number == "184_2" & visit == "1" & village == "bambawo" ~ "187",
                                   
                                   trap_number == "146" & visit == "1" & village == "baiama" ~ "145",
                                   trap_number == "146_2" & visit == "1" & village == "baiama" ~ "146",
                                   trap_number == "141_2" & visit == "1" & village == "baiama" ~ "101",
                                   
                                   trap_number == "2002" & visit == "2" & village == "baiama" ~ "202",
                                   
                                   trap_number == "237" & visit == "2" & village == "lambayama" & grid_number == 5 ~ "236",
                                   
                                   trap_number == "14" & visit == "3" & village == "lalehun" ~ "13",
                                   trap_number == "14_2" & visit == "3" & village == "lalehun" ~ "14",
                                   trap_number == "20_2" & visit == "3" & village == "lalehun" ~ "26",
                                   trap_number == "221_2" & visit == "3" & village == "lalehun" ~ "295",
                                   
                                   trap_number == "123" & visit == "3" & village == "seilama" ~ "23",
                                   trap_number == "123_2" & visit == "3" & village == "seilama" ~ "123",
                                   
                                   trap_number == "296_2" & visit == "4" & village == "seilama" ~ "297",
                                   
                                   trap_number == "267_2" & visit == "4" & village == "lalehun" ~ "270",
                                   
                                   TRUE ~ trap_number),
           trap_number = as.numeric(trap_number),
           trap_number = case_when(is.na(trap_number) & visit == "2" & village == "lambayama" & grid_number == 2 ~ 62,
                                   is.na(trap_number) & visit == "2" & village == "lambayama" & grid_number == 4 & elevation == 152 ~ 195,
                                   is.na(trap_number) & visit == "2" & village == "lambayama" & grid_number == 4 & elevation == 155 ~ 294,
                                   is.na(trap_number) & visit == "2" & village == "lambayama" & grid_number == 6 ~ 237,
                                   
                                   is.na(trap_number) & visit == "2" & village == "baiama" & grid_number == "6" & proximity == "garden" ~ 233,
                                   is.na(trap_number) & visit == "2" & village == "baiama" & grid_number == "6" & proximity == "road" ~ 245,
                                   
                                   is.na(trap_number) & visit == "5" & village == "seilama" & grid_number == "6" ~ 270,
                                   
                                   is.na(trap_number) & visit == "5" & village == "lalehun" & grid_number == "4" ~ 175,
                                   is.na(trap_number) & visit == "6" & village == "lalehun" & grid_number == "7" ~ 343,
                                   
                                   is.na(trap_number) & visit == "3" & village == "baiama" & grid_number == "4" ~ 150,
                                   is.na(trap_number) & visit == "3" & village == "baiama" & grid_number == "2" ~ 97,
                                   
                                   is.na(trap_number) & visit == "4" & village == "baiama" & grid_number == "4" ~ 197,
                                   
                                   is.na(trap_number) & visit == "3" & village == "lambayama" & grid_number == "4" & proximity == "garden" ~ 170,
                                   is.na(trap_number) & visit == "3" & village == "lambayama" & grid_number == "4" & proximity == "house" ~ 179,
                                   
                                   is.na(trap_number) & visit == "4" & village == "baiama" & grid_number == "7" ~ 264,
                                   
                                   is.na(trap_number) & visit == "4" & village == "lambayama" & grid_number == "6" ~ 294,
                                   
                                   is.na(trap_number) & visit == "6" & village == "seilama" & grid_number == "5" ~ 220,
                                   is.na(trap_number) & visit == "6" & village == "seilama" & grid_number == "3" ~ 147,
                                   
                                   is.na(trap_number) & visit == "6" & village == "lalehun" & grid_number == "6" ~ 293,
                                   
                                   is.na(trap_number) & key == "uuid:7d2cbdd3-c9fe-4d6b-af1b-933d389ebff5" ~ 197,
                                   is.na(trap_number) & key == "uuid:0ceb1121-b5e9-4675-87eb-383fe0273075" ~ 270,
                                   is.na(trap_number) & key == "uuid:4eab9b5f-1235-4e78-b7aa-f363e8afbb98" ~ 236,
                                   is.na(trap_number) & key == "uuid:5888a838-d656-40af-bd53-cd065ad4aaf8" & proximity == "garden" ~ 245,
                                   is.na(trap_number) & key == "uuid:5888a838-d656-40af-bd53-cd065ad4aaf8" & proximity == "road" ~ 233,
                                   
                                   TRUE ~ as.numeric(trap_number))) %>%
    mutate(lon_degree = 11,
           lat_degree = case_when(village == "lambayama" ~ 7,
                                  village == "baiama" ~ 7,
                                  TRUE ~ 8),
           swapped_lat = lat_dec,
           swapped_lon = lon_dec,
           lon_dec = case_when(
             village == "lalehun" & visit == 3 & trap_number == 204 ~ 4.802,
             village == "lalehun" & visit == 3 & lon_dec >= 11 ~ (lon_dec - 11) * 100,
             village == "lalehun" & visit == 3 & trap_number == 35 ~ 4.7621,
             
             village == "lalehun" & visit == 4 & trap_number == 1 ~ 4.7653,
             village == "lalehun" & visit == 4 & trap_number == 45 ~ 4.7544,
             village == "lalehun" & visit == 4 & trap_number == 27 ~ 4.7653,
             village == "lalehun" & visit == 4 & trap_number == 84 ~ 4.7430,
             
             village == "lalehun" & visit == 4 & lat_dec >=4 & lat_dec <= 5 ~ swapped_lat,
             village == "lalehun" & visit == 4 & lat_dec > 11 ~ (swapped_lat - 11) * 100,
             
             village == "lalehun" & visit == 5 & trap_number == 176  ~ 4.6840,
             village == "lalehun" & visit == 5 & trap_number == 175  ~ 4.6857,
             village == "lalehun" & visit == 5 & trap_number == 161  ~ 4.6895,
             
             village == "lalehun" & visit == 5 & grid_number %in% c(5, 4) ~ (swapped_lat - 11) * 100,
             
             village == "lalehun" & visit == 6 & trap_number == 54  ~ 4.770,
             village == "lalehun" & visit == 6 & trap_number == 95  ~ 4.737,
             
             village == "lalehun" & visit == 6 & grid_number %in% c(2, 3, 4, 5) ~ (swapped_lat - 11) * 100,
             
             village == "lalehun" & visit == 6 & grid_number== 7 & trap_number %in% c(4, 311, 312, 313) ~ swapped_lat,
             
             village == "seilama" & visit == 3 & trap_number == 228 ~ 11.866,
             village == "seilama" & visit == 3 & trap_number == 12 ~ 11.5320,
             village == "seilama" & visit == 3 & trap_number == 22 ~ 11.5418,
             village == "seilama" & visit == 3 & trap_number == 290 ~ 11.6351,
             village == "seilama" & visit == 3 & trap_number == 63 ~ 11.6532,
             
             village == "seilama" & visit == 3 & grid_number %in% c(2, 3, 4, 5) ~ (lon_dec-11) * 100,
             
             village == "seilama" & visit == 4 & trap_number == 19 ~ 11.5402,
             village == "seilama" & visit == 4 & trap_number == 294 ~ 11.62526,
             
             village == "seilama" & visit == 4 & grid_number %in% c(1, 6, 7) ~ swapped_lat,
             village == "seilama" & visit == 4 & grid_number %in% c(2, 3, 4) ~ (swapped_lat - 11) * 100,
             
             village == "seilama" & visit == 6 & trap_number == 1 ~ 11.531,
             
             village == "baiama" & visit == 1 & trap_number %in% c(2, 3, 4, 5, 6, 7) ~ swapped_lat,
             village == "baiama" & visit == 1 & trap_number == 195 ~ 15.9879,
             
             village == "baiama" & visit == 2 & trap_number == 195 ~ 15.975,
             village == "baiama" & visit == 2 & trap_number == 26 ~ 15.017,
             village == "baiama" & visit == 2 & trap_number == 28 ~ 15.019,
             village == "baiama" & visit == 2 & trap_number == 109 ~ 15.736,
             village == "baiama" & visit == 2 & trap_number == 111 ~ 15.724,
             village == "baiama" & visit == 2 & trap_number == 121 ~ 15.726,
             village == "baiama" & visit == 2 & trap_number == 122 ~ 15.750,
             village == "baiama" & visit == 2 & trap_number == 128 ~ 15.756,
             village == "baiama" & visit == 2 & trap_number == 130 ~ 15.733,
             village == "baiama" & visit == 2 & trap_number == 140 ~ 15.7530,
             
             village == "baiama" & visit == 2 & grid_number %in% c(4, 5, 6, 7) ~ swapped_lat,
             village == "baiama" & visit == 2 & grid_number %in% c(1, 2, 3) ~ (swapped_lat - 11) * 100,
             
             village == "baiama" & visit == 3 & trap_number == 231 ~ 16.0647,
             
             village == "baiama" & visit == 3 & grid_number %in% c(1, 2, 3) ~ (swapped_lat - 11) * 100,
             
             village == "baiama" & visit == 4 & trap_number == 50 ~ 15.245,
             village == "baiama" & visit == 4 & trap_number == 55 ~ 15.242,
             village == "baiama" & visit == 4 & trap_number == 98 ~ 15.234,
             
             village == "baiama" & visit == 4 & grid_number %in% c(2, 3) ~ (swapped_lat - 11) * 100,
             
             village == "lambayama" & visit == 1 & trap_number == 50 ~ 11.6820,
             
             village == "lambayama" & visit == 2 & trap_number == 50 ~ 11.6765,
             village == "lambayama" & visit == 2 & trap_number == 156 ~ 11.8062,
             village == "lambayama" & visit == 2 & trap_number == 165 ~ 11.8136,
             village == "lambayama" & visit == 2 & trap_number == 119 ~ 11.550,
             village == "lambayama" & visit == 2 & trap_number == 215 ~ 11.7934,
             
             village == "lambayama" & visit == 2 & grid_number %in% c(1, 2, 3, 4, 5, 6, 7) ~ swapped_lat,
             
             village == "lambayama" & visit == 3 & trap_number == 226 ~ 11.8027,
             
             village == "lambayama" & visit == 3 & grid_number %in% c(1, 2, 3) ~ (swapped_lat - 11) * 100,
             
             village == "lambayama" & visit == 4 & trap_number == 76 ~ 11.675,
             village == "lambayama" & visit == 4 & trap_number == 62 ~ 11.678,
             village == "lambayama" & visit == 4 & trap_number == 94 ~ 11.680,
             village == "lambayama" & visit == 4 & trap_number == 146 ~ 11.533,
             village == "lambayama" & visit == 4 & trap_number == 161 ~ 11.807,
             village == "lambayama" & visit == 4 & trap_number == 176 ~ 11.807,
             village == "lambayama" & visit == 4 & trap_number == 184 ~ 11.816,
             village == "lambayama" & visit == 4 & grid_number == 7 & trap_number %in% c(303, 304, 305, 306) ~ 11.809,
             
             village == "lambayama" & visit == 4 & trap_number == 173 ~ 11.806,
             
             village == "bambawo" & visit == 1 & trap_number == 191 ~ 8.5905,
             
             village == "seilama" & visit == 6 & trap_number == 65 ~ 11.651,
             village == "seilama" & visit == 6 & trap_number == 89 ~ 11.657,
             village == "seilama" & visit == 6 & trap_number == 223 ~ 11.862,
             
             village == "lambayama" & visit == 4 & grid_number %in% c(1, 2, 3) ~ (swapped_lat - 11) * 100,
             
             TRUE ~ lon_dec),
           lat_dec = case_when(
             
             village == "lalehun" & visit == 3 & trap_number == 159 ~ 11.925,
             village == "lalehun" & visit == 3 & trap_number == 188 ~ 11.915,
             village == "lalehun" & visit == 3 & trap_number == 239 ~ 11.633,
             village == "lalehun" & visit == 3 & trap_number == 218 ~ 11.628,
             village == "lalehun" & visit == 3 & trap_number == 10 ~ 11.791,
             village == "lalehun" & visit == 3 & trap_number == 35 ~ 11.7751,
             
             village == "lalehun" & visit == 3 & lat_dec <= 9 ~ (lat_dec-8)*100,
             
             village == "lalehun" & visit == 4 & trap_number == 27 ~ 11.7797,
             village == "lalehun" & visit == 4 & trap_number == 45 ~ 11.7787,
             village == "lalehun" & visit == 4 & trap_number == 1 ~ 11.8006,
             village == "lalehun" & visit == 4 & trap_number == 142 ~ 11.962,
             
             village == "lalehun" & visit == 4 & lat_dec <= 12 & lat_dec >= 8 ~ (swapped_lon-8)*100,
             village == "lalehun" & visit == 4 & lat_dec <= 5 ~ swapped_lon,
             
             village == "lalehun" & visit == 5 & grid_number %in% c(4, 5) ~ (swapped_lon-8)*100,
             
             village == "lalehun" & visit == 6 & grid_number== 7 & trap_number %in% c(4, 311, 312, 313) ~ elevation,
             village == "lalehun" & visit == 6 & grid_number== 7 & trap_number %in% c(303, 304, 305, 306) ~ 11.829,
             village == "lalehun" & visit == 6 & grid_number== 1 & trap_number == 12 ~ 11.784,
             village == "lalehun" & visit == 6 & trap_number == 54 ~ 11.920,
             village == "lalehun" & visit == 6 & trap_number == 44 ~ 11.785,
             
             village == "lalehun" & visit == 6 & grid_number== 4 & trap_number == 191 ~ (swapped_lon-88)*100,
             village == "lalehun" & visit == 6 & grid_number %in% c(2, 3, 4, 5) ~ (swapped_lon-8)*100,
             
             village == "seilama" & visit == 3 & trap_number == 208 ~ 7.424,
             village == "seilama" & visit == 3 & trap_number == 203 ~ 7.412,
             village == "seilama" & visit == 3 & trap_number == 112 ~ 7.2447,
             village == "seilama" & visit == 3 & trap_number == 278 ~ 7.3694,
             village == "seilama" & visit == 3 & trap_number == 12 ~ 7.3343,
             
             village == "seilama" & visit == 3 & grid_number %in% c(2, 3, 4, 5) ~ (lat_dec-8) * 100,
             
             village == "seilama" & visit == 4 & trap_number == 54 ~ 7.3775,
             
             village == "seilama" & visit == 4 & grid_number %in% c(1, 6, 7) ~ swapped_lon,
             village == "seilama" & visit == 4 & grid_number %in% c(2, 3, 4) ~ (swapped_lon - 8) * 100,
             
             village == "seilama" & visit == 6 & grid_number == 1 & trap_number == 39 ~ 7.33,
             village == "seilama" & visit == 6 & trap_number == 247 ~ 7.323,
             village == "seilama" & visit == 6 & trap_number == 272 ~ 7.357,
             village == "seilama" & visit == 6 & trap_number == 278 ~ 7.338,
             village == "seilama" & visit == 6 & trap_number == 280 ~ 7.331,
             
             village == "baiama" & visit == 1 & trap_number %in% c(2, 3, 4, 5, 6, 7) ~ swapped_lon,
             
             village == "baiama" & visit == 2 & trap_number == 185 ~ 50.1830,
             
             village == "baiama" & visit == 2 & grid_number %in% c(4, 5, 6, 7) ~ swapped_lon,
             village == "baiama" & visit == 2 & grid_number %in% c(1, 2) ~ (swapped_lon- 7) * 100,
             village == "baiama" & visit == 2 & grid_number %in% c(3) ~ (swapped_lon - 8) * 100,
             
             village == "baiama" & visit == 3 & grid_number == 4 & trap_number == 150 ~ 50.1842,
             village == "baiama" & visit == 3 & trap_number == 168 ~ 50.1906,
             village == "baiama" & visit == 3 & trap_number == 169 ~ 50.1907,
             village == "baiama" & visit == 3 & trap_number == 185 ~ 50.1909,
             village == "baiama" & visit == 3 & trap_number == 195 ~ 50.1972,
             village == "baiama" & visit == 3 & trap_number == 213 ~ 50.189,
             
             village == "baiama" & visit == 3 & grid_number %in% c(1, 2, 3) ~ (swapped_lon - 7) * 100,
             
             village == "baiama" & visit == 4 & trap_number == 50 ~ 49.795,
             village == "baiama" & visit == 4 & grid_number == 1 & lat_dec == 49 ~ 49.472,
             village == "baiama" & visit == 4 & trap_number == 66 ~ 49.794,
             village == "baiama" & visit == 4 & trap_number == 147 ~ 49.835,
             village == "baiama" & visit == 4 & trap_number == 213 ~ 50.189,
             
             village == "baiama" & visit == 4 & grid_number == 4 & trap_number %in% c(149, 152) ~ lat_dec/1000,
             
             village == "baiama" & visit == 4 & grid_number %in% c(2, 3) ~ (swapped_lon - 7) * 100,
            
             village == "lambayama" & visit == 1 & trap_number == 50 ~ 51.0729,
             village == "lambayama" & visit == 1 & trap_number == 153 ~ 50.9851,
             
             village == "lambayama" & visit == 2 & trap_number == 1 ~ 50.9861,
             village == "lambayama" & visit == 2 & trap_number == 83 ~ 51.0624,
             village == "lambayama" & visit == 2 & trap_number == 165 ~ 50.9904,
             
             village == "lambayama" & visit == 2 & grid_number %in% c(1, 2, 3, 4, 5, 6, 7) ~ swapped_lon,
             
             village == "lambayama" & visit == 3 & trap_number %in% c(271:274) ~ 51.0953,
             
             village == "lambayama" & visit == 3 & grid_number %in% c(1, 2, 3) ~ (swapped_lon - 7) * 100,
             
             village == "lambayama" & visit == 4 & trap_number == 3 ~ 51.001,
             village == "lambayama" & visit == 4 & trap_number == 4 ~ 51.002,
             village == "lambayama" & visit == 4 & trap_number == 25 ~ 51.055,
             village == "lambayama" & visit == 4 & trap_number == 50 ~ 51.0804,
             village == "lambayama" & visit == 4 & trap_number == 92 ~ 51.081,
             village == "lambayama" & visit == 4 & trap_number == 94 ~ 51.078,
             village == "lambayama" & visit == 4 & trap_number == 272 ~ 51.000,
             village == "lambayama" & visit == 4 & trap_number == 263 ~ 51.004,
             village == "lambayama" & visit == 4 & trap_number == 282 ~ 51.005,
             
             village == "lambayama" & visit == 4 & grid_number %in% c(1, 2, 3) ~ (swapped_lon - 7) * 100,
             
             village == "bambawo" & visit == 1 & trap_number == 53 ~ 0.5458,
             
             village == "seilama" & visit == 6 & trap_number == 207 ~ 7.423,
             
             village == "lambayama" & visit == 4 & trap_number %in% c(327:330) ~ 51.045,
             
             village == "baiama" & visit == 4 & trap_number == 238 ~ 50.168,
             
             TRUE ~ lat_dec
           )) %>%
    mutate(lon_DdM = paste(paste(lon_degree, lon_dec, sep = "_"), "'", sep = ""),
           lat_DdM = paste(paste(lat_degree, lat_dec, sep = "_"), "'", sep = ""),
           lon = -1 * dg2dec(var = lon_DdM, Dg = "_", Min = "'"),
           lat = dg2dec(var = lat_DdM, Dg = "_", Min = "'"),
           trap_land_type = str_replace_all(
             case_when(is.na(proximity) ~ paste(trap_land_type, sep = " "),
                       is.na(trap_land_type) & !is.na(proximity) ~ paste(proximity, sep = " "),
                       TRUE ~ paste(trap_land_type, proximity, sep = ",")),
             " ", ", "),
           trap_land_type = case_when(site_use == "housing" ~ "indoors",
                                      TRUE ~ str_to_sentence(trap_land_type)),
           habitat = case_when(habitat == "agricultural" ~ habitat_type,
                               TRUE ~ habitat),
           job = case_when(job == "other" ~ str_to_lower(job_other),
                           TRUE ~ job)) %>%
    mutate(grid_number = case_when(habitat_type == "village_inside" | str_detect(site_use, "In house|Indoor") ~ 7,
                                   habitat_type == "village_outside" & !grid_number %in% c(1, 3, 4) ~ 7,
                                   TRUE ~ grid_number)) %>% # recode study site to 6 and 7 being within the village grouped as 7
    mutate(SubmissionDate = ymd(as.Date(SubmissionDate)),
           date_set = ymd(as.Date(date_set)),
           village = factor(village),
           grid_number = factor(grid_number),
           trap_number = as.integer(trap_number),
           site_use = factor(site_use),
           intensity = factor(intensity),
           crop_type = factor(crop_type),
           habitat = factor(habitat),
           proximity = factor(proximity),
           trap_land_type = factor(trap_land_type),
           job = factor(job),
           roof = factor(roof),
           containers = factor(containers),
           sleeping = factor(sleeping)) %>%
    dplyr::select(SubmissionDate, date_set, village, visit, grid_number, trap_number, site_use, intensity, crop_type, habitat,
                  proximity, trap_land_type, lon, lon_dec, lon_DdM, lat, lat_dec, lat_DdM, elevation, key, site_key, number_of_adults, job, number_of_children, roof, walls,
                  floor, containers, sleeping, trap_image)
  
  trap_nos <- filter(full_trap_locations, grepl("_2", trap_number))
  
  message(cat(ifelse(nrow(trap_nos) == 0, "There are no duplicated trap numbers,",
                     paste(
                       paste0("There are ", nrow(trap_nos), " duplicated trap numbers:"),
                       paste0("They are trap numbers: ", knitr::combine_words(trap_nos$trap_number)),
                       sep = "\n"))))
  
  missing_trap_nos <- full_trap_locations %>%
    filter(is.na(trap_number))
  
  # Trap numbers are missing from the Baiama site 1 visit 4 I assume these are 9 to 49
  missing_baiama_4_1 <- missing_trap_nos %>%
    filter(village == "baiama" &
           visit == 4 &
           grid_number == 1)
  missing_baiama_4_1$trap_number <- 9:49
  
  missing_trap_nos <- anti_join(missing_trap_nos, missing_baiama_4_1, by = c("village", "visit", "grid_number", "lat_dec", "lon_dec"))
  
  correcting_missing_tn <- full_trap_locations %>%
    left_join(missing_baiama_4_1 %>%
                   select(village, visit, grid_number, trap_number, lon_dec, lat_dec, key), by = c("village", "visit", "grid_number", "lon_dec", "lat_dec", "key")) %>%
    mutate(trap_number = coalesce(trap_number.x, trap_number.y)) %>%
    select(-trap_number.x, -trap_number.y)
  
  message(cat(ifelse(nrow(missing_trap_nos) == 0, "There are no uncorrected missing trap numbers,",
                     paste(
                       paste0("There are ", nrow(missing_trap_nos), " missing trap numbers:")))))
  
  full_trap_locations <- correcting_missing_tn
  
  full_trap_locations <- full_trap_locations %>%
    group_by_all() %>%
    expand(trap_night = 1:4) %>%
    ungroup() %>%
    filter(!trap_number %in% trap_nos$trap_number) %>%
    mutate(elevation = case_when(elevation >= 2000 ~ elevation/10,
                                 elevation <= 100 ~ elevation*10,
                                 TRUE ~ elevation),
           date_set = date_set + (as.numeric(trap_night)-1),
           trap_uid = paste0(village, "_", visit, "_", trap_night, "_", grid_number, "_", trap_number)) %>%
    dplyr::select(-c(lat_dec, lat_DdM, lon_dec, lon_DdM)) %>%
    mutate(lat = case_when(village == "baiama" & visit == 4 & trap_number == 49 ~ 7.824753,
                           TRUE ~ lat)) # make some final corrections to coordinates
  
  message("Traps expanded for 4 nights at each site.")
  
  # fix improbable coordinates here
  full_trap_locations <- fix_improbable_coordinates(data =  full_trap_locations)
  
  improbable_coordinates <- full_trap_locations %>%
    distinct(village, visit, trap_number, .keep_all = T) %>%
    filter(lon <= -14 | lon >= -10 | lat  >= 9 | lat <= 7)
  
  message(paste(nrow(improbable_coordinates), "trap sites have improbable coordinates that need changing within the correction dictionary. The trap unique id's are stored in the improbable coordinates element"))
  
  coord_check <- full_trap_locations %>%
    distinct(village, visit, trap_number, .keep_all = T) %>%
    drop_na(lon, lat) %>%
    st_as_sf(coords = c("lon", "lat")) %>%
    st_set_crs(., value = 4326) %>%
    ungroup()
  
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
                             .[st_as_sf(village_bbox[["lambayama"]]), , op = st_disjoint]) %>%
    filter(!(village == "baiama" & visit %in% c(2, 3, 4) & grid_number %in% c("5", "6", "7"))) # these sites were set up after visit 1
  
  message(
    cat(
      paste(
        paste0("Using the study site bounding coordinates from the first visit there are potentially ", nrow(coord_error), " misspecified trap locations:"),
        paste0("These locations are contained in the coord_error dataframe. This does not include locations with improbable coordinates"),
        sep = "\n"))
  )
  
  coord_error <- list(spatial = full_trap_locations %>%
                        distinct(village, visit, grid_number, trap_number, .keep_all = T) %>%
                        drop_na(lon, lat) %>%
                        mutate(coord ="Likely correct") %>%
                        filter(!trap_uid %in% coord_error$trap_uid)  %>%
                        st_as_sf(coords = c("lon", "lat")) %>%
                        st_set_crs(., value = 4326) %>%
                        bind_rows(coord_error %>%
                                    mutate(coord = "Likely incorrect")) %>%
                        mutate(lon = st_coordinates(.)[,1],
                               lat = st_coordinates(.)[,2]),
                      traps_error = full_trap_locations %>%
                        filter(trap_uid %in% coord_error$trap_uid))
  
  return(output = list(full_trap_locations = full_trap_locations,
                       improbable_coordinates = improbable_coordinates,
                       coord_error = coord_error))
}
