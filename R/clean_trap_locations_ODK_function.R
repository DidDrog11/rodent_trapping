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
              by = c("visit_key" = "key")) %>%
    distinct()
  
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
    distinct() %>%
    bind_cols(., indoor_traps) %>%
    dplyr::select(-trap_number) %>%
    pivot_longer(cols = c(t_1, t_2, t_3, t_4), values_to = "trap_number") %>%
    dplyr::select(-name) %>%
    mutate(visit_key = gsub("\\/within.*", "", site_key))
  
  houses <- house_traps %>%
    left_join(., house_sites,
              by = c("visit_key", "site_key", "village_name", "visit_number", "study_site", "habitat_type")) %>%
    filter(!site_key %in% visit_3_houses$site_key) %>%
    bind_rows(., visit_3_houses) %>%
    distinct()
  
  # The first step in the cleaning process is to align the visits with the canonical values based on when the form was created and when the trapping session occurred
  # Potentially a previously started form was used for visit 10 in Lalehun uuid:a00e7269-057e-4016-a462-ca64b3988bfb

  fix_visit <- full_join(trap_sites, trap_locations, by = "key") %>%
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
    rename("village" = "village_name",
           "visit" = "visit_number",
           "grid_number" = "study_site",
           "lon_dec" = "longitude",
           "lat_dec" = "latitude") %>%
    mutate(form_entry = case_when(key %in% c("uuid:0d0621a7-768a-41b8-a563-31f7d31c9e47",
                                             "uuid:1f75e3b2-478e-4b5d-a475-de5aaed6593c") ~ ymd("2022-08-03"),
                                  key %in% c("uuid:a00e7269-057e-4016-a462-ca64b3988bfb") ~ ymd("2023-04-21"),
                                  TRUE ~ form_entry)) %>%
    mutate(year = year(form_entry),
           month = month(form_entry),
           day = dt_case_when(village == "seilama" & month == 10 & year == 2022 ~ day(form_entry),
                              TRUE ~ as.integer(NA))) %>%
    left_join(visit_dates %>%
                mutate(day = as.integer(day)) %>%
                rename(visit_canonical = visit),
              by = c("year", "month", "day", "village")) %>%
    drop_na(form_entry) %>%
    mutate(visit = visit_canonical) %>%
    select(-year, -month, -day, -visit_canonical) %>%
    distinct()
  
  fix_houses <- fix_visit %>%
    # manage houses
    group_by(village, visit, grid_number) %>%
    mutate(site_use = case_when(habitat_type == "village_inside" ~ "housing",
                                TRUE ~ site_use),
           intensity = case_when(habitat_type == "village_inside" ~ "intense",
                                 TRUE ~ intensity),
           habitat = case_when(habitat_type == "village_inside" ~ "houses",
                               key == "uuid:7d2cbdd3-c9fe-4d6b-af1b-933d389ebff5" ~ "proximal_agriculture",
                               TRUE ~ habitat),
           habitat_type = case_when(key == "uuid:7d2cbdd3-c9fe-4d6b-af1b-933d389ebff5" ~ "proximal_agriculture",
                                    TRUE ~ habitat_type)) %>%
    ungroup() %>%
    distinct()
  
  # The traps for baiama visit 6 site 1 were numbered wrong. No rodents were trapped at this site so the numbers are not so important for matching
  # they will be recoded as 1:49 as they should be
  
  baiama_6_1 <- fix_houses %>%
    filter(key == "uuid:58812f58-03c4-4e74-8320-1ffd8304deaa") %>%
    mutate(trap_number = row_number())
  
  remove_baiama_6_1 <- fix_houses %>%
    filter(key != "uuid:58812f58-03c4-4e74-8320-1ffd8304deaa")
  
  # Combine these again for subsequent processing
  prep_trap_numbers <- bind_rows(remove_baiama_6_1, baiama_6_1) %>%
    # Remove duplicated entries that do not contain data
    filter(!key %in% c("uuid:fb73e831-1048-4a65-883b-489d03ad0383", #Grid number 1 Seilama visit 6
                       "uuid:e81caa23-6d1d-431d-a6cf-2f0a700a1353", #Grid number 2 Lalehun visit 7 this data appears to be missing but this entry does not contain useful data
                       "uuid:273a50c3-82e5-4346-b8fa-162307ad4d3f", #Grid number 1 Seilama visit 3, data is duplicated
                       "uuid:16e261ed-a712-4e6e-a2be-3a7706265985", #Contains two traps, unclear whether this was a mistake
                       "uuid:88c3c5bb-3d43-45d4-b77b-e06fb011d704", # Grid 5 Seilama visit 4, no trap data
                       "uuid:50d8c3fc-9d92-455f-b079-39ea097b2879",
                       "uuid:eba75906-d935-47aa-9922-2e01f1cafa73",
                       "uuid:832983ff-0500-42d9-94b3-e12f4cf3432f", # Grids 4, 5 and 7 Lalehun visit 4, no trap data
                       "uuid:a88b065f-bf90-4ca6-a5ff-b83512774cd8", # Lalehun visit 2 grid 5 was collected on paper
                       "uuid:baae3f5a-7e17-4a6b-869e-e1b3d3e59535", # As was grid 6
                       "uuid:99929980-5bf2-4852-a748-29aad213ca5d", # Duplicate of above
                       "uuid:fdee86a1-60e2-452d-93e9-5eea899f6bda",
                       "uuid:96ba17dc-9d66-4432-bb78-cd84cfc514f1"
                       )) %>%
    filter(!(key %in% c("uuid:e3dd03fa-9fdc-44e0-a4a9-f4d9471b9b95",
                        "uuid:1fc265fd-0953-4919-b138-1dd1695001b1",
                        "uuid:3a2029b8-bb47-4d28-a6e8-10247d8608cb",
                        "uuid:3ca04e00-549b-4e01-9b61-ac2dfd56c2ca",
                        "uuid:9a81b76f-2a33-43d1-95c2-91ac4f954441",
                        "uuid:459531db-0653-43fb-aba2-1fffe05c9afc",
                        "uuid:8427d4ab-7c98-4df9-a922-60f24f06462c"
                        ) & is.na(elevation))) # Remove single rows where they have miscounted the number of traps
  
  fix_trap_numbers <- prep_trap_numbers %>%
    group_by(key, village, visit, trap_number) %>%
    mutate(trap_number_duplicates = case_when(is.na(trap_number) ~ "Not numbered",
                                              TRUE ~ paste(trap_number, row_number(), sep = "_"))) %>%
    ungroup() %>%
    mutate(trap_number = dt_case_when(key == "uuid:0ceb1121-b5e9-4675-87eb-383fe0273075" & trap_number_duplicates == "290_1" ~ 270,
                                      key == "uuid:0ceb1121-b5e9-4675-87eb-383fe0273075" & trap_number_duplicates == "290_2" ~ 290,
                                      key == "uuid:0d0621a7-768a-41b8-a563-31f7d31c9e47" & trap_number_duplicates == "246_2" ~ 247,
                                      key == "uuid:14a15395-72f1-4c21-ba8f-d87f1a325217" & trap_number_duplicates == "78_2" ~ 79,
                                      key == "uuid:1666a94e-0085-4d1d-9319-8919f270e9fb" & trap_number_duplicates == "196_2" ~ 294,
                                      key == "uuid:18f14b9a-9dac-4f1f-bbfa-344edacd3848" & trap_number_duplicates == "236_1" ~ 235,
                                      key == "uuid:18f14b9a-9dac-4f1f-bbfa-344edacd3848" & trap_number_duplicates == "236_2" ~ 236,
                                      key == "uuid:18f14b9a-9dac-4f1f-bbfa-344edacd3848" & trap_number_duplicates == "244_1" ~ 240,
                                      key == "uuid:18f14b9a-9dac-4f1f-bbfa-344edacd3848" & trap_number_duplicates == "244_2" ~ 244,
                                      key == "uuid:18f14b9a-9dac-4f1f-bbfa-344edacd3848" & trap_number_duplicates == "140_1" & habitat_type == "village_outside" ~ 227,
                                      key == "uuid:1b0f5761-d581-406b-8fd4-d5585fce4f33" & trap_number_duplicates == "116_2" ~ 147, #Two 116's, 147 is missing, no rodents trapped in either of the 116
                                      key == "uuid:1fc265fd-0953-4919-b138-1dd1695001b1" & trap_number_duplicates == "Not numbered" ~ 294,
                                      key == "uuid:2374d745-45e3-42b7-b651-7733295b3142" & trap_number_duplicates == "278_2" ~ 293,
                                      key == "uuid:301ee287-2635-4ba4-96f3-cc80ceec85be" & trap_number_duplicates == "184_2" ~ 187,
                                      key == "uuid:3921b449-10a2-4784-be18-2cdd76455e38" & trap_number_duplicates == "96_2" ~ 97,
                                      key == "uuid:459531db-0653-43fb-aba2-1fffe05c9afc" & trap_number_duplicates == "77_2" ~ 78,
                                      key == "uuid:45fcea23-9ec3-4b58-915a-75ee31a08402" & trap_number_duplicates == "14_1" ~ 13,
                                      key == "uuid:45fcea23-9ec3-4b58-915a-75ee31a08402" & trap_number_duplicates == "14_2" ~ 14, #Two 14's but 13 is missing
                                      key == "uuid:45fcea23-9ec3-4b58-915a-75ee31a08402" & trap_number_duplicates == "20_2" ~ 99,
                                      key == "uuid:479fc079-636b-47ae-812a-bd6b8abeabcb" & trap_number_duplicates == "265_1" ~ 264,
                                      key == "uuid:479fc079-636b-47ae-812a-bd6b8abeabcb" & trap_number_duplicates == "265_2" ~ 265,
                                      key == "uuid:4b3738fd-e6d1-4d55-bc77-fa52074287b8" & trap_number_duplicates == "141_2" ~ 101, #Two 141's but 101 is missing
                                      key == "uuid:4b3738fd-e6d1-4d55-bc77-fa52074287b8" & trap_number_duplicates == "146_1" ~ 145, 
                                      key == "uuid:4b3738fd-e6d1-4d55-bc77-fa52074287b8" & trap_number_duplicates == "146_2" ~ 146, #Two 146's but 145 is missing
                                      key == "uuid:4eab9b5f-1235-4e78-b7aa-f363e8afbb98" & trap_number_duplicates == "237_1" ~ 236, #Two 237's but 236 is missing
                                      key == "uuid:4eab9b5f-1235-4e78-b7aa-f363e8afbb98" & trap_number_duplicates == "237_2" ~ 237,
                                      key == "uuid:5888a838-d656-40af-bd53-cd065ad4aaf8" & trap_number_duplicates == "232_2" ~ 233,
                                      key == "uuid:5888a838-d656-40af-bd53-cd065ad4aaf8" & trap_number_duplicates == "243_2" ~ 245,
                                      key == "uuid:5bedb731-5e12-49e2-8aef-fca6a1647690" & trap_number_duplicates == "85_1" ~ 55,
                                      key == "uuid:5bedb731-5e12-49e2-8aef-fca6a1647690" & trap_number_duplicates == "85_2" ~ 85,
                                      key == "uuid:6d61a211-7987-4e94-a297-2ff4b7248bf6" & trap_number_duplicates == "283_2" ~ 293,
                                      key == "uuid:6fa6c154-b5e1-40cc-9db2-f8944bdd9280" & trap_number_duplicates == "17_2" ~ 18, #Two 17's but 18 is missing 
                                      key == "uuid:6fa6c154-b5e1-40cc-9db2-f8944bdd9280" & trap_number_duplicates == "29_1" ~ 28, #Two 29's but 28 is missing 
                                      key == "uuid:6fa6c154-b5e1-40cc-9db2-f8944bdd9280" & trap_number_duplicates == "29_2" ~ 29,
                                      key == "uuid:6fa6c154-b5e1-40cc-9db2-f8944bdd9280" & trap_number_duplicates == "35_2" ~ 38, #Two 35's but 38 is missing 
                                      key == "uuid:7d2cbdd3-c9fe-4d6b-af1b-933d389ebff5" & trap_number_duplicates == "173_2" ~ 174, #Two 17's but 18 is missing 
                                      key == "uuid:89d746e8-8093-41a3-b4e1-547dc27e04a1" & trap_number_duplicates == "221_1" ~ 220, #Two 221's but 220 is missing 
                                      key == "uuid:89d746e8-8093-41a3-b4e1-547dc27e04a1" & trap_number_duplicates == "221_2" ~ 221,
                                      key == "uuid:96dac5d1-d118-48d4-a7fd-4cfaff85b6c9" & trap_number_duplicates == "176_1" ~ 175, #Two 176's but 175 is missing 
                                      key == "uuid:96dac5d1-d118-48d4-a7fd-4cfaff85b6c9" & trap_number_duplicates == "176_2" ~ 176,
                                      key == "uuid:9b3b8634-4f61-443a-b160-655e8bfbc5f1" & trap_number_duplicates == "237_2" ~ 245,
                                      key == "uuid:ab22cce9-93ae-49ce-a0e7-93a0ab784860" & trap_number_duplicates == "203_1" ~ 202,
                                      key == "uuid:ab22cce9-93ae-49ce-a0e7-93a0ab784860" & trap_number_duplicates == "203_2" ~ 203,
                                      key == "uuid:b9192d3f-efa5-40f8-93b3-d1419fab39ae" & trap_number_duplicates == "267_2" ~ 270,
                                      key == "uuid:baef336e-bb76-44f8-8829-78fe32592009" & trap_number_duplicates == "340_2" ~ 343,
                                      key == "uuid:c1537883-b1c5-495e-8389-17d6abe50bb7" & trap_number_duplicates == "160_2" ~ 170, #Two 160's but 170 is missing 
                                      key == "uuid:c1537883-b1c5-495e-8389-17d6abe50bb7" & trap_number_duplicates == "187_1" ~ 179,
                                      key == "uuid:c1537883-b1c5-495e-8389-17d6abe50bb7" & trap_number_duplicates == "187_2" ~ 187,
                                      key == "uuid:c81ed3db-e7b3-45d6-a38b-0c542500483d" & trap_number_duplicates == "294_1" ~ 293,
                                      key == "uuid:c81ed3db-e7b3-45d6-a38b-0c542500483d" & trap_number_duplicates == "294_2" ~ 294,
                                      key == "uuid:c875b5a5-157b-49eb-a52b-f2c1a291eb7c" & trap_number_duplicates == "151_1" ~ 150, 
                                      key == "uuid:c875b5a5-157b-49eb-a52b-f2c1a291eb7c" & trap_number_duplicates == "151_2" ~ 151, #Two 151's but 150 is missing 
                                      key == "uuid:d31f48a6-12a5-491a-8d98-d9b1ebdd6bbb" & trap_number_duplicates == "249_1" ~ 248,
                                      key == "uuid:d31f48a6-12a5-491a-8d98-d9b1ebdd6bbb" & trap_number_duplicates == "249_2" ~ 249,
                                      key == "uuid:d31f48a6-12a5-491a-8d98-d9b1ebdd6bbb" & trap_number_duplicates == "272_2" ~ 273,
                                      key == "uuid:e3dd03fa-9fdc-44e0-a4a9-f4d9471b9b95" & trap_number_duplicates == "63_1" ~ 62,
                                      key == "uuid:e3dd03fa-9fdc-44e0-a4a9-f4d9471b9b95" & trap_number_duplicates == "63_2" ~ 63,
                                      key == "uuid:f1c393d6-c48f-472b-9838-029004553d45" & trap_number_duplicates == "296_2" ~ 297,
                                      key == "uuid:fb8b9501-30d9-48ef-b2b2-4cadbd643ce2" & trap_number_duplicates == "262_2" ~ 343, # No rodents caught in these traps, allocate them unused numbers
                                      key == "uuid:fb8b9501-30d9-48ef-b2b2-4cadbd643ce2" & trap_number_duplicates == "263_2" ~ 344,
                                      key == "uuid:fb8b9501-30d9-48ef-b2b2-4cadbd643ce2" & trap_number_duplicates == "264_2" ~ 345,
                                      key == "uuid:fb8b9501-30d9-48ef-b2b2-4cadbd643ce2" & trap_number_duplicates == "265_2" ~ 346,
                                      key == "uuid:fb8b9501-30d9-48ef-b2b2-4cadbd643ce2" & trap_number_duplicates == "266_2" ~ 347,
                                      key == "uuid:5888a838-d656-40af-bd53-cd065ad4aaf8" & trap_number_duplicates == "2002_1" ~ 202,
                                      key == "uuid:ba6bf1ba-15bc-43cc-a605-61146a99c69e" & trap_number_duplicates == "117979_1" ~ 196,
                                      key == "uuid:cf73f54e-e526-489a-be87-5a557ad3d254" & trap_number_duplicates == "51_2" ~ 52,
                                      key == "uuid:cf73f54e-e526-489a-be87-5a557ad3d254" & trap_number_duplicates == "59_2" ~ 62,
                                      key == "uuid:0be52388-4ea0-46f0-ab9c-11bb6b539a7a" & trap_number_duplicates == "303_2" ~ 297,
                                      key == "uuid:0be52388-4ea0-46f0-ab9c-11bb6b539a7a" & trap_number_duplicates == "4_1" ~ 344,
                                      str_detect(trap_number_duplicates, "_") ~ trap_number)) %>%
    drop_na(all_of(c("lon_dec", "lat_dec", "trap_number"))) %>% # remove those missing trap numbers and coordinates, it is likely they miscounted the number of entries required
    select(-trap_number_duplicates)
  
  fix_grids <- fix_trap_numbers %>%
    mutate(fix_grid_number = dt_case_when(site_use == "Forest" & village == "seilama" & visit == 3 & grid_number == "4" ~ 5,
                                          village == "lalehun" & visit == "3" & as.numeric(trap_number) < 99 & as.numeric(trap_number) >= 50 ~ 3,
                                          village == "lambayama" & visit == "4" & grid_number == "1" ~ 4,
                                          village == "lambayama" & visit == "4" & grid_number == "2" ~ 1,
                                          village == "lambayama" & visit == "4" & grid_number == "3" ~ 2,
                                          village == "lambayama" & visit == "4" & grid_number == "4" ~ 3,
                                          village == "lambayama" & visit == "4" & trap_number %in% c(148:196) ~ 4,
                                          village == "lambayama" & visit == "6" & trap_number %in% c(148:196) ~ 4,
                                          village == "lalehun" & visit == "9" & trap_number %in% c(148:196) ~ 5,
                                          village == "lalehun" & visit == "9" & trap_number %in% c(197:245) ~ 4,
                                          village == "seilama" & visit == "9" & trap_number %in% c(148:196) ~ 4,
                                          site_use == "village_inside" ~ 7,
                                          TRUE ~ grid_number)) %>%
    mutate(grid_number = coalesce(fix_grid_number, grid_number)) %>%
    select(-fix_grid_number)
  
  fix_locations <- fix_grids %>%
    mutate(lon_degree = 11,
           lat_degree = case_when(village == "lambayama" ~ 7,
                                  village == "baiama" ~ 7,
                                  TRUE ~ 8),
           swapped_lat = lat_dec,
           swapped_lon = lon_dec,
           lon_dec = dt_case_when(
             village == "lalehun" & visit == 3 & trap_number == 204 ~ 4.802,
             village == "lalehun" & visit == 3 & lon_dec >= 11 ~ (lon_dec - 11) * 100,
             village == "lalehun" & visit == 3 & trap_number == 35 ~ 4.7621,
             
             village == "lalehun" & visit == 4 & trap_number == 1 ~ 4.7653,
             village == "lalehun" & visit == 4 & trap_number == 45 ~ 4.7544,
             village == "lalehun" & visit == 4 & trap_number == 27 ~ 4.7653,
             village == "lalehun" & visit == 4 & trap_number == 32 ~ 4.7602,
             village == "lalehun" & visit == 4 & trap_number == 84 ~ 4.7430,
             village == "lalehun" & visit == 4 & trap_number == 124 ~ 4.830,
             village == "lalehun" & visit == 4 & trap_number == 144 ~ 4.826,
             village == "lalehun" & visit == 4 & trap_number == 174 ~ 4.686,
             village == "lalehun" & visit == 4 & trap_number == 176 ~ 4.684,
             village == "lalehun" & visit == 4 & trap_number == 187 ~ 4.6902,
             
             village == "lalehun" & visit == 4 & lat_dec >=4 & lat_dec <= 5 ~ swapped_lat,
             village == "lalehun" & visit == 4 & lat_dec > 11 ~ (swapped_lat - 11) * 100,
             
             village == "lalehun" & visit == 5 & trap_number == 176  ~ 4.6840,
             village == "lalehun" & visit == 5 & trap_number == 175  ~ 4.6857,
             village == "lalehun" & visit == 5 & trap_number == 174  ~ 4.6868,
             village == "lalehun" & visit == 5 & trap_number == 161  ~ 4.6895,
             village == "lalehun" & visit == 5 & trap_number == 94  ~ 4.7344,
             village == "lalehun" & visit == 5 & trap_number == 34  ~ 4.7646,
             village == "lalehun" & visit == 5 & trap_number == 187  ~ 4.6902,
             village == "lalehun" & visit == 5 & trap_number == 86  ~ 4.7399,
             village == "lalehun" & visit == 5 & trap_number == 186  ~ 4.6925,
             
             village == "lalehun" & visit == 5 & grid_number %in% c(5, 4) ~ (swapped_lat - 11) * 100,
             
             village == "lalehun" & visit == 6 & trap_number == 54  ~ 4.770,
             village == "lalehun" & visit == 6 & trap_number == 95  ~ 4.737,
             village == "lalehun" & visit == 6 & trap_number == 246  ~ 4.7929,
             
             village == "lalehun" & visit == 6 & grid_number %in% c(2, 3, 4, 5) ~ (swapped_lat - 11) * 100,
             
             village == "lalehun" & visit == 6 & grid_number== 7 & trap_number %in% c(4, 311, 312, 313) ~ swapped_lat,
             
             village == "lalehun" & visit == 7 & trap_number == 73  ~ 4.758,
             village == "lalehun" & visit == 7 & trap_number == 92  ~ 4.743,
             village == "lalehun" & visit == 7 & trap_number == 97  ~ 4.7370,
             village == "lalehun" & visit == 7 & trap_number == 121  ~ 4.821,
             village == "lalehun" & visit == 7 & trap_number == 131  ~ 4.837,
             village == "lalehun" & visit == 7 & trap_number == 148  ~ 4.6834,
             village == "lalehun" & visit == 7 & trap_number == 151  ~ 4.6925,
             village == "lalehun" & visit == 7 & trap_number == 221  ~ 4.8146,
             village == "lalehun" & visit == 7 & trap_number == 246  ~ 4.7929,
             
             village == "lalehun" & visit == 7 & grid_number %in% c(2, 3, 4, 5) ~ (swapped_lat - 11) * 100,
             
             village == "lalehun" & visit == 8 & trap_number == 26  ~ 4.7623,
             village == "lalehun" & visit == 8 & trap_number == 43  ~ 4.7518,
             village == "lalehun" & visit == 8 & trap_number == 70  ~ 4.762,
             village == "lalehun" & visit == 8 & trap_number == 282  ~ 4.7708,
             
             village == "lalehun" & visit == 8 & grid_number %in% c(2) ~ (swapped_lat - 11) * 100,
             
             village == "lalehun" & visit == 9 & trap_number == 38  ~ 4.755,
             village == "lalehun" & visit == 9 & trap_number == 50  ~ 4.772,
             village == "lalehun" & visit == 9 & trap_number == 51  ~ 4.773,
             village == "lalehun" & visit == 9 & trap_number == 98  ~ 4.73,
             village == "lalehun" & visit == 9 & trap_number == 103  ~ 4.8130,
             village == "lalehun" & visit == 9 & trap_number == 113  ~ 4.817,
             village == "lalehun" & visit == 9 & trap_number == 239  ~ 4.6901,
             village == "lalehun" & visit == 9 & trap_number == 247  ~ 4.8030,
             
             village == "lalehun" & visit == 9 & grid_number %in% c(2, 3, 4, 5) ~ (swapped_lat - 11) * 100,
             
             village == "lalehun" & visit == 10 & trap_number == 114  ~ 4.8188,
             village == "lalehun" & visit == 10 & trap_number == 116  ~ 4.821,
             
             village == "lalehun" & visit == 10 & trap_number %in% c(54:59, 62:98) ~ swapped_lat,
             village == "lalehun" & visit == 10 & trap_number %in% c(50:53, 60:61)  ~ (swapped_lat - 11) * 100,
             
             village == "lalehun" & visit == 10 & grid_number %in% c(3, 4, 5) ~ (swapped_lat - 11) * 100,
             
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
             
             village == "seilama" & visit == 5 & trap_number == 31 ~ 11.5493,
             village == "seilama" & visit == 5 & trap_number == 71 ~ 11.65,
             village == "seilama" & visit == 5 & trap_number == 65 ~ 11.651,
             village == "seilama" & visit == 5 & trap_number == 89 ~ 11.657,
             village == "seilama" & visit == 5 & trap_number == 161 ~ 11.6761,
             village == "seilama" & visit == 5 & trap_number == 192 ~ 11.6238,
             village == "seilama" & visit == 5 & trap_number == 223 ~ 11.862,
             
             village == "seilama" & visit == 6 & trap_number == 1 ~ 11.531,
             village == "seilama" & visit == 6 & trap_number == 65 ~ 11.651,
             village == "seilama" & visit == 6 & trap_number == 89 ~ 11.657,
             village == "seilama" & visit == 6 & trap_number == 223 ~ 11.862,
             
             village == "seilama" & visit == 7 & trap_number == 1 ~ 11.5261,
             village == "seilama" & visit == 7 & trap_number == 31 ~ 11.5484,
             village == "seilama" & visit == 7 & trap_number == 66 ~ 11.658,
             village == "seilama" & visit == 7 & trap_number == 118 ~ 11.671,
             village == "seilama" & visit == 7 & trap_number == 193 ~ 11.627,
             village == "seilama" & visit == 7 & trap_number == 197 ~ 11.8862,
             
             village == "seilama" & visit == 7 & grid_number %in% c(2, 3, 4) ~ (swapped_lat - 11) * 100,
             
             village == "seilama" & visit == 8 & trap_number == 26 ~ 11.5406,
             village == "seilama" & visit == 8 & trap_number == 141 ~ 11.674,
             village == "seilama" & visit == 8 & trap_number == 184 ~ 11.655,
             village == "seilama" & visit == 8 & trap_number == 285 ~ 11.6275,
             
             village == "seilama" & visit == 8 & grid_number %in% c(2, 4, 5) ~ (swapped_lat - 11) * 100,
             
             village == "seilama" & visit == 9 & trap_number == 11 ~ 11.532,
             village == "seilama" & visit == 9 & trap_number == 59 ~ 11.655,
             village == "seilama" & visit == 9 & trap_number == 63 ~ 11.656,
             village == "seilama" & visit == 9 & trap_number == 264 ~ 11.598,
             village == "seilama" & visit == 9 & trap_number == 279 ~ 11.630,
             
             village == "seilama" & visit == 9 & grid_number %in% c(3, 4, 5) ~ (swapped_lat - 11) * 100,
             
             village == "seilama" & visit == 10 & trap_number == 26 ~ 11.5464,
             village == "seilama" & visit == 10 & trap_number == 65 ~ 11.6524,
             village == "seilama" & visit == 10 & trap_number == 90 ~ 11.6575,
             village == "seilama" & visit == 10 & trap_number == 100 ~ 11.692,
             village == "seilama" & visit == 10 & trap_number == 112 ~ 11.701,
             village == "seilama" & visit == 10 & trap_number == 183 ~ 11.693,
             village == "seilama" & visit == 10 & trap_number == 211 ~ 11.919,
             village == "seilama" & visit == 10 & trap_number == 230 ~ 11.901,
             village == "seilama" & visit == 10 & trap_number == 255 ~ 11.6356,
             
             village == "seilama" & visit == 10 & grid_number %in% c(3, 4, 5) ~ (swapped_lat - 11) * 100,
             
             village == "baiama" & visit == 3 & trap_number %in% c(2, 3, 4, 5, 6, 7) ~ swapped_lat,
             village == "baiama" & visit == 3 & trap_number == 195 ~ 15.9879,
             
             village == "baiama" & visit == 4 & trap_number == 195 ~ 15.975,
             village == "baiama" & visit == 4 & trap_number == 26 ~ 15.017,
             village == "baiama" & visit == 4 & trap_number == 28 ~ 15.019,
             village == "baiama" & visit == 4 & trap_number == 109 ~ 15.736,
             village == "baiama" & visit == 4 & trap_number == 111 ~ 15.724,
             village == "baiama" & visit == 4 & trap_number == 121 ~ 15.726,
             village == "baiama" & visit == 4 & trap_number == 122 ~ 15.750,
             village == "baiama" & visit == 4 & trap_number == 128 ~ 15.756,
             village == "baiama" & visit == 4 & trap_number == 130 ~ 15.733,
             village == "baiama" & visit == 4 & trap_number == 140 ~ 15.7530,
             
             village == "baiama" & visit == 4 & grid_number %in% c(4, 5, 6, 7) ~ swapped_lat,
             village == "baiama" & visit == 4 & grid_number %in% c(1, 2, 3) ~ (swapped_lat - 11) * 100,
             
             village == "baiama" & visit == 5 & trap_number == 231 ~ 16.0647,
             
             village == "baiama" & visit == 5 & grid_number %in% c(1, 2, 3) ~ (swapped_lat - 11) * 100,
             
             village == "baiama" & visit == 6 & trap_number == 50 ~ 15.245,
             village == "baiama" & visit == 6 & trap_number == 55 ~ 15.242,
             village == "baiama" & visit == 6 & trap_number == 98 ~ 15.234,
             
             village == "baiama" & visit == 6 & grid_number %in% c(2, 3) ~ (swapped_lat - 11) * 100,
             
             village == "baiama" & visit == 7 & trap_number == 170 ~ 15.9897,
             village == "baiama" & visit == 7 & trap_number == 203 ~ 16.0730,
             village == "baiama" & visit == 7 & trap_number == 189 ~ 15.9761,
             
             village == "baiama" & visit == 7 & grid_number %in% c(1, 2, 3) ~ (swapped_lat - 11) * 100,
             
             village == "baiama" & visit == 8 & trap_number == 51 ~ 15.2327,
             village == "baiama" & visit == 8 & trap_number == 94 ~ 15.2182,
             village == "baiama" & visit == 8 & trap_number == 15 ~ 15.009,
             
             village == "baiama" & visit == 8 & grid_number %in% c(1) ~ (swapped_lat - 11) * 100,
             
             village == "baiama" & visit == 9 ~ (swapped_lat - 11) * 100,
             
             village == "baiama" & visit == 10 & trap_number == 165 ~ 16.030,
             
             village == "baiama" & visit == 10 & grid_number %in% c(1, 2, 3) ~ (swapped_lat - 11) * 100,
             
             village == "lambayama" & visit == 3 & trap_number == 50 ~ 11.6820,
             
             village == "lambayama" & visit == 4 & trap_number == 50 ~ 11.6765,
             village == "lambayama" & visit == 4 & trap_number == 156 ~ 11.8062,
             village == "lambayama" & visit == 4 & trap_number == 165 ~ 11.8136,
             village == "lambayama" & visit == 4 & trap_number == 119 ~ 11.550,
             village == "lambayama" & visit == 4 & trap_number == 215 ~ 11.7934,
             
             village == "lambayama" & visit == 4 & grid_number %in% c(1, 2, 3, 4, 5, 6, 7) ~ swapped_lat,
             
             village == "lambayama" & visit == 5 & trap_number == 69 ~ 11.678,
             village == "lambayama" & visit == 5 & trap_number == 226 ~ 11.8027,
             village == "lambayama" & visit == 5 & trap_number == 237 ~ 11.8194,
             
             village == "lambayama" & visit == 5 & grid_number %in% c(1, 2, 3) ~ (swapped_lat - 11) * 100,
             
             village == "lambayama" & visit == 6 & trap_number == 76 ~ 11.675,
             village == "lambayama" & visit == 6 & trap_number == 62 ~ 11.678,
             village == "lambayama" & visit == 6 & trap_number == 94 ~ 11.680,
             village == "lambayama" & visit == 6 & trap_number == 146 ~ 11.533,
             village == "lambayama" & visit == 6 & trap_number == 161 ~ 11.807,
             village == "lambayama" & visit == 6 & trap_number == 176 ~ 11.807,
             village == "lambayama" & visit == 6 & trap_number == 184 ~ 11.816,
             village == "lambayama" & visit == 6 & grid_number == 7 & trap_number %in% c(303, 304, 305, 306) ~ 11.809,
             village == "lambayama" & visit == 6 & trap_number == 173 ~ 11.806,
             village == "lambayama" & visit == 6 & grid_number %in% c(1, 2, 3) ~ (swapped_lat - 11) * 100,
             
             village == "lambayama" & visit == 7 & trap_number == 114 ~ 11.55,
             village == "lambayama" & visit == 7 & trap_number == 155 ~ 11.8061,
             village == "lambayama" & visit == 7 & trap_number == 186 ~ 11.8063,
             village == "lambayama" & visit == 7 & trap_number == 228 ~ 11.7889,
             village == "lambayama" & visit == 7 & lon_dec == 50.9729 ~ 11.7979,
             village == "lambayama" & visit == 7 & trap_number == 233 ~ 11.7841,
             
             village == "lambayama" & visit == 7 & grid_number %in% c(1, 2, 3) ~ (swapped_lat - 11) * 100,
             
             village == "lambayama" & visit == 9 & trap_number == 197 ~ 11.78,
             
             village == "lambayama" & visit == 9 & grid_number %in% c(1, 2, 3) ~ (swapped_lat - 11) * 100,
             village == "lambayama" & visit == 9 & grid_number %in% c(7) & trap_number %in% c(197:245)  ~ (swapped_lat - 11) * 100,
             village == "lambayama" & visit == 9 & grid_number %in% c(7) & trap_number %in% c(246:295)  ~ lon_dec,
             
             village == "lambayama" & visit == 10 & trap_number == 7 ~ 11.741,
             
             village == "lambayama" & visit == 10 & grid_number %in% c(1, 2, 3, 4, 7) ~ (swapped_lat - 11) * 100,
             
             village == "bambawo" & visit == 1 & trap_number == 191 ~ 8.5905,
             
             TRUE ~ lon_dec),
           lat_dec = dt_case_when(
             
             village == "lalehun" & visit == 3 & trap_number == 159 ~ 11.925,
             village == "lalehun" & visit == 3 & trap_number == 188 ~ 11.915,
             village == "lalehun" & visit == 3 & trap_number == 239 ~ 11.633,
             village == "lalehun" & visit == 3 & trap_number == 218 ~ 11.628,
             village == "lalehun" & visit == 3 & trap_number == 10 ~ 11.791,
             village == "lalehun" & visit == 3 & trap_number == 35 ~ 11.7751,
             village == "lalehun" & visit == 3 & trap_number == 90 ~ 11.9191,
             
             village == "lalehun" & visit == 3 & lat_dec <= 9 ~ (lat_dec-8)*100,
             
             village == "lalehun" & visit == 4 & trap_number == 27 ~ 11.7797,
             village == "lalehun" & visit == 4 & trap_number == 45 ~ 11.7787,
             village == "lalehun" & visit == 4 & trap_number == 1 ~ 11.8006,
             village == "lalehun" & visit == 4 & trap_number == 124 ~ 11.938,
             village == "lalehun" & visit == 4 & trap_number == 142 ~ 11.962,
             
             village == "lalehun" & visit == 4 & lat_dec <= 12 & lat_dec >= 8 ~ (swapped_lon-8)*100,
             village == "lalehun" & visit == 4 & lat_dec <= 5 ~ swapped_lon,
             
             village == "lalehun" & visit == 5 & trap_number == 9 ~ 11.7973,
             village == "lalehun" & visit == 5 & trap_number == 16 ~ 11.7958,
             village == "lalehun" & visit == 5 & trap_number == 108 ~ 11.9323,
             village == "lalehun" & visit == 5 & trap_number == 115 ~ 11.9342,
             village == "lalehun" & visit == 5 & trap_number == 116 ~ 11.9342,
             village == "lalehun" & visit == 5 & trap_number == 123 ~ 11.9346,
             village == "lalehun" & visit == 5 & trap_number == 130 ~ 11.9318,
             village == "lalehun" & visit == 5 & trap_number == 131 ~ 11.9342,
             village == "lalehun" & visit == 5 & trap_number == 167 ~ 11.6566,
             village == "lalehun" & visit == 5 & trap_number == 180 ~ 11.6639,
             village == "lalehun" & visit == 5 & trap_number == 275 ~ 11.8632,
             
             village == "lalehun" & visit == 5 & grid_number %in% c(4, 5) ~ (swapped_lon-8)*100,
             
             village == "lalehun" & visit == 6 & grid_number== 7 & trap_number %in% c(4, 311, 312, 313) ~ elevation,
             village == "lalehun" & visit == 6 & grid_number== 7 & trap_number %in% c(303, 304, 305, 306) ~ 11.829,
             village == "lalehun" & visit == 6 & grid_number== 1 & trap_number == 12 ~ 11.784,
             village == "lalehun" & visit == 6 & trap_number == 54 ~ 11.920,
             village == "lalehun" & visit == 6 & trap_number == 44 ~ 11.785,
             village == "lalehun" & visit == 6 & grid_number== 4 & trap_number == 191 ~ (swapped_lon-88)*100,
             
             village == "lalehun" & visit == 6 & grid_number %in% c(2, 3, 4, 5) ~ (swapped_lon-8)*100,
             
             village == "lalehun" & visit == 7 & trap_number == 10 ~ 11.7955,
             village == "lalehun" & visit == 7 & trap_number == 92 ~ 11.925,
             village == "lalehun" & visit == 7 & trap_number == 140 ~ 11.946,
             village == "lalehun" & visit == 7 & trap_number == 144 ~ 11.961,
             village == "lalehun" & visit == 7 & trap_number == 148  ~ 11.6537,
             village == "lalehun" & visit == 7 & trap_number == 121 ~ 11.931,
             village == "lalehun" & visit == 7 & trap_number == 151  ~ 11.64,
             
             village == "lalehun" & visit == 7 & grid_number %in% c(2, 3, 4, 5) ~ (swapped_lon-8)*100,
             
             village == "lalehun" & visit == 8 & trap_number == 43  ~ 11.7833,
             
             village == "lalehun" & visit == 8 & grid_number %in% c(2) ~ (swapped_lon-8)*100,
             
             village == "lalehun" & visit == 9 & trap_number == 75  ~ 11.925,
             village == "lalehun" & visit == 9 & trap_number == 86  ~ 11.932,
             village == "lalehun" & visit == 9 & trap_number == 113  ~ 11.940,
             village == "lalehun" & visit == 9 & trap_number == 266  ~ 11.842,
             
             village == "lalehun" & visit == 9 & grid_number %in% c(2, 3, 4, 5) ~ (swapped_lon-8)*100,
             
             village == "lalehun" & visit == 10 & trap_number == 114  ~ 11.9341,
             village == "lalehun" & visit == 10 & trap_number == 116  ~ 11.9373,
             village == "lalehun" & visit == 10 & trap_number == 247  ~ 11.847,
             village == "lalehun" & visit == 10 & trap_number == 273  ~ 11.867,
             village == "lalehun" & visit == 10 & trap_number == 277  ~ 11.871,
             
             village == "lalehun" & visit == 10 & trap_number %in% c(54:59, 62:98) ~ swapped_lon,
             village == "lalehun" & visit == 10 & trap_number %in% c(50:53, 60:61)  ~ (swapped_lon-8)*100,
             
             village == "lalehun" & visit == 10 & grid_number %in% c(3, 4, 5) ~ (swapped_lon-8)*100,
             
             village == "seilama" & visit == 3 & trap_number == 208 ~ 7.424,
             village == "seilama" & visit == 3 & trap_number == 203 ~ 7.412,
             village == "seilama" & visit == 3 & trap_number == 112 ~ 7.2447,
             village == "seilama" & visit == 3 & trap_number == 278 ~ 7.3694,
             village == "seilama" & visit == 3 & trap_number == 12 ~ 7.3343,
             
             village == "seilama" & visit == 3 & grid_number %in% c(2, 3, 4, 5) ~ (lat_dec-8) * 100,
             
             village == "seilama" & visit == 4 & trap_number == 54 ~ 7.3775,
             
             village == "seilama" & visit == 4 & grid_number %in% c(1, 6, 7) ~ swapped_lon,
             village == "seilama" & visit == 4 & grid_number %in% c(2, 3, 4) ~ (swapped_lon - 8) * 100,
             
             village == "seilama" & visit == 5 & trap_number == 124 ~ 7.4321,
             
             village == "seilama" & visit == 6 & grid_number == 1 & trap_number == 39 ~ 7.33,
             village == "seilama" & visit == 6 & trap_number == 247 ~ 7.323,
             village == "seilama" & visit == 6 & trap_number == 272 ~ 7.357,
             village == "seilama" & visit == 6 & trap_number == 278 ~ 7.338,
             village == "seilama" & visit == 6 & trap_number == 280 ~ 7.331,
             
             village == "seilama" & visit == 7 & trap_number == 1 ~ 7.3261,
             village == "seilama" & visit == 7 & trap_number == 107 ~ 7.4265,
             village == "seilama" & visit == 7 & trap_number == 150 ~ 7.248,
             village == "seilama" & visit == 7 & trap_number == 197 ~ 7.4233,
             village == "seilama" & visit == 7 & trap_number == 203 ~ 7.4146,
             village == "seilama" & visit == 7 & trap_number == 277 ~ 7.3592,
             
             village == "seilama" & visit == 7 & grid_number %in% c(2, 3, 4) ~ (swapped_lon - 8) * 100,
             
             village == "seilama" & visit == 8 & grid_number == 3 ~ lat_dec - 1,
             
             village == "seilama" & visit == 8 & grid_number %in% c(2, 4, 5) ~ (swapped_lon - 8) * 100,
             
             village == "seilama" & visit == 9 & trap_number == 25 ~ 7.329,
             village == "seilama" & visit == 9 & trap_number == 51 ~ 7.3731,
             village == "seilama" & visit == 9 & trap_number == 58 ~ 7.375,
             village == "seilama" & visit == 9 & trap_number == 68 ~ 7.386,
             village == "seilama" & visit == 9 & trap_number == 92 ~ 7.391,
             village == "seilama" & visit == 9 & trap_number == 141 ~ 7.424,
             village == "seilama" & visit == 9 & trap_number == 142 ~ 7.425,
             village == "seilama" & visit == 9 & trap_number == 143 ~ 7.427,
             village == "seilama" & visit == 9 & trap_number == 144 ~ 7.429,
             village == "seilama" & visit == 9 & trap_number == 145 ~ 7.430,
             village == "seilama" & visit == 9 & trap_number == 146 ~ 7.432,
             village == "seilama" & visit == 9 & trap_number == 147 ~ 7.434,
             village == "seilama" & visit == 9 & trap_number %in% c(299:302) ~ 7.331,
             
             village == "seilama" & visit == 9 & grid_number %in% c(3, 4, 5) ~ (swapped_lon - 8) * 100,
             
             village == "seilama" & visit == 10 & trap_number == 36 ~ 7.32332,
             village == "seilama" & visit == 10 & trap_number == 65 ~ 7.3772,
             village == "seilama" & visit == 10 & trap_number == 112 ~ 7.472,
             village == "seilama" & visit == 10 & trap_number == 183 ~ 7.275,
             village == "seilama" & visit == 10 & trap_number == 211 ~ 7.471,
             village == "seilama" & visit == 10 & trap_number == 266 ~ 7.3128,
             
             village == "seilama" & visit == 10 & grid_number %in% c(3, 4, 5) ~ (swapped_lon - 8) * 100,
             
             village == "baiama" & visit == 3 & trap_number %in% c(2, 3, 4, 5, 6, 7) ~ swapped_lon,
             
             village == "baiama" & visit == 4 & trap_number == 185 ~ 50.1830,
             
             village == "baiama" & visit == 4 & grid_number %in% c(4, 5, 6, 7) ~ swapped_lon,
             village == "baiama" & visit == 4 & grid_number %in% c(1, 2) ~ (swapped_lon- 7) * 100,
             village == "baiama" & visit == 4 & grid_number %in% c(3) ~ (swapped_lon - 8) * 100,
             
             village == "baiama" & visit == 5 & trap_number == 13 ~ 49.4961,
             village == "baiama" & visit == 5 & trap_number == 14 ~ 49.4945,
             village == "baiama" & visit == 5 & trap_number == 40 ~ 49.4741,
             village == "baiama" & visit == 5 & trap_number == 151 ~ 50.1842,
             village == "baiama" & visit == 5 & trap_number == 168 ~ 50.1906,
             village == "baiama" & visit == 5 & trap_number == 169 ~ 50.1907,
             village == "baiama" & visit == 5 & trap_number == 185 ~ 50.1909,
             village == "baiama" & visit == 5 & trap_number == 195 ~ 50.1972,
             village == "baiama" & visit == 5 & trap_number == 206 ~ 50.1659,
             village == "baiama" & visit == 5 & trap_number == 213 ~ 50.189,
             village == "baiama" & visit == 5 & trap_number == 216 ~ 50.1812,
             village == "baiama" & visit == 5 & trap_number %in% c(250:253) ~ 50.1832,
             
             village == "baiama" & visit == 5 & grid_number %in% c(1, 2, 3) ~ (swapped_lon - 7) * 100,
             
             village == "baiama" & visit == 6 & trap_number == 50 ~ 49.795,
             village == "baiama" & visit == 6 & grid_number == 1 & lat_dec == 49 ~ 49.472,
             village == "baiama" & visit == 6 & trap_number == 66 ~ 49.794,
             village == "baiama" & visit == 6 & trap_number == 147 ~ 49.835,
             village == "baiama" & visit == 6 & trap_number == 213 ~ 50.189,
             village == "baiama" & visit == 6 & trap_number == 238 ~ 50.168,
             
             village == "baiama" & visit == 6 & grid_number == 4 & trap_number %in% c(149, 152) ~ lat_dec/1000,
             
             village == "baiama" & visit == 6 & grid_number %in% c(2, 3) ~ (swapped_lon - 7) * 100,
             
             village == "baiama" & visit == 7 & trap_number == 242 ~ 50.2229,
             
             village == "baiama" & visit == 7 & grid_number %in% c(1, 2, 3) ~ (swapped_lon - 7) * 100,
             
             village == "baiama" & visit == 8 & trap_number == 79 ~ 49.7945,
             
             village == "baiama" & visit == 8 & grid_number %in% c(1) ~ (swapped_lon - 7) * 100,
             
             village == "baiama" & visit == 9 ~ (swapped_lon - 7) * 100,
             
             village == "baiama" & visit == 10 & grid_number %in% c(1, 2, 3) ~ (swapped_lon - 7) * 100,
             
             village == "lambayama" & visit == 3 & trap_number == 50 ~ 51.0729,
             village == "lambayama" & visit == 3 & trap_number == 153 ~ 50.9851,
             
             village == "lambayama" & visit == 4 & trap_number == 1 ~ 50.9861,
             village == "lambayama" & visit == 4 & trap_number == 50 ~ 51.0824,
             village == "lambayama" & visit == 4 & trap_number == 83 ~ 51.0624,
             village == "lambayama" & visit == 4 & trap_number == 165 ~ 50.9904,
             
             village == "lambayama" & visit == 4 & grid_number %in% c(1, 2, 3, 4, 5, 6, 7) ~ swapped_lon,
             
             village == "lambayama" & visit == 5 & trap_number == 154 ~ 51.0014,
             village == "lambayama" & visit == 5 & trap_number %in% c(271:274) ~ 51.0953,
             village == "lambayama" & visit == 5 & trap_number == 214 ~ 51.0079,
             village == "lambayama" & visit == 5 & trap_number == 216 ~ 50.9945,
             village == "lambayama" & visit == 5 & trap_number == 217 ~ 50.9969,
             
             village == "lambayama" & visit == 5 & grid_number %in% c(1, 2, 3) ~ (swapped_lon - 7) * 100,
             
             village == "lambayama" & visit == 6 & trap_number == 3 ~ 51.001,
             village == "lambayama" & visit == 6 & trap_number == 4 ~ 51.002,
             village == "lambayama" & visit == 6 & trap_number == 25 ~ 51.055,
             village == "lambayama" & visit == 6 & trap_number == 50 ~ 51.0804,
             village == "lambayama" & visit == 6 & trap_number == 92 ~ 51.081,
             village == "lambayama" & visit == 6 & trap_number == 94 ~ 51.078,
             village == "lambayama" & visit == 6 & trap_number == 272 ~ 51.000,
             village == "lambayama" & visit == 6 & trap_number == 263 ~ 51.004,
             village == "lambayama" & visit == 6 & trap_number == 282 ~ 51.005,
             
             village == "lambayama" & visit == 6 & trap_number %in% c(327:330) ~ 51.045,
             
             village == "lambayama" & visit == 6 & grid_number %in% c(1, 2, 3) ~ (swapped_lon - 7) * 100,
             
             village == "lambayama" & visit == 7 & trap_number == 114 ~ 50.950,
             village == "lambayama" & visit == 7 & trap_number == 154 ~ 51.0014,
             village == "lambayama" & visit == 7 & trap_number == 241 ~ 51.0047,
             village == "lambayama" & visit == 7 & trap_number == 196 ~ 50.9790,
             village == "lambayama" & visit == 7 & trap_number == 216 ~ 51.0172,
             village == "lambayama" & visit == 7 & trap_number == 228 ~ 51.0206,
             village == "lambayama" & visit == 7 & trap_number == 231 ~ 50.9998,
             village == "lambayama" & visit == 7 & trap_number == 232 ~ 50.9962,
             village == "lambayama" & visit == 7 & trap_number == 233 ~ 50.9933,
             village == "lambayama" & visit == 7 & trap_number == 234 ~ 50.9948,
             village == "lambayama" & visit == 7 & trap_number == 235 ~ 50.9963,
             village == "lambayama" & visit == 7 & trap_number == 236 ~ 50.9963,
             village == "lambayama" & visit == 7 & trap_number == 237 ~ 50.9995,
             village == "lambayama" & visit == 7 & lat_dec == 51.9980 ~ 50.9980,
             
             village == "lambayama" & visit == 7 & grid_number %in% c(1, 2, 3) ~ (swapped_lon - 7) * 100,
             
             village == "lambayama" & visit == 8 & trap_number == 228 ~ 50.9995,
             
             village == "lambayama" & visit == 9 & trap_number %in% c(58, 59, 56, 60) ~ 51.054,
             village == "lambayama" & visit == 9 & trap_number == 62 ~ 51.055,
             village == "lambayama" & visit == 9 & trap_number == 63 ~ 51.056,
             village == "lambayama" & visit == 9 & trap_number == 65 ~ 51.058,
             village == "lambayama" & visit == 9 & trap_number %in% c(64, 66, 67) ~ 51.059,
             village == "lambayama" & visit == 9 & trap_number %in% c(68, 69) ~ 51.06,
             village == "lambayama" & visit == 9 & trap_number %in% c(70, 71, 72) ~ 51.061,
             village == "lambayama" & visit == 9 & trap_number %in% c(73) ~ 51.062,
             village == "lambayama" & visit == 9 & trap_number %in% c(74, 75, 78) ~ 51.063,
             village == "lambayama" & visit == 9 & trap_number %in% c(76, 77) ~ 51.064,
             village == "lambayama" & visit == 9 & trap_number %in% c(79, 80, 81, 82) ~ 51.065,
             village == "lambayama" & visit == 9 & trap_number %in% c(83, 85, 86, 87) ~ 51.066,
             village == "lambayama" & visit == 9 & trap_number %in% c(84, 88, 89, 96) ~ 51.067,
             village == "lambayama" & visit == 9 & trap_number %in% c(90) ~ 51.068,
             village == "lambayama" & visit == 9 & trap_number %in% c(91, 92, 93, 94, 95) ~ 51.069,
             village == "lambayama" & visit == 9 & trap_number %in% c(96, 98) ~ 51.07,
             village == "lambayama" & visit == 9 & trap_number %in% c(97) ~ 51.071,
             
             village == "lambayama" & visit == 9 & grid_number %in% c(1, 2, 3) ~ (swapped_lon - 7) * 100,
             village == "lambayama" & visit == 9 & grid_number %in% c(7) & trap_number %in% c(197:245)  ~ (swapped_lon - 7) * 100,
             village == "lambayama" & visit == 9 & grid_number %in% c(7) & trap_number %in% c(246:295) ~ lat_dec + 1,
             
             village == "lambayama" & visit == 10 & trap_number == 7 ~ 51.0097,
             village == "lambayama" & visit == 10 & trap_number %in% c(211, 218, 219, 220) ~ ((swapped_lon - 7) * 100) - 1,
             
             village == "lambayama" & visit == 10 & grid_number %in% c(1, 2, 3, 4, 7) ~ (swapped_lon - 7) * 100,
             
             village == "bambawo" & visit == 3 & trap_number == 53 ~ 0.5458,
             
             village == "seilama" & visit == 6 & trap_number == 207 ~ 7.423,
             
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
                                   habitat_type == "village_outside" & !grid_number %in% c(1, 2, 3, 4) ~ 7,
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
  
  mapview::mapview(st_as_sf(fix_locations, coords = c("lon", "lat"), crs = project_crs), z = "grid_number")

  trap_nos <- fix_locations %>%
    group_by(village, visit, grid_number, trap_number) %>% 
    mutate(tn = n()) %>%
    filter(tn > 1)
  
  message(cat(ifelse(nrow(trap_nos) == 0, "There are no duplicated trap numbers.",
                     paste(
                       paste0("There are ", nrow(trap_nos), " duplicated trap numbers:"),
                       paste0("They are trap numbers: ", knitr::combine_words(trap_nos$trap_number)),
                       sep = "\n"))))
  
  missing_trap_nos <- fix_locations %>%
    filter(is.na(trap_number))
  
  message(cat(ifelse(nrow(missing_trap_nos) == 0, "There are no uncorrected missing trap numbers.",
                     paste(
                       paste0("There are ", nrow(missing_trap_nos), " missing trap numbers:")))))
  
  harmonise_set_dates <- fix_locations %>%
    mutate(date_set = dt_case_when(village == "lalehun" & visit == 4 ~ ymd("2021-10-12"),
                                   village == "seilama" & visit == 4 ~ ymd("2021-10-16"),
                                   village == "baiama" & visit == 4 ~ ymd("2021-10-22"),
                                   village == "lalehun" & visit == 5 ~ ymd("2022-01-15"),
                                   village == "seilama" & visit == 5 ~ ymd("2022-01-19"),
                                   village == "baiama" & visit == 5 ~ ymd("2022-01-26"),
                                   village == "lambayama" & visit == 5 ~ ymd("2022-01-31"),
                                   village == "lalehun" & visit == 6 ~ ymd("2022-04-12"),
                                   village == "seilama" & visit == 6 ~ ymd("2022-04-17"),
                                   village == "baiama" & visit == 6 ~ ymd("2022-04-28"),
                                   village == "lambayama" & visit == 6 ~ ymd("2022-04-24"),
                                   village == "lalehun" & visit == 7 ~ ymd("2022-08-09"),
                                   village == "seilama" & visit == 7 ~ ymd("2022-08-05"),
                                   village == "baiama" & visit == 7 ~ ymd("2022-08-13"),
                                   village == "lambayama" & visit == 7 ~ ymd("2022-08-17"),
                                   village == "lalehun" & visit == 8 ~ ymd("2022-10-23"),
                                   village == "seilama" & visit == 8 ~ ymd("2022-11-06"),
                                   village == "baiama" & visit == 8 ~ ymd("2022-11-13"),
                                   village == "lambayama" & visit == 8 ~ ymd("2022-10-29"),
                                   village == "lalehun" & visit == 9 ~ ymd("2023-02-03"),
                                   village == "seilama" & visit == 9 ~ ymd("2023-02-05"),
                                   village == "baiama" & visit == 9 ~ ymd("2023-02-19"),
                                   village == "lambayama" & visit == 9 ~ ymd("2023-02-13"),
                                   village == "lalehun" & visit == 10 ~ ymd("2023-04-24"),
                                   TRUE ~ date_set))
  
  full_trap_locations <- harmonise_set_dates %>%
    group_by_all() %>%
    expand(trap_night = 1:4) %>%
    ungroup() %>%
    filter(!trap_number %in% trap_nos$trap_number) %>%
    mutate(elevation = case_when(elevation >= 2000 ~ elevation/10,
                                 elevation <= 100 ~ elevation*10,
                                 TRUE ~ elevation),
           date_set = date_set + (as.numeric(trap_night)-1),
           trap_uid = paste0(village, "_", visit, "_", trap_night, "_", grid_number, "_", trap_number)) %>%
    dplyr::select(-c(lat_dec, lat_DdM, lon_dec, lon_DdM))
  
  message("Traps expanded for 4 nights at each site.")

  coord_check <- full_trap_locations %>%
    distinct(village, visit, trap_number, .keep_all = T) %>%
    drop_na(lon, lat) %>%
    st_as_sf(coords = c("lon", "lat")) %>%
    st_set_crs(., value = 4326) %>%
    ungroup()
  
  return(output = list(full_trap_locations = full_trap_locations,
                       full_trap_locations_spatial = coord_check))
}
