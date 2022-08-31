clean_site_ODK <- function() {
  
  all_files <- list.files(here("data", "raw_odk", paste0("trap_sites", "_", Sys.Date())), full.names = T)
  
  if(identical(all_files, character(0))) all_files <- list.files(tail(sort(list.files(here("data", "raw_odk"), pattern = "trap_sites_", full.names = TRUE)), 1), full.names = TRUE)
  
  not_needed_vars <- c("other_village_name", "consecutive_traps", "meta-instanceID", "submitterID", "SubmitterName", "AttachmentsPresent",
                       "AttachmentsExpected", "Status", "ReviewState", "DeviceID", "Edits", "non_village-consecutive_traps", "non_village-starting_number",
                       "non_village-highest", "non_village-ending_number", "non_village-trap_count", "within_houses-consecutive_traps", "swapped_lat", "swapped_lon")
  
  trap_sites <- read_csv(all_files[4], show_col_types = FALSE) %>%
    filter(ReviewState != "rejected" | is.na(ReviewState)) %>%
    filter(!KEY %in% c("uuid:4161d048-299d-42b8-a758-480ba044cea9", "uuid:3db1d7ec-ca8c-407c-900a-b7d4e48c3d3f")) %>%
    mutate(SubmissionDate = ymd(as.Date(SubmissionDate)),
           form_entry = ymd(as.Date(form_entry)),
           other_village_name = case_when(other_village_name == "Lambeyama" ~ "Lambayama", # Correct the spelling of a village site
                                          other_village_name == "Lambiyama" ~ "Lambayama",
                                          TRUE ~ other_village_name),
           village_name = case_when(village_name != "other" ~ village_name,
                                    TRUE ~ tolower(as.character(other_village_name))),
           visit_number = case_when(month(form_entry) == 3 & year(form_entry) == 2021 & village_name %in% c("lalehun", "seilama") ~ 2,
                                    month(form_entry) == 3 & year(form_entry) == 2021 & village_name %in% c("lambayama", "bamabawo", "baiama") ~ 1,
                                    month(form_entry) >= 6 & month(form_entry) <= 7 & year(form_entry) == 2021 & village_name %in% c("lambayama", "bambawo", "baiama") ~ 1,
                                    month(form_entry) >= 6 & month(form_entry) <= 7 & year(form_entry) == 2021 & village_name %in% c("lalehun", "seilama") ~ 3,
                                    month(form_entry) >= 10 & month(form_entry) <= 11 & year(form_entry) == 2021 & village_name %in% c("lalehun", "seilama") ~ 4,
                                    month(form_entry) >= 10 & month(form_entry) <= 11 & year(form_entry) == 2021 & village_name %in% c("lambayama", "bambawo", "baiama") ~ 2,
                                    month(form_entry) <= 2 & year(form_entry) == 2022 & village_name %in% c("lalehun", "seilama") ~ 5,
                                    month(form_entry) <= 3 & year(form_entry) == 2022 & village_name %in% c("lambayama", "baiama") ~ 3,
                                    KEY == "uuid:1f75e3b2-478e-4b5d-a475-de5aaed6593c" ~ 7,
                                    month(form_entry) >= 4 & month(form_entry) <= 7 & year(form_entry) == 2022 & village_name %in% c("lalehun", "seilama") ~ 6,
                                    month(form_entry) >= 4 & month(form_entry) <= 7 & year(form_entry) == 2022 & village_name %in% c("lambayama", "baiama") ~ 4,
                                    month(form_entry) >= 8 & month(form_entry) <= 9 & year(form_entry) == 2022 & village_name %in% c("lalehun", "seilama", "lambayama", "baiama") ~ 7,
                                    TRUE ~ visit_number),
           study_site = case_when(KEY == "uuid:5bedb731-5e12-49e2-8aef-fca6a1647690" ~ 2, # A trap site was miscoded as 1 instead of 2, this corrects it.
                                  KEY == "uuid:78e1e027-0851-4c47-8077-ccb8e0e2f049" ~ 5,
                                  habitat_type == "village_inside" | str_detect(site_use, "In house|Indoor") ~ 7,
                                  habitat_type == "village_outside" & !study_site %in% c(1, 3) ~ 7,
                                  TRUE ~ study_site),
           habitat_type = case_when(is.na(habitat_type) & village_name == "lambayama" & study_site == 1 ~ "proximal_agriculture",
                                    is.na(habitat_type) & village_name == "lalehun" & study_site == 1 ~ "proximal_agriculture",
                                    is.na(habitat_type) & village_name == "lalehun" & study_site == 7 ~ "village_outside",
                                    TRUE ~ habitat_type),
           intensity = case_when(str_detect(habitat_type, "village") ~ "intense",
                                 TRUE ~ intensity),
           `non_village-crop_type_new_site` = case_when(KEY == "uuid:ce1c4ae9-a601-4b1a-b571-fc652f3624d1" ~ "Palm plantation",
                                                        str_detect(`non_village-crop_type_new_site`, "None for now") ~ "fallow",
                                                        str_detect(habitat_type, "agriculture") & str_detect(`non_village-crop_type_new_site`, "none|None") ~ "fallow",
                                                        is.na(`non_village-crop_type_new_site`) & str_detect(habitat_type, "agriculture") ~ site_use,
                                                        TRUE ~ `non_village-crop_type_new_site`)) %>%
    group_by(village_name, visit_number) %>%
    mutate(date_set = ymd(min(form_entry))) %>%
    rename("crop_type" = "non_village-crop_type_new_site",
           "number_traps" = "non_village-number_of_traps", 
           "number_houses" = "within_houses-number_of_houses",
           "house_count" = "within_houses-house_count",
           "key" = "KEY") %>%
    dplyr::select(-any_of(not_needed_vars), -starts_with("site_images"))
  
  sites_recorded <- paste0(trap_sites$village_name, "_", trap_sites$visit_number, "_",  trap_sites$study_site) %>%
    sort()
  
  site_habitats <- tibble(village = trap_sites$village_name,
                          visit_number = trap_sites$visit_number,
                          site = trap_sites$study_site,
                          habitat = trap_sites$habitat_type) %>%
    drop_na() %>%
    mutate(habitat = case_when(village == "lalehun" & visit_number == 2 & site == 1 ~ "proximal_agriculture",
                               habitat %in% c("village", "village_inside", "village_outside") ~ "village",
                               TRUE ~ habitat)) %>%
    group_by_all() %>%
    distinct() %>%
    ungroup() %>%
    arrange(village, visit_number, site)
  
  missing_site_habitats <- tibble(village = c(rep("lalehun", 6), rep("seilama", 6), rep("lalehun", 3), rep("seilama", 6)),
                                  visit_number = c(rep(1, 12), rep(2, 9)),
                                  site = c(rep(c(1, 2, 3, 4, 5, 6), 2), 2, 3, 4, 1, 2, 3, 4, 5, 7),
                                  habitat = c(rep("proximal_agriculture", 3), "forested", "distal_agriculture", "village",
                                              rep("proximal_agriculture", 2), rep("distal_agriculture", 2), "forested", "village",
                                              rep("proximal_agriculture", 3), 
                                              rep("proximal_agriculture", 2), rep("distal_agriculture", 2), "forested", "village"))
  
  site_habitats <- bind_rows(site_habitats, missing_site_habitats)
  
  sites <- list(trap_sites = trap_sites,
                sites_recorded = sites_recorded,
                site_habitats = site_habitats)
  
  return(sites)
}
