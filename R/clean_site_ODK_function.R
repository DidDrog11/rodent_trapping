clean_site_ODK <- function() {
  
  # Expected results are 1 entry for each trap site in each village at each visit.
  # Visit 1 was completed on paper for Seilama and Lalehun, so they should only start at visit 2
  # Bambawo was stopped after visit 1
  # The different numbers confused the field team so Baiama and Lambayama went from visit 4 to visit 7, i.e. 5 and 6 are missing
  # Sites 6 and 7 are combined
  # There should not be two entries for a single site
  
  
  all_files <- list.files(here("data", "raw_odk", paste0("trap_sites", "_", Sys.Date())), full.names = T)
  
  if(identical(all_files, character(0))) all_files <- list.files(tail(sort(list.files(here("data", "raw_odk"), pattern = "trap_sites_", full.names = TRUE)), 1), full.names = TRUE)
  
  not_needed_vars <- c("other_village_name", "consecutive_traps", "meta-instanceID", "submitterID", "SubmitterName", "AttachmentsPresent",
                       "AttachmentsExpected", "Status", "ReviewState", "DeviceID", "Edits", "non_village-consecutive_traps", "non_village-starting_number",
                       "non_village-highest", "non_village-ending_number", "non_village-trap_count", "within_houses-consecutive_traps", "swapped_lat", "swapped_lon")
  
  trap_sites <- read_csv(all_files[4], show_col_types = FALSE) %>%
    filter(ReviewState != "rejected" | is.na(ReviewState)) %>%
    filter(!KEY %in% c("uuid:4161d048-299d-42b8-a758-480ba044cea9", "uuid:3db1d7ec-ca8c-407c-900a-b7d4e48c3d3f", "uuid:1d3d91e3-6668-4cf5-a3d9-81d5ba02b41a")) %>% # Ignore files that were used for testing
    mutate(SubmissionDate = ymd(as.Date(SubmissionDate)),
           form_entry = ymd(as.Date(form_entry)),
           other_village_name = case_when(other_village_name == "Lambeyama" ~ "Lambayama", # Correct the spelling of a village site
                                          other_village_name == "Lambiyama" ~ "Lambayama",
                                          TRUE ~ other_village_name),
           village_name = case_when(village_name != "other" ~ village_name,
                                    TRUE ~ tolower(as.character(other_village_name))),
           study_site = case_when(KEY == "uuid:5bedb731-5e12-49e2-8aef-fca6a1647690" ~ 2, # A study site was miscoded as 1 instead of 2, this corrects it.
                                  KEY == "uuid:78e1e027-0851-4c47-8077-ccb8e0e2f049" ~ 5,
                                  KEY == "uuid:ed678515-2168-4644-8452-0a501168c333" ~ 4,
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
  
  recode_visits <- trap_sites %>%
    ungroup() %>%
    select(form_entry, village = village_name, visit_number, study_site) %>%
    mutate(year = year(form_entry),
           month = month(form_entry),
           day = case_when(village == "seilama" & month == 10 & year == 2022 ~ day(form_entry),
                           TRUE ~ as.integer(NA))) %>%
    left_join(visit_dates %>%
                mutate(day = as.integer(day)), by = c("year", "month", "day", "village")) %>%
    select(form_entry, village, visit)
  
  trap_sites <- trap_sites %>%
    ungroup() %>%
    left_join(recode_visits %>%
                select(form_entry, village_name = village, visit)) %>%
    select(-visit_number) %>%
    rename(visit_number = visit)
  
  sites_recorded <- paste0(trap_sites$village_name, "_", trap_sites$visit_number, "_",  trap_sites$study_site) %>%
    sort()
  
  # The following dataframe contains all of the habitats that should have been sampled. 
  # Ultimately we group both village_outside (6) and village_inside (7) as site 7
  site_habitats <- tibble(village = c(rep("lalehun", 7), rep("seilama", 7), rep("bambawo", 4), rep("baiama", 6), rep("lambayama", 6)),
                                  study_site = c(1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 1, 2, 3, 4, 7, 7, 1, 2, 3, 4, 7, 7),
                                  habitat = c("proximal_agriculture", "proximal_agriculture", "proximal_agriculture", "forest", "distal_agriculture", "village_outside", "village_inside",
                                              "proximal_agriculture", "proximal_agriculture", "distal_agriculture", "distal_agriculture", "forest", "village_outside", "village_inside",
                                              "forest", "distal_agriculture", "fallow", "proximal_agriculture",
                                              "forest", "distal_agriculture", "proximal_agriculture", "proximal_agriculture", "village_outside", "village_inside",
                                              "proximal_agriculture", "proximal_agriculture", "fallow", "proximal_agriculture", "village_outside", "village_inside"),
                                  visit = max(trap_sites$visit_number)) %>%
    uncount(visit, .id = "visit") %>%
    arrange(visit, village, study_site, habitat) %>%
    mutate(remove = case_when(str_detect(village, "baiama|lambayama") & visit <= 2 ~ TRUE,
                              str_detect(village, "baiama|lambayama") & visit == 3 & str_detect(habitat, "village") ~ TRUE,
                              str_detect(village, "bambawo") & visit != 3 ~ TRUE,
                              str_detect(village, "lalehun|seilama") & visit <= 3 & str_detect(habitat, "village") ~ TRUE,
                              TRUE ~ FALSE)) %>% # Remove sites that were not set up prior to that visit
    filter(remove == FALSE) %>%
    select(-remove)
  
  sites <- list(trap_sites = trap_sites,
                sites_recorded = sites_recorded,
                site_habitats = site_habitats)
  
  return(sites)
}
