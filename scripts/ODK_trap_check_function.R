clean_odk_trap_check <- function(type = "check") {
  
  all_files <- list.files(here("data", "raw_odk", "trap_check"), full.names = T)
  
  not_needed_vars <- c("SubmissionDate", "photo_paper", 
                       "notes", "meta-instanceID", "SubmitterID", "SubmitterName", 
                       "AttachmentsPresent", "AttachmentsExpected", "Status", "ReviewState", 
                       "DeviceID", "Edits", "individual_traps-bait_questions-number_repeat_bait_count",
                       "individual_traps-trap_closed-number_repeat_shut_count",
                       "individual_traps-rodent_trapped-rodents_trapped_count",
                       "individual_traps-bait_questions-number_traps_bait_missing",
                       "individual_traps-trap_closed-trap_found_shut",
                       "individual_traps-rodent_trapped-trap_rodent",
                       "individual_traps-bait_questions-number_traps_bait_missing",
                       "individual_traps-bait_questions-bait_removed")
  
  trap_check <- read_csv(all_files[4]) %>%
    filter(ReviewState != "rejected" | is.na(ReviewState)) %>%
    mutate(village_name = as_factor(tolower(village_name)),
           form_entry = as.Date(ymd_hms(form_entry))) %>%
    rename("village" = "village_name",
           "visit" = "visit_number",
           "grid_number" = "study_site",
           "bait_removed" = ,
           "number_missing_bait" = "individual_traps-bait_questions-number_traps_bait_missing",
           "trap_shut" = "individual_traps-trap_closed-number_traps_shut",
           "rodents_trapped" = "individual_traps-rodent_trapped-number_rodents_trapped",
           "key" = "KEY") %>%
    dplyr::select(-any_of(not_needed_vars), -starts_with("site_images")) %>%
    mutate(visit = case_when(key == "uuid:21f76f88-0c45-4382-97fa-4768777feae8" ~ 1,
           TRUE ~ visit)) # Correct miscoded visit number
  
  last_date <<- max(trap_check$form_entry)
  
  trap_check_bait <- read_csv(all_files[1]) %>%
    rename("trap_missing_bait" = 1,
           "key" = "PARENT_KEY") %>%
    mutate(bait_present = case_when(is.na(trap_missing_bait) ~ "y",
                                    TRUE ~ "n")) %>%
    dplyr::select(-KEY)
  
  trap_check_shut <- read_csv(all_files[2]) %>%
    rename("key" = "PARENT_KEY") %>%
    mutate(trap_sprung = case_when(is.na(trap_number_shut) ~ "n",
                                   TRUE ~ "y")) %>%
    dplyr::select(-KEY)
  
  trap_check_rodent <- read_csv(all_files[3]) %>%
    rename("key" = "PARENT_KEY") %>% 
    mutate(rodent_trapped = case_when(is.na(trap_number_rodent) ~ "n",
                                      TRUE ~ "y")) %>%
    dplyr::select(-KEY)
  
  combined_trap_check <- full_join(trap_check, trap_check_bait, by = "key") %>%
    full_join(., trap_check_shut, by = "key") %>%
    full_join(., trap_check_rodent, by = "key") %>%
    mutate(trap_uid_bait = case_when(bait_present == "n" ~ paste0(village, "_", visit, "_", trap_night, "_", grid_number, "_", trap_missing_bait)),
           trap_uid_shut = case_when(trap_sprung == "y" ~ paste0(village, "_", visit, "_", trap_night, "_", grid_number, "_", trap_number_shut)),
           trap_uid_rodent = case_when(rodent_trapped == "y" ~ paste0(village, "_", visit, "_", trap_night, "_", grid_number, "_", trap_number_rodent)))
  
  bait <- combined_trap_check %>%
    distinct(weather, trap_uid_bait) %>%
    drop_na() %>%
    rename("trap_uid" = "trap_uid_bait")
  
  sprung <- combined_trap_check %>%
    distinct(weather, trap_uid_shut) %>%
    drop_na() %>%
    rename("trap_uid" = "trap_uid_shut")
  
  rodent <- combined_trap_check %>%
    distinct(weather, trap_uid_rodent) %>%
    drop_na() %>%
    rename("trap_uid" = "trap_uid_rodent")
  
  positive_trap_check <- bind_rows(bait, sprung, rodent, .id = "id") %>%
    mutate(id = case_when(id == "1" ~ "bait",
                          id == "2" ~ "sprung",
                          id == "3" ~ "rodent")) %>%
    distinct(trap_uid, .keep_all = T)
  
  return(positive_trap_check)
}
