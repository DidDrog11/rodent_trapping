clean_trap_check_ODK <- function() {
  
  all_files <- list.files(here("data", "raw_odk", paste0("trap_check", "_", Sys.Date())), full.names = T)
  
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
  
  duplicated_forms = c("uuid:34ab21ed-10b6-4f6d-8b00-4436defa3316",
                       "uuid:35258b6b-df77-4032-9737-ef8f6cdb1f7a",
                       "uuid:a5b5f8bd-daeb-4f99-a9da-df1d903ee3a6")
 
  trap_check <- read_csv(all_files[4], show_col_types = FALSE) %>%
    filter(ReviewState != "rejected" | is.na(ReviewState)) %>%
    filter(!KEY %in% duplicated_forms) %>%
    mutate(village_name = as_factor(tolower(village_name)),
           form_entry = as.Date(ymd_hms(form_entry))) %>%
    rename("village" = "village_name",
           "visit" = "visit_number",
           "grid_number" = "study_site",
           "bait_removed" = "individual_traps-bait_questions-bait_removed",
           "number_missing_bait" = "individual_traps-bait_questions-number_traps_bait_missing",
           "trap_shut" = "individual_traps-trap_closed-number_traps_shut",
           "rodents_trapped" = "individual_traps-rodent_trapped-number_rodents_trapped",
           "key" = "KEY") %>%
    dplyr::select(-any_of(not_needed_vars), -starts_with("site_images")) %>%
    mutate(visit = case_when(key == "uuid:21f76f88-0c45-4382-97fa-4768777feae8" ~ 1,
                             month(form_entry) <= 2 & year(form_entry) == 2022 & village %in% c("lalehun", "seilama") ~ 5,
                             TRUE ~ visit)) # Correct miscoded visit number
  
  last_date <- max(trap_check$form_entry)
  
  trap_check_bait <- read_csv(all_files[1], show_col_types = FALSE) %>%
    filter(!KEY %in% duplicated_forms) %>%
    rename("trap_missing_bait" = 1,
           "key" = "PARENT_KEY") %>%
    mutate(bait_present = case_when(is.na(trap_missing_bait) ~ "y",
                                    TRUE ~ "n")) %>%
    dplyr::select(-KEY) %>%
    distinct(trap_missing_bait, key, bait_present)
  
  trap_check_shut <- read_csv(all_files[2], show_col_types = FALSE) %>%
    filter(!KEY %in% duplicated_forms) %>%
    rename("key" = "PARENT_KEY") %>%
    mutate(trap_sprung = case_when(is.na(trap_number_shut) ~ "n",
                                   TRUE ~ "y")) %>%
    dplyr::select(-KEY) %>%
    distinct(trap_number_shut, key, trap_sprung)
  
  trap_check_rodent <- read_csv(all_files[3], show_col_types = FALSE) %>%
    filter(!KEY %in% duplicated_forms) %>%
    rename("key" = "PARENT_KEY") %>% 
    mutate(rodent_trapped = case_when(is.na(trap_number_rodent) ~ "n",
                                      TRUE ~ "y")) %>%
    dplyr::select(-KEY) %>%
    distinct(trap_number_rodent, key, rodent_trapped)
  
  combined_trap_check <- trap_check_bait %>%
    mutate(trap_number = case_when(is.na(trap_missing_bait) ~ NA_integer_,
                                   TRUE ~ as.integer(trap_missing_bait))) %>%
    full_join(., trap_check_shut %>%
                mutate(trap_number = trap_number_shut), by = c("key", "trap_number")) %>%
    full_join(., trap_check_rodent %>%
                mutate(trap_number = trap_number_rodent), by = c("key", "trap_number")) %>%
    dplyr::select(trap_number, key, bait_present, trap_sprung, rodent_trapped) %>%
    full_join(trap_check, ., by = "key") %>%
    dplyr::select(-c(bait_removed, number_missing_bait, trap_shut, rodents_trapped)) %>%
    mutate(grid_number = case_when(is.na(grid_number) & village == "lalehun" & !is.na(trap_number) ~ 7,
                                   TRUE ~ grid_number)) %>%
    mutate(trap_uid = case_when(!is.na(trap_number) & !is.na(grid_number) & !is.na(visit) ~ paste0(village, "_",
                                                                                                   visit, "_",
                                                                                                   trap_night, "_",
                                                                                                   grid_number, "_",
                                                                                                   trap_number)))
  
  return(combined_trap_check)
}