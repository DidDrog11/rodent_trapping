ODK_paper_combine_rodent <- function(ODK_data = ODK_rodents) {
  
  factor_vars <- c("village", "visit", "grid_number", "line_number",
                   "bait_type", "trap_uid", "empty_morning", "bait_present", "trap_sprung", "rodent_trapped",
                   "rodent_id", "weather", "initial_species_id", "group", "sex", "testes", "seminal_vesicles",
                   "vagina_perforate", "teats_visible", "photos_taken", "visit", "all_samples", "cut_tail", "genus",
                   "habitat_group")
  
  paper_forms_trapped_rodents <- readxl::read_xlsx(path = here("data", "trap_sites_all.xlsx"), sheet = 3) %>%
    mutate(visit = case_when(str_starts(rodent_id, "[A-Z]") ~ "1",
                             TRUE ~ str_sub(rodent_id, start = 1, end = 1)),
           visit = as.numeric(visit),
           all_samples = case_when(blood_filter == "y" &
                                     serum == "y" &
                                     liver_spleen == "y" &
                                     ear_sampled == "y" &
                                     eye_sampled == "y" ~ "yes",
                                   TRUE ~ "no"),
           date_entered = ymd(date),
           pairs_teats = as.numeric(pairs_teats),
           number_embryos = as.numeric(number_embryos),
           study_site = case_when(site_id == "3b" ~ "3",
                                  TRUE ~ as.character(site_id)),
           study_site = as_factor(as.integer(study_site)),
           village = case_when(str_detect(rodent_id, "LAL") ~ "lalehun",
                               str_detect(rodent_id, "SEI") ~ "seilama"),
           trap_uid = paste0(village, "_", visit, "_", trap_night, "_", study_site, "_", trap_id)) %>%
    rename("vagina_perforate" = "vagina",
           "photos_taken" = "photos",
           "trap_number" = "trap_id",
           "rodent_uid" = "rodent_id") %>%
    dplyr::select(-date, -any_of(c("blood_filter", "filter_id", "serum", "liver_spleen", "ear_sampled", "eye_sampled", "site_id")))
  
  combined_rodents <- bind_rows(paper_forms_trapped_rodents,
                                ODK_data) %>%
    mutate(across(any_of(factor_vars), ~as_factor(.)),
           rodent_uid = substr(rodent_uid, 1, 9))
  
  write_csv(combined_rodents, here("data", "clean_data", "rodents", paste0("rodents_trapped_", Sys.Date(), ".csv")))
  
  return(combined_rodents)
  
}