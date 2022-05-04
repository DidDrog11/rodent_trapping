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
  
  combined_rodents <- bind_rows(paper_forms_trapped_rodents %>%
                                  mutate(source_data = "Paper"),
                                ODK_data %>%
                                  mutate(source_data = "ODK")) %>%
    mutate(across(any_of(factor_vars), ~as_factor(.)),
           rodent_uid = substr(rodent_uid, 1, 9))
  
  # Age classification logic
  # Females with perforate vaginas are adults
  # Females with =>1 embryo are adults
  # Females without perforate vaginas are juveniles
  # Those with missing data are not_known
  # Males with developed seminal vesicles are adults
  # Males with testes outside their bodes are adults
  # Males with undeveloped seminal vesicles are juveniles
  # Non-crocidura males with internal testes are juveniles
  # Otherwise rodents are defined as not_known
  
  age_classification <- combined_rodents %>%
    mutate(age_group = case_when(sex == "female" & str_detect(vagina_perforate, "^open|yes") ~ "adult",
                                 sex == "female" & number_embryos >= 1 ~ "adult",
                                 sex == "female" & str_detect(vagina_perforate, "not_open|no$") ~ "juvenile",
                                 sex == "female" ~ "not_known",
                                 sex == "male" & str_detect(seminal_vesicles, "^developed|yes") ~ "adult",
                                 sex == "male" & str_detect(testes, "outside") ~ "adult",
                                 sex == "male" & str_detect(seminal_vesicles, "undeveloped|no$") ~ "juvenile",
                                 sex == "male" & str_detect(testes, "inside")  & !str_detect(initial_species_id, "crocidura") ~ "juvenile",
                                 TRUE ~ "not_known")) %>%
    select(rodent_uid, age_group)
  
  combined_rodents <- combined_rodents %>%
    left_join(., age_classification, by = "rodent_uid") %>%
    mutate(age_group = factor(age_group, levels = c("juvenile", "adult", "not_known")))
  
  write_csv(combined_rodents, here("data", "clean_data", "rodents", paste0("rodents_trapped_", Sys.Date(), ".csv")))
  
  return(combined_rodents)
  
}