clean_rodent_data_ODK <- function(){
  
  all_files <- list.files(here("data", "raw_odk", paste0("rodent_data", "_", Sys.Date())), full.names = T)
  
  correct_species_error <- c("unclear_dasymys", "unclear_lemnisomys",
                             "unclear_lophuromys", "unclear_praomys")  
  names(correct_species_error) <- c("unclear_dasymysrus", "unclear_lemniscrus", 
                                    "unclear_lophurorus", "unclear_proamysrus")
  
  not_needed_vars <- c("SubmissionDate", "village_name", "rodent_numbering", 
                       "rodent_number", "trap_details-study_site", "trap_details-site_number_input", 
                       "trap_details-trap_number_int", "trap_details-trap_night", "trap_details-notes", 
                       "rodent_details-genus", "rodent_details-genus_other", "rodent_details-species", 
                       "rodent_details-species_other", "rodent_details-photo_dorsal", 
                       "rodent_details-photo_ventral", "rodent_details-weight", "rodent_details-head_body", 
                       "rodent_details-cut_tail", "rodent_details-tail", "rodent_details-hind_foot", 
                       "rodent_details-ear", "rodent_details-skull", "rodent_details-sex", 
                       "rodent_details-testes", "rodent_details-seminal", "rodent_details-vagina", 
                       "rodent_details-teats_visible", "rodent_details-number_teats", 
                       "rodent_details-embryos", "acquisition-blood_filter", "acquisition-filter_label", 
                       "acquisition-blood_serum", "acquisition-liver_spleen", "acquisition-ear_snip", 
                       "acquisition-eye_sample", "notes", "meta-instanceID", "KEY", 
                       "SubmitterID", "SubmitterName", "AttachmentsPresent", "AttachmentsExpected", 
                       "Status", "ReviewState", "DeviceID", "Edits", "village_abbreviation", "date_set")
  
  
  rodents <- read_csv(all_files[1], show_col_types = FALSE) %>%
    filter(ReviewState != "rejected" | is.na(ReviewState)) %>%
    mutate(date_entered = as.Date(ymd_hms(form_entry)),
           village_abbreviation = toupper(str_sub(village_name, end = 3)),
           visit = case_when(is.na(visit) & village_name == "bambawo" ~ 1,
                             TRUE ~ visit),
           genus = case_when(`rodent_details-genus` == "not_listed" ~ `rodent_details-genus_other`,
                             TRUE ~ `rodent_details-genus`),
           initial_species_id = recode(`rodent_details-species`, !!!correct_species_error),
           initial_species_id = case_when(str_detect(initial_species_id, "unclear") ~ paste0(genus, "_spp"),
                                          str_detect(initial_species_id, "not_listed") ~ paste0(genus, "_spp"),
                                          is.na(`rodent_details-species`) ~ paste0(genus, "_spp"),
                                          genus == "shrew" ~ "crocidura_spp",
                                          TRUE ~ initial_species_id),
           photos_taken = case_when(!is.na(`rodent_details-photo_dorsal`) & !is.na(`rodent_details-photo_ventral`) ~ "yes",
                                    TRUE ~ "no"),
           all_samples = case_when(`acquisition-blood_filter` == "yes" &
                                     `acquisition-blood_serum` == "yes" &
                                     `acquisition-liver_spleen` == "yes" &
                                     `acquisition-ear_snip` == "yes" &
                                     `acquisition-eye_sample` == "yes" ~ "yes",
                                   TRUE ~ "no"),
           visit = case_when(visit == 41 ~ 4,
                             TRUE ~ visit)) %>%
    rename("village" = "village_name",
           "study_site" = "trap_details-study_site", 
           "trap_number" = "trap_details-trap_number_int",
           "trap_night" = "trap_details-trap_night",
           "dorsal_image_id"  = "rodent_details-photo_dorsal", 
           "ventral_image_id" = "rodent_details-photo_ventral",
           "weight" = "rodent_details-weight", 
           "head_body" = "rodent_details-head_body", 
           "cut_tail" = "rodent_details-cut_tail", 
           "tail" = "rodent_details-tail", 
           "hind_foot" = "rodent_details-hind_foot", 
           "ear" = "rodent_details-ear", 
           "length_skull" = "rodent_details-skull", 
           "sex" = "rodent_details-sex", 
           "testes" = "rodent_details-testes",
           "seminal_vesicles" = "rodent_details-seminal", 
           "vagina_perforate" = "rodent_details-vagina", 
           "teats_visible" = "rodent_details-teats_visible", 
           "pairs_teats" = "rodent_details-number_teats", 
           "number_embryos" = "rodent_details-embryos",
           "key" = "KEY") %>%
    filter(key != "uuid:16e6f1d6-6e16-4d70-8aa2-b82698872d69") %>%
    # Create the rodent number based on the label used for blood filter
    mutate(filter_label_number = str_pad(str_extract(`acquisition-filter_label`, "\\d+[^\\d]*$"), 3, pad = 0), 
           rodent_uid = paste0(visit, "_", village_abbreviation, "_", filter_label_number),
           trap_number = case_when(rodent_number == 1 & village == "bambawo" & visit == 1 ~ 9,
                                   rodent_number == 2 & village == "bambawo" & visit == 1 ~ 13,
                                   TRUE ~ trap_number),
           study_site = case_when(village == "lalehun" & study_site == "not_listed" & visit == 4 ~ 7,
                                  
                                  rodent_number == 10 & village == "bambawo" & visit == 1 ~ 3,
                                  rodent_number == 3 & village == "bambawo" & visit == 1 ~ 3,
                                  rodent_number == 11 & village == "bambawo" & visit == 1 ~ 1,
                                  
                                  rodent_number == 21 & village == "seilama" & visit == 3 ~ 4,
                                  rodent_number == 20 & village == "seilama" & visit == 3 ~ 3,
                                  rodent_number == 17 & village == "seilama" & visit == 3 ~ 3,
                                  rodent_number == 14 & village == "seilama" & visit == 3 ~ 5,
                                  rodent_number == 13 & village == "seilama" & visit == 3 ~ 3,
                                  rodent_number == 12 & village == "seilama" & visit == 3 ~ 3,
                                  rodent_number == 11 & village == "seilama" & visit == 3 ~ 4,
                                  rodent_number == 24 & village == "seilama" & visit == 3 ~ 4,
                                  
                                  trap_number == 12 & village == "seilama" & visit == 4 ~ 1,
                                  trap_number == 10 & village == "seilama" & visit == 4 ~ 1,
                                  trap_number == 313 & village == "seilama" & visit == 4 ~ 7,
                                  
                                  rodent_number == 12 & village == "lalehun" & visit == 3 ~ 2,
                                  rodent_number == 6 & village == "lalehun" & visit == 3 ~ 1,
                                  rodent_number == 4 & village == "lalehun" & visit == 3 ~ 2,
                                  rodent_number == 7 & village == "lalehun" & visit == 3 ~ 3,
                                  rodent_number == 3 & village == "lalehun" & visit == 3 ~ 3,
                                  
                                  rodent_number == 5 & village == "lambayama" & visit == 1 ~ 4,
                                  TRUE ~ as.numeric(study_site)),
           study_site = as_factor(study_site),
           trap_uid = paste0(village, "_", visit, "_", trap_night, "_", study_site, "_", trap_number)) %>%
           dplyr::select(-any_of(not_needed_vars), -starts_with("site_images"))
  
}