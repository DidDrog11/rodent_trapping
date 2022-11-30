clean_rodent_data_ODK <- function(){
  
  all_files <- list.files(here("data", "raw_odk", paste0("rodent_data", "_", Sys.Date())), full.names = T)
  
  if(!identical(all_files, character(0))) {
    
    if(download_rodent_pictures == TRUE) {
      
      all_files <- list.files(tail(sort(list.files(here("data", "raw_odk"), pattern = "rodent_data_", full.names = TRUE)), 1), full.names = TRUE)[2]
      
    } else {
      
      all_files <- list.files(tail(sort(list.files(here("data", "raw_odk"), pattern = "rodent_data_", full.names = TRUE)), 1), full.names = TRUE)[1]
      
    } 
  } else {
    
    all_files <- if(identical(all_files, character(0))) {
      
      list.files(tail(sort(list.files(here("data", "raw_odk"), pattern = "rodent_data_", full.names = TRUE)), 1), full.names = TRUE)[1]
      
    } else { 
      
      list.files(here("data", "raw_odk", paste0("rodent_data", "_", Sys.Date())), full.names = TRUE)
      
    }
  }
  
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
    mutate(village_name = case_when(is.na(village_name) & rodent_number == 38 ~ "seilama",
                                    KEY == "uuid:d33d36a5-37ce-4453-bcde-46ef75d23974" ~ "lambayama",
                                    TRUE ~ village_name),
           date_entered = as.Date(ymd_hms(form_entry)),
           village_abbreviation = toupper(str_sub(village_name, end = 3)),
           visit = case_when(KEY == "uuid:44df6c7a-ab7c-4ed5-8223-2e583395fb17" ~ 8, # grid missing for visit, rodent allocated to different visit in same season
                             KEY == "uuid:856d83f6-15e0-4084-a9a8-a61cc8aba13a" ~ 5, # grid missing allocated to next visit
                             is.na(visit) & village_name == "bambawo" ~ 1,
                             month(form_entry) %in% c(6, 7) & year(form_entry) == 2021 & village_name %in% c("lambayama", "baiama") ~ 1,
                             month(form_entry) %in% c(6, 7) & year(form_entry) == 2021 & village_name %in% c("lalehun", "seilama") ~ 3,
                             month(form_entry) %in% c(10) & year(form_entry) == 2021 & village_name %in% c("lalehun", "seilama") ~ 4,
                             month(form_entry) %in% c(10, 11) & year(form_entry) == 2021 & village_name %in% c("lambayama", "baiama") ~ 2,
                             month(form_entry) %in% c(1) & year(form_entry) == 2022 & village_name %in% c("lalehun", "seilama") ~ 5,
                             month(form_entry) %in% c(1) & year(form_entry) == 2022 & village_name %in% c("lambayama", "baiama") ~ 3,
                             month(form_entry) %in% c(4) & year(form_entry) == 2022 & village_name %in% c("lalehun", "seilama") ~ 6,
                             month(form_entry) %in% c(4) & year(form_entry) == 2022 & village_name %in% c("lambayama", "baiama") ~ 4,
                             month(form_entry) %in% c(8) & year(form_entry) == 2022  ~ 7,
                             visit == 41 ~ 4,
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
           rodent_number = case_when(rodent_number == 249 ~ 8,
                                     KEY == "uuid:a11445f8-6ec5-4726-a0f2-f915c22fa148" ~ 13,
                                     TRUE ~ rodent_number)) %>%
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
           filter_label_number = case_when(key == "uuid:a11445f8-6ec5-4726-a0f2-f915c22fa148" ~ "013",
                                           TRUE ~ filter_label_number),
           filter_label = `acquisition-filter_label`,
           rodent_uid = paste0(visit, "_", village_abbreviation, "_", filter_label_number),
           trap_number = case_when(rodent_number == 1 & village == "bambawo" & visit == 1 ~ 9,
                                   rodent_number == 2 & village == "bambawo" & visit == 1 ~ 13,
                                   
                                   rodent_number == 4 & village == "lambayama" & visit == 2 ~ 232,
                                   key == "uuid:5a5cc97c-55d8-4022-a8e4-296b050f180a" ~ 301,
                                   key == "uuid:92153960-251f-4946-ad1e-034b02326eea" ~ 298,
                                   key == "uuid:6c7b1900-4802-4212-a9b9-f105735ff999" ~ 232,
                                   key == "uuid:05bfcf2d-8935-4b36-a5e7-252c6ae91b6d" ~ 11,
                                   key == "uuid:494fbf14-c7a6-4d8c-9251-11d87c6a5584" ~ 18,
                                   key == "uuid:8b0553c5-6f1c-4cfc-94a6-5aa29ce54f17" ~ 31,
                                   key == "uuid:e89bc936-1505-4ccf-9903-b555c5a1be0e" ~ 34,
                                   key == "uuid:3a144c76-9fe3-4075-9f60-033163e88963" ~ 40,
                                   key == "uuid:a318a091-f7ff-4612-ab65-672def33a705" ~ 59,
                                   key == "uuid:6693d496-2e2a-4c16-a8c2-015302fc9577" ~ 142,
                                   key == "uuid:08398e35-e7a1-44d7-8637-4a4743c7b411" ~ 229,
                                   key == "uuid:856d83f6-15e0-4084-a9a8-a61cc8aba13a" ~ 192, #
                                   key == "uuid:e17b3227-bc71-4d2e-8e04-c2f340ae0de8" ~ 225, #
                                   key == "uuid:856d83f6-15e0-4084-a9a8-a61cc8aba13a" ~ 228, #
                                   key == "uuid:5ffee554-6d8c-4c33-ba16-72403c3f5305" ~ 243, #
                                   key == "uuid:72d60def-49f3-43a0-988f-0b4f3ce9902e" ~ 246, #
                                   key == "uuid:73063e63-4fe1-49b2-b3b3-54335745a093" ~ 335,
                                   key == "uuid:541b64c1-eedd-4220-ba3c-faf7c9c79c43" ~ 264, # Grid data missing, allocated to closest known trap
                                   key == "uuid:e17b3227-bc71-4d2e-8e04-c2f340ae0de8" ~ 255, # Grid data missing, allocated to closest known trap
                                   key == "uuid:856d83f6-15e0-4084-a9a8-a61cc8aba13a" ~ 202, # Grid missing, allocated to trap at next session
                                   key == "uuid:5ffee554-6d8c-4c33-ba16-72403c3f5305" ~ 253, # Grid data missing, allocated to closest known trap
                                   key == "uuid:92153960-251f-4946-ad1e-034b02326eea" ~ 293, # Grid data missing, allocated to closest known trap
                                   key == "uuid:5a5cc97c-55d8-4022-a8e4-296b050f180a" ~ 281, # Grid data missing, allocated to closest known trap
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
                                  
                                  rodent_number %in% c(8, 9, 10, 11, 12, 25) & village == "lalehun" & visit == 4 ~ 7,
                                  
                                  rodent_number == 5 & village == "lambayama" & visit == 1 ~ 4,
                                  
                                  rodent_number == 38 & village == "seilama" & visit == 5 ~ 3,
                                  
                                  # Wrong site listed for Lalehun
                                  # Visit 4
                                  key == "uuid:0428fbca-5684-4046-ae3a-d5e39e10aa78" ~ 7,
                                  
                                  # Wrong site listed for Baiama
                                  # Visit 7
                                  key %in% c("uuid:02a4d50a-345e-41e2-90ae-f5a5e35abfc9",
                                             "uuid:6c26d66d-2f2d-4fc5-a2a9-a04122941902",
                                             "uuid:77d21152-3b44-4064-b416-12b8740cf25f",
                                             "uuid:b1309980-c4ca-49f4-aabe-4b15613af033",
                                             "uuid:35971111-f6d7-4f22-af79-01f3eba3d073",
                                             "uuid:fc940bc6-210e-41a6-badb-7523294a81e0") ~ 7,
                                  
                                  # Visit 4
                                  key == "uuid:7900c206-affa-46ad-a48a-30ec373eb5bc" ~ 3,
                                  key %in% c("uuid:6fc707e5-b587-4495-8f50-f42a56bcc953",
                                             "uuid:447fb45f-cd6e-4f86-8700-a70988e45ded") ~ 7,
                                  
                                  #Visit 2
                                  key %in% c("uuid:dfb6dce9-1dc8-4c25-a5f7-a376b1af3fd2",
                                             "uuid:807ba701-dc1b-4fda-9e9d-f570c49a7a80",
                                             "uuid:1bdb8afa-a884-446b-8c70-40a00c84b8f7") ~ 7,
                                  
                                  # Wrong site listed for Lambayama
                                  
                                  # Visit 4
                                  key %in% c("uuid:79d42e7f-b09c-457b-b917-23ddd79d957b",
                                             "uuid:92d37977-90bd-4693-934a-0d39235a2cb8",
                                             "uuid:fa38c704-256d-4f14-85c3-9a9d0388b366",
                                             "uuid:99887e62-1634-42d9-b775-bb9f6cb0f0eb") ~ 4,
                                  # Visit 3
                                  key %in% c("uuid:a91f7490-6ff9-4ce0-a2db-5cab55f8552a",
                                            "uuid:7a8a6178-11d6-45c4-92e9-f658c1b3d5b9") ~ 7,
                                  
                                  # Visit 2
                                  key %in% c("uuid:ec85ef23-8f9c-4243-903f-c3331ac841d4",
                                             "uuid:6c7b1900-4802-4212-a9b9-f105735ff999",
                                             "uuid:82a6c741-3108-4756-b4d8-dd606e4aa04a",
                                             "uuid:9ecd5ad5-b1db-462d-9a68-23640f3caed6") ~ 7,
                                  
                                  key %in% c("uuid:c3b66aae-0645-498b-9d32-692f0186b566",
                                             "uuid:c9cfa3f3-85d9-41a6-98e4-1507c7eca703",
                                             "uuid:b416b322-798c-4a64-846b-0df6611f7c31",
                                             "uuid:15b9adb2-c5b9-4c35-806d-23b4e903fdc9",
                                             "uuid:6bbbbb3b-64af-4e56-81d8-f083b6aed5e4") ~ 4,
                                  
                                  study_site == 6 ~ 7,
                                  
                                  TRUE ~ as.numeric(study_site)),
           trap_night = case_when(key == "uuid:73063e63-4fe1-49b2-b3b3-54335745a093" ~ 4,
                                  TRUE ~ trap_night),
           initial_species_id = case_when(key == "uuid:73063e63-4fe1-49b2-b3b3-54335745a093" ~ "mastomys_spp",
                                          key == "uuid:1a49c72c-8ff7-4570-b710-3802c3b7ab34" ~ "mus_spp",
                                          TRUE ~ initial_species_id),
           study_site = as_factor(study_site),
           trap_uid = paste0(village, "_", visit, "_", trap_night, "_", study_site, "_", trap_number)) %>%
           dplyr::select(-any_of(not_needed_vars), -starts_with("site_images"))
  
}