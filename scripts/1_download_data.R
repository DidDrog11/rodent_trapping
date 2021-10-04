source(here::here("scripts", "0_project_library.R"))
source(here("scripts", "0_ODK_API.R"))

drive_download("https://drive.google.com/file/d/1kxpH6RvWgAMwhpqZ4yoH_6SYso06uuDa/view?usp=sharing",
               path = here("data", "trap_sites_all.xlsx"), overwrite = T) # Download data that was captured initially on paper forms

trap_site_odk <- clean_odk_sites()
odk_trap <- trap_site_odk$odk_data
coord_errors <- trap_site_odk$coord_error

# Map of likely errors and likely correctly placed traps fix before continuing with analysis
mapview::mapview(coord_errors, zcol = "coord")

odk_trap_checks <- clean_odk_trap_check()
odk_rodent <- clean_odk_rodent()

odk_forms_combined <- odk_trap %>%
  mutate(bait_present = case_when(date_set > last_date ~ "missing",
                                  trap_uid %in% odk_trap_checks$trap_uid[odk_trap_checks$id == "bait"] ~ "n",
                                  TRUE ~ "y"),
         trap_sprung = case_when(date_set > last_date ~ "missing",
                                 trap_uid %in% odk_trap_checks$trap_uid[odk_trap_checks$id == "sprung"] ~ "y",
                               TRUE ~ "n"),
         rodent_trapped = case_when(date_set > last_date ~ "missing",
                                    trap_uid %in% odk_trap_checks$trap_uid[odk_trap_checks$id == "rodent"] ~ "y",
                                    TRUE ~ "n"),
         empty_morning = case_when(rodent_trapped == "y" ~ "y",
                                   rodent_trapped == "missing" ~ "missing",
                                   TRUE ~ "n"))

paper_forms_trap <- readxl::read_xlsx(path = here("data", "trap_sites_all.xlsx"), sheet = 2) %>%
  mutate(lon_DdM = paste(paste(lon_degree, lon_dec, sep = "_"), "'", sep = ""),
         lat_DdM = paste(paste(lat_degree, lat_dec, sep = "_"), "'", sep = ""),
         lon = -1 * dg2dec(var = lon_DdM, Dg = "_", Min = "'"),
         lat = dg2dec(var = lat_DdM, Dg = "_", Min = "'"),
         date_set = as.Date(date_set) + (as.numeric(trap_night)-1),
         across(any_of(factor_vars), ~ as_factor(.)),
         grid_number = case_when(grid_number %in% c("3a", "3b") ~ "3",
                                TRUE ~ as.character(grid_number)),
         grid_number = as_factor(as.integer(grid_number)),
         trap_uid = paste0(village, "_", visit, "_", trap_night, "_", grid_number, "_", trap_number)) %>%
  dplyr::select(-c("lat_degree", "lat_dec", "lon_degree", "lon_dec", "lon_DdM", "lat_DdM"))

trap_sites <- bind_rows(paper_forms_trap, odk_forms_combined) %>%
  dplyr::select(-`...20`) %>%
  mutate(across(any_of(factor_vars), ~ as_factor(.))) %>%
  rename("rodent_id_paper" = "rodent_id")

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
         "trap_number" = "trap_id") %>%
  dplyr::select(-date, -any_of(c("blood_filter", "filter_id", "serum", "liver_spleen", "ear_sampled", "eye_sampled", "site_id")))

combined_rodents <- bind_rows(paper_forms_trapped_rodents,
                              odk_rodent) %>%
  mutate(across(any_of(factor_vars), ~as_factor(.))) %>%
  write_csv(here("data", "clean_data", "rodents", paste0("rodents_trapped", Sys.Date(), ".csv")))

# Add the rodent ID's back into the trap data
final_sites <- trap_sites %>%
  left_join(., combined_rodents %>%
              dplyr::select(trap_uid, rodent_id),
            by = "trap_uid") %>%
  mutate(rodent_trapped = case_when(!is.na(rodent_id) ~ "y",
                                    TRUE ~ as.character(rodent_trapped)),
         trap_sprung = case_when(!is.na(rodent_id) ~ "y",
                                 TRUE ~ as.character(trap_sprung)),
         across(any_of(factor_vars), ~as_factor(.))) %>%
  write_csv(here("data", "clean_data", "trap_sites", paste0("trap_sites", Sys.Date(), ".csv"))) #Read the data file from excel document and save within the repo as csv

missing_rodents <- combined_rodents %>%
  filter(!rodent_id %in% final_sites$rodent_id)

rodent_identification <- readxl::read_xlsx(path = here("data", "trap_sites_all.xlsx"), sheet = 4) %>%
  write_csv(here("data", "rodent_ids.csv"))
