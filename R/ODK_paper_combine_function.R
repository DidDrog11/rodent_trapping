ODK_paper_combine <- function(ODK_data = ODK_combined) {
  
  drive_download("https://drive.google.com/file/d/1kxpH6RvWgAMwhpqZ4yoH_6SYso06uuDa/view?usp=sharing",
                 path = here("data", "trap_sites_all.xlsx"), overwrite = T) # Download data that was captured initially on paper forms
  
  factor_vars <- c("village", "visit", "grid_number", "line_number",
                   "bait_type", "trap_uid", "empty_morning", "bait_present", "trap_sprung", "rodent_trapped",
                   "rodent_id", "weather", "initial_species_id", "group", "sex", "testes", "seminal_vesicles",
                   "vagina_perforate", "teats_visible", "photos_taken", "visit", "all_samples", "cut_tail", "genus",
                   "habitat_group")
  
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
           visit = as.numeric(visit),
           trap_number = as.integer(trap_number),
           trap_night = as.numeric(trap_night),
           trap_uid = paste0(village, "_", visit, "_", trap_night, "_", grid_number, "_", trap_number),
           across(where(is.factor), ~ recode(., "y" = "yes", "n" = "no"))) %>%
    dplyr::select(-c("lat_degree", "lat_dec", "lon_degree", "lon_dec", "lon_DdM", "lat_DdM", `...24`, "empty_morning")) %>%
    rename("rodent_uid" = "rodent_id")
  
  trap_sites <- bind_rows(ODK_data, paper_forms_trap) %>%
    mutate(across(any_of(factor_vars), ~ as_factor(.)))
  
  write_csv(trap_sites, here("data", "clean_data", "trap_sites", paste0("trap_sites_", Sys.Date(), ".csv")))
  
  return(trap_sites)
  
}