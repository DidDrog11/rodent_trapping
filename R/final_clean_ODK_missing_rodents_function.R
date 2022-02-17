final_cleaning <- function(trap_data = all_traps, rodent_data = all_rodents, site_data = ODK_sites$site_habitats) {
  
  # Add the rodent ID's back into the trap data
  clean_sites <- trap_data %>%
    dplyr::select(-rodent_uid) %>%
    left_join(., rodent_data %>%
                dplyr::select(trap_uid, rodent_uid),
              by = "trap_uid") %>%
    mutate(habitat_group = recode(habitat, !!!clean_habitat_group),
           habitat = recode(habitat, !!!clean_habitat),
           rodent_trapped = case_when(rodent_trapped == "yes" ~ "yes",
                                      TRUE ~ "no")) %>%
    left_join(., site_data %>%
                mutate(visit_number = as_factor(visit_number),
                       site = as_factor(site)) %>%
                select(site_habitat = habitat, village, visit = visit_number, grid_number = site),
              by = c("village", "visit", "grid_number")) %>%
    mutate(research_visit = case_when(village %in% c("bambawo", "lambayama", "baiama") ~ as.numeric(visit) + 1,
                                      village %in% c("lalehun", "seilama") ~ as.numeric(visit) - 1),
           research_visit = factor(research_visit, labels = c("Pilot", "1", "2", "3", "4"))) %>%
    write_csv(here("data", "clean_data", "trap_sites", paste0("trap_sites_", Sys.Date(), ".csv"))) #Read the data file from excel document and save within the repo as csv
  
  missing_rodents <- rodent_data %>%
    filter(!rodent_uid %in% clean_sites$rodent_uid)
  
  clean_data <- list("clean_sites" = clean_sites,
                     "missing_rodents" = missing_rodents)
  
  message(
    cat(
      paste(
        paste0("All trap data is available in the clean_data$clean_sites dataframe"),
        paste0("However, due to some delays in uploading data from Sierra Leone some rodent records may be temporarily unmatched."),
        paste0("Currently there are ", nrow(missing_rodents), ". They are stored in the $missing_rodents dataframe"),
        sep = "\n"))
  )
  
  return(clean_data)
}