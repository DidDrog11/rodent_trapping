final_cleaning <- function(trap_data = all_traps, rodent_data = all_rodents) {
  
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
        paste0("Currently these are: ", knitr::combine_words(missing_rodents$rodent_uid)),
        sep = "\n"))
  )
  
  return(clean_data)
}