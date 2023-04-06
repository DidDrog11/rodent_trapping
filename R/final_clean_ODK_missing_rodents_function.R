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
    mutate(habitat_group = case_when(village == "bambawo" & grid_number == "1" ~ "forest/fallow",
                                     village == "bambawo" & grid_number == "2" ~ "distal_agriculture",
                                     village == "bambawo" & grid_number == "3" ~ "forest/fallow",
                                     village == "bambawo" & grid_number == "4" ~ "proximal_agriculture",
                                     village == "baiama" & grid_number == "1" ~ "forest/fallow",
                                     village == "baiama" & grid_number == "2" & visit %in% c("1", "3") ~ "forest/fallow",
                                     village == "baiama" & grid_number == "2" & visit %in% c("2", "4") ~ "distal_agriculture", # site became agricultural after clearing
                                     village == "baiama" & grid_number == "3" ~ "distal_agriculture",
                                     village == "baiama" & grid_number == "4" ~ "proximal_agriculture",
                                     village == "baiama" & grid_number == "7" ~ "village",
                                     village == "lalehun" & grid_number == "1" ~ "proximal_agriculture",
                                     village == "lalehun" & grid_number == "2" ~ "proximal_agriculture",
                                     village == "lalehun" & grid_number == "3" ~ "proximal_agriculture",
                                     village == "lalehun" & grid_number == "4" ~ "forest/fallow",
                                     village == "lalehun" & grid_number == "5" ~ "distal_agriculture",
                                     village == "lalehun" & grid_number %in% c("6", "7") ~ "village",
                                     village == "lambayama" & grid_number == "1" ~ "proximal_agriculture",
                                     village == "lambayama" & grid_number == "2" ~ "proximal_agriculture",
                                     village == "lambayama" & grid_number == "3" ~ "forest/fallow",
                                     village == "lambayama" & grid_number == "4" ~ "proximal_agriculture",
                                     village == "lambayama" & grid_number == "7" ~ "village",
                                     village == "seilama" & grid_number == "1" ~ "proximal_agriculture",
                                     village == "seilama" & grid_number == "2" ~ "proximal_agriculture",
                                     village == "seilama" & grid_number == "3" ~ "distal_agriculture",
                                     village == "seilama" & grid_number == "4" ~ "distal_agriculture",
                                     village == "seilama" & grid_number == "5" ~ "forest/fallow",
                                     village == "seilama" & grid_number %in% c("6", "7") ~ "village",
                                     TRUE ~ habitat_group)) %>%
    write_csv(here("data", "clean_data", "trap_sites", paste0("trap_sites_", Sys.Date(), ".csv"))) #Read the data file from excel document and save within the repo as csv
  
  deduplicate_clean_sites <- clean_sites %>%
    distinct(date_set, village, visit, trap_night, grid_number, trap_number, rodent_uid, .keep_all = TRUE) %>%
    group_by(village, visit, grid_number, trap_number) %>%
    mutate(n = n())
  
  missing_rodents <- rodent_data %>%
    filter(!rodent_uid %in% clean_sites$rodent_uid)
  
  clean_data <- list("clean_sites" = deduplicate_clean_sites,
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
