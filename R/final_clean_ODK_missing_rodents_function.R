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
    mutate(research_visit = case_when(village %in% c("lalehun", "seilama") & visit == "1" ~ 0,
                                      village %in% c("lalehun", "seilama") & visit == "2" ~ 1,
                                      village %in% c("lalehun", "seilama") & visit == "3" ~ 2,
                                      village %in% c("lalehun", "seilama") & visit == "4" ~ 3,
                                      village %in% c("lalehun", "seilama") & visit == "5" ~ 4,
                                      village %in% c("lalehun", "seilama") & visit == "6" ~ 5,
                                      village %in% c("lalehun", "seilama") & visit == "7" ~ 6,
                                      village %in% c("lalehun", "seilama") & visit == "8" ~ 7,
                                      village %in% c("bambawo", "baiama", "lambayama") & visit == "1" ~ 2,
                                      village %in% c("bambawo", "baiama", "lambayama") & visit == "2" ~ 3,
                                      village %in% c("bambawo", "baiama", "lambayama") & visit == "3" ~ 4,
                                      village %in% c("bambawo", "baiama", "lambayama") & visit == "4" ~ 5,
                                      village %in% c("bambawo", "baiama", "lambayama") & visit == "7" ~ 6,
                                      village %in% c("bambawo", "baiama", "lambayama") & visit == "8" ~ 7,
                                      TRUE ~ as.numeric(visit)),
           research_visit = factor(research_visit, levels = c(0, 1, 2, 3, 4, 5, 6, 7), labels = c("Pilot", "1", "2", "3", "4", "5", "6", "7"))) %>%
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
                                     TRUE ~ habitat_group),
           site_habitat = case_when(village == "bambawo" & grid_number == "1" ~ "forest",
                                    village == "bambawo" & grid_number == "2" ~ "mixed_agriculture",
                                    village == "bambawo" & grid_number == "3" ~ "old_quarry",
                                    village == "bambawo" & grid_number == "4" ~ "mixed_agriculture",
                                    village == "baiama" & grid_number == "1" ~ "forest",
                                    village == "baiama" & grid_number == "2" & visit %in% c("1", "3") ~ "fallow",
                                    village == "baiama" & grid_number == "2" & visit %in% c("2", "4") ~ "cassava_field",
                                    village == "baiama" & grid_number == "3" ~ "mixed_agriculture",
                                    village == "baiama" & grid_number == "4" ~ "banana_plantation",
                                    village == "baiama" & grid_number == "7" & trap_land_type == "indoors" ~ "village_inside",
                                    village == "baiama" & grid_number == "7" ~ "village_outside",
                                    village == "lalehun" & grid_number == "1" ~ "mixed_agriculture",
                                    village == "lalehun" & grid_number == "2" ~ "rice_field",
                                    village == "lalehun" & grid_number == "3" ~ "mixed_agriculture",
                                    village == "lalehun" & grid_number == "4" ~ "forest",
                                    village == "lalehun" & grid_number == "5" ~ "cassava_field",
                                    village == "lalehun" & grid_number %in% c("6", "7") & trap_land_type == "indoors" ~ "village_inside",
                                    village == "lalehun" & grid_number %in% c("6", "7") ~ "village_outside",
                                    village == "lambayama" & grid_number == "1" ~ "rice_field",
                                    village == "lambayama" & grid_number == "2" ~ "fallow",
                                    village == "lambayama" & grid_number == "3" ~ "fallow",
                                    village == "lambayama" & grid_number == "4" ~ "mixed_agriculture",
                                    village == "lambayama" & grid_number == "7" & trap_land_type == "indoors" ~ "village_inside",
                                    village == "lambayama" & grid_number == "7" ~ "village_outside",
                                    village == "seilama" & grid_number == "1" ~ "palm_plantation",
                                    village == "seilama" & grid_number == "2" ~ "mixed_agriculture",
                                    village == "seilama" & grid_number == "3" ~ "rice_field",
                                    village == "seilama" & grid_number == "4" ~ "rice_field",
                                    village == "seilama" & grid_number == "5" ~ "forest",
                                    village == "seilama" & grid_number %in% c("6", "7") & trap_land_type == "indoors" ~ "village_inside",
                                    village == "seilama" & grid_number %in% c("6", "7") ~ "village_outside",
                                    TRUE ~ site_habitat)) %>%
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