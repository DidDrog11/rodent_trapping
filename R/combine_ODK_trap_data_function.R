combine_ODK_data <- function(trap = ODK_traps$full_trap_locations, check = ODK_trap_check, rodents = ODK_rodents) {
  
  combined_data <- trap %>%
    left_join(., rodents %>%
                dplyr::select(trap_uid, rodent_uid),
              by = "trap_uid") %>%
    left_join(., check %>%
                dplyr::select(village, visit, trap_night, weather) %>%
                distinct(village, visit, trap_night, .keep_all = TRUE),
              by = c("village", "visit", "trap_night")) %>%
    left_join(., check %>%
                dplyr::select(trap_uid, bait_present, trap_sprung, rodent_trapped),
              by = "trap_uid") %>%
    mutate(bait_present = case_when(is.na(bait_present) ~ "yes",
                                    bait_present == "y" ~ "yes",
                                    bait_present == "n" ~ "no",
                                    TRUE ~ bait_present),
           trap_sprung = case_when(is.na(trap_sprung) ~ "no",
                                   trap_sprung == "y" ~ "yes",
                                   trap_sprung == "n" ~ "no",
                                   TRUE ~ trap_sprung),
           rodent_trapped = case_when(rodent_trapped == "y" ~ "yes",
                                      rodent_trapped == "n" ~ "no",
                                      !is.na(rodent_uid) ~ "yes",
                                      TRUE ~ "no"))
  
  unaligned_rodents <- trap %>%
    anti_join(rodents %>%
                dplyr::select(trap_uid, rodent_uid),
              .,
              by = "trap_uid")
  
  message(paste(nrow(unaligned_rodents), "rodents are unmatched based on trap_uid. Currently 16 rodents are known to be unmatched due to delay in uploading trap data for visit 4 in Seilama and Lalehun"))
  
  return(combined_data)
}