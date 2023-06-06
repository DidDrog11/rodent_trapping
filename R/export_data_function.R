save_for_chapters <- function(trap_data = final_cleaned_trap_data$spatial_data, rodent_data = final_rodent_data) {
  
  raw_detections <- rodent_data %>%
    mutate(clean_names = field_id) %>%
    filter(trap_uid %in% trap_data$trap_uid) %>% # only keep rodents for sites with coordinates
    # Currently 699 rodents
    filter(!str_detect(rodent_uid, "BAM")) %>% # remove Bambawo
    # Leaves 684 rodents
    drop_na(clean_names) %>% # remove individuals without a genus identification
    # Leaves 684 rodents
    mutate(village = str_split(as.character(trap_uid), "_", simplify = TRUE)[, 1], # Extract location data for these rodents
           visit = as.numeric(str_split(as.character(trap_uid), "_", simplify = TRUE)[, 2]),
           trap_night = as.numeric(str_split(as.character(trap_uid), "_", simplify = TRUE)[, 3]),
           grid_number = as.numeric(str_split(as.character(trap_uid), "_", simplify = TRUE)[, 4]),
           grid_number = case_when(grid_number == 6 ~ 7,
                                   TRUE ~ grid_number),
           trap_number = as.numeric(str_split(as.character(trap_uid), "_", simplify = TRUE)[, 5]),
           clean_names = case_when(qc == "Passed" ~ species,
                                   qc == "Failed" & str_split(species, "_", simplify = TRUE)[ , 1] == genus ~ species,
                                   TRUE ~ NA),
           clean_names = coalesce(clean_names, field_id)) 
  
  confirmed_species_summary <- raw_detections %>%
    filter(qc == "Passed") %>%
    group_by(species, age_group) %>%
    mutate(genus = str_split(species, "_", simplify = TRUE)[ ,1]) %>%
    summarise(n = n(),
              min_weight = min(weight, na.rm = TRUE),
              max_weight = max(weight, na.rm = TRUE),
              median_weight = median(weight, na.rm = TRUE),
              min_hb = min(head_body, na.rm = TRUE),
              max_hb = max(head_body, na.rm = TRUE),
              median_hb = median(head_body, na.rm = TRUE),
              min_hf = min(hind_foot, na.rm = TRUE),
              max_hf = max(hind_foot, na.rm = TRUE),
              median_hf = median(hind_foot, na.rm = TRUE),
              min_ear = min(ear, na.rm = TRUE),
              max_ear = max(ear, na.rm = TRUE),
              median_ear = median(ear, na.rm = TRUE),
              min_skull = min(length_skull, na.rm = TRUE),
              max_skull = max(length_skull, na.rm = TRUE),
              median_skull = median(length_skull, na.rm = TRUE))
  
  unconfirmed_species <- raw_detections %>%
    filter(qc != "Passed" | is.na(qc)) %>%
    mutate(clean_names = case_when(qc == "Failed" & str_detect(species, genus) ~ species,
                                   genus == "lophuromys" ~ "lophuromys_sikapusi",
                                   genus == "lemniscomys" ~ "lemniscomys_striatus",
                                   genus == "malacomys" ~ "malacomys_edwardsi",
                                   genus == "mastomys" ~ "mastomys_natalensis",
                                   genus == "mus" & village == "lambayama" ~ "mus_musculus",
                                   genus == "mus" & weight <= 40 & ear <= 20 ~ "mus_setulosus",
                                   genus == "praomys" ~ "praomys_rostratus",
                                   genus == "rattus" ~ "rattus_rattus",
                                   genus == "hybomys" ~ "hybomys_planifrons",
                                   genus == "hylomyscus" ~ "hylomyscus_simus",
                                   genus == "gerbilliscus" ~ "gerbilliscus_guineae",
                                   genus == "dasymys" ~ "dasymys_rufulus",
                                   genus == "crocidura" & weight <= 18 & head_body <= 90 ~ "crocidura_grandiceps",
                                   genus == "crocidura" & weight <= 25 & head_body <= 100 ~ "crocidura_buettikoferi",
                                   genus == "crocidura" ~ "crocidura_olivieri"))
  
  rodent_data <- bind_rows(raw_detections %>%
                             filter(qc == "Passed"),
                           unconfirmed_species)
  
  combined_data <- list(trap_data = trap_data,
                        rodent_data = rodent_data)
  
  write_rds(combined_data, here("data", "data_for_export", "combined_data.rds"))
  
}
