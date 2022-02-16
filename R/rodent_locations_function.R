describe_rodent_locations <- function(spatial_data = final_cleaned_trap_data$spatial_data, rodent_data = final_cleaned_rodent_data) {
  
  combined_data <- spatial_data %>%
    left_join(., rodent_data %>%
                select(-notes, -key), by = c("rodent_uid", "trap_uid")) %>%
    mutate(site_habitat = case_when(site_habitat %in% c("forested", "fallow_land") ~ "Forest/fallow land",
                                    TRUE ~ str_replace(str_to_sentence(site_habitat), "_", " ")),
           genus = str_to_sentence(genus))
  
  presence <- combined_data %>%
    filter(!is.na(rodent_uid)) %>%
    group_by(village) %>%
    group_split()
  
  no_rodent <- combined_data %>%
    filter(is.na(rodent_uid)) %>%
    distinct(geometry, village, site_habitat) %>%
    group_by(village) %>%
    group_split()
  
  presence_maps <- list()
  
  for(i in 1:length(presence)) {
    
    presence_maps[[i]] <- ggplot(presence[[i]]) +
      geom_sf(aes(colour = site_habitat)) +
      facet_wrap(~ genus) +
      scale_colour_manual(values = trap_palette) +
      geom_sf(data = no_rodent[[i]], colour = "black", alpha = 0.05, size = 0.1) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = str_to_sentence(unique(presence[[i]]$village)))
    
    save_plot(here("output", "figures", "rodent_locations", paste0(unique(presence[[i]]$village), "_rodent_presence.png")), presence_maps[[i]], base_height = 10, base_width = 14)
      
  }
  
}