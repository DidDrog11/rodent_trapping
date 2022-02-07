describe_rodents_trapped <- function(data = final_cleaned_rodent_data, trap_data = final_cleaned_trap_data$clean_sites) {
  
  data <- data %>%
    mutate(village = str_extract(trap_uid, "[^_]+"))
  
  number_rodents <- nrow(data)
  
  n_rodents_village <- data %>% 
    janitor::tabyl(village)
  
  n_rodents_genus <- data %>% 
    janitor::tabyl(genus)
  
  n_rodents_genus_village <- data %>% 
    janitor::tabyl(village, genus)
  
  trap_data_rodent <- data %>%
    left_join(., trap_data %>%
                select(site_habitat, rodent_uid), by = "rodent_uid") %>%
    mutate(site_habitat = case_when(site_habitat %in% c("forested", "fallow_land") ~ "Forest/fallow land",
                                    TRUE ~ str_replace(str_to_sentence(site_habitat), "_", " ")),
           village = str_to_sentence(village),
           genus = str_to_sentence(genus)) %>%
    drop_na(site_habitat) %>%
    group_by(village, genus, site_habitat) %>%
    summarise(n = n()) %>%
    group_by(genus) %>%
    mutate(total = sum(n)) %>%
    ungroup() %>%
    arrange(-total) %>%
    mutate(genus = fct_inorder(genus),
           site_habitat = factor(site_habitat, levels = c(names(trap_palette))))
  
  plot_combined <- trap_data_rodent %>%
    ggplot() +
    geom_col(aes(x = fct_rev(genus), y = n, fill = fct_rev(site_habitat))) +
    coord_flip() +
    theme_minimal() +
    scale_fill_manual(values = trap_palette) +
    labs(x = element_blank(),
         y = element_blank(),
         fill = element_blank(),
         title = "Combined")
  
  plot_village <- trap_data_rodent %>%
    ggplot() +
    geom_col(aes(x = fct_rev(genus), y = n, fill = fct_rev(site_habitat))) +
    facet_wrap(~ village, scales = "free") +
    coord_flip() +
    theme_minimal() +
    scale_fill_manual(values = trap_palette) +
    labs(x = element_blank(),
         y = element_blank(),
         fill = element_blank(),
         title = "By village")
  
  output <- list(number_rodents = number_rodents,
                 n_rodents_village = n_rodents_village,
                 n_rodents_genus = n_rodents_genus,
                 n_rodents_genus_village = n_rodents_genus_village,
                 trap_data_rodent = trap_data_rodent,
                 plot_combined = plot_combined,
                 plot_village = plot_village)
  
  return(output)
}