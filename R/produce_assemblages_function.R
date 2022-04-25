produce_assemblages <- function(spatial_data = final_cleaned_trap_data$spatial_data, rodent_data = final_cleaned_rodent_data, distance = 50) {
  
  rodent_data <- rodent_data %>%
    select(rodent_uid, trap_uid, clean_names, genus)
  
  spatial_data <- spatial_data %>%
    select(date_set, village, visit, grid_number, trap_uid, habitat_group, site_habitat, geometry)
  
  units(distance) <- "meters"
  
  rodent_spatial <- rodent_data %>%
    left_join(spatial_data, by = "trap_uid") %>%
    drop_na(visit) %>%
    st_as_sf(crs = st_crs(spatial_data))
  
  rodent_buffered <- rodent_spatial %>%
    st_buffer(dist = distance)
  
  individual_buffers <- rodent_buffered %>%
    group_by(rodent_uid) %>%
    group_split()
  
  assemblages <- lapply(individual_buffers, function(x) {
    
    reference_visit <- unique(x$visit)
    
    reference_species <- unique(x$clean_names)
    
    assemblage <- st_join(rodent_spatial, 
                          x %>%
                            select(rodent_uid, geometry) %>%
                            mutate(reference_rodent = "true"),
                          left = FALSE, join = st_within) %>%
      mutate(reference_rodent = case_when(rodent_uid.x != rodent_uid.y ~ "non_reference",
                                          TRUE ~ "reference"),
             same_visit = case_when(visit == reference_visit ~ TRUE,
                                    TRUE ~ FALSE)) %>%
      rename("rodent_uid" = rodent_uid.x,
             "reference_rodent_uid" = rodent_uid.y)  %>%
      filter(!clean_names %in% reference_species | reference_rodent %in% c("reference"))
    
  })
  
  combined_assemblages <- bind_rows(assemblages, .id = "assemblage") %>%
    group_by(assemblage) %>%
    mutate(assemblage = as.numeric(assemblage),
           same_visit = case_when(visit %in% visit[which(reference_rodent == "reference")] ~ TRUE,
                                  TRUE ~ FALSE))
  
  summarise_assemblages <- combined_assemblages %>%
    tibble() %>%
    group_by(assemblage, clean_names, reference_rodent_uid, reference_rodent, same_visit) %>%
    summarise(n_individuals  = n())
  
  same_visit <- summarise_assemblages %>%
    filter(same_visit == TRUE)
  
  summarise_assemblages_same_visit_plot <- same_visit %>%
    group_by(assemblage) %>%
    mutate(n_other_species = n()-1,
           clean_names = str_to_sentence(str_replace_all(clean_names, "_", " "))) %>%
    filter(reference_rodent == "reference") %>%
    ggplot(aes(x = clean_names, y = n_other_species)) + 
    geom_violin() +
    geom_jitter(width = 0.1, height = 0.1) +
    coord_flip() +
    theme_minimal() +
    labs(y = "Co-located species (N)",
         x = element_blank(),
         title = "Same visit")
  
  all_visits <- summarise_assemblages
  
  summarise_assemblages_all_visits_plot <- all_visits %>%
    group_by(assemblage) %>%
    mutate(n_other_species = n()-1,
           clean_names = str_to_sentence(str_replace_all(clean_names, "_", " "))) %>%
    filter(reference_rodent == "reference") %>%
    ggplot(aes(x = clean_names, y = n_other_species)) + 
    geom_violin() +
    geom_jitter(width = 0.1, height = 0.1) +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    coord_flip() +
    theme_minimal() +
    labs(y = "Co-located species (N)",
         x = element_blank(),
         title = "All visits")
  
  colocated_species_same_visit <- same_visit %>%
    group_by(assemblage) %>%
    mutate(reference_species = case_when(reference_rodent == "reference" ~ clean_names,
                                         TRUE ~ as.character(NA)),
           colocated_species = case_when(reference_rodent == "non_reference" ~ clean_names,
                                         TRUE ~ as.character(NA))) %>%
    tidyr::fill(reference_species, .direction = "updown") %>%
    tidyr::fill(colocated_species, .direction = "downup") %>%
    ungroup() %>%
    distinct(assemblage, reference_species, colocated_species) %>%
    mutate(colocated_species = str_to_sentence(str_replace_all(colocated_species, "_", " ")),
           reference_species = str_to_sentence(str_replace_all(reference_species, "_", " ")),
           colocated_species = replace_na(colocated_species, replace = "No co-located species")) %>%
    group_by(reference_species, colocated_species) %>%
    summarise(n = n()) %>%
    group_by(reference_species) %>%
    mutate(total = sum(n),
           proportion = round(n/total, 2))
  
  order_species <- c(colocated_species_same_visit %>% 
                       group_by(colocated_species) %>% 
                       summarise(n = n()) %>% 
                       arrange(-n) %>% 
                       pull(colocated_species),
                     "Gerbilliscus spp")
  
  colocated_species_same_visit %<>%
    mutate(reference_species = factor(reference_species, levels = order_species, labels = str_replace_all(order_species, " ", "\n")),
           colocated_species = factor(colocated_species, levels = order_species, labels = str_replace_all(order_species, " ", "\n")))
  
  colocated_species_same_visit_plot <- ggplot(colocated_species_same_visit) +
    geom_tile(aes(x = colocated_species, y = reference_species, fill = proportion, width = 0.95, height = 0.95)) +
    geom_label(aes(x = colocated_species, y = reference_species, label = proportion)) +
    scale_fill_viridis_c() +
    theme_minimal() +
    labs(y = "Reference species",
         x = "Co-located species",
         fill = "Proportion",
         title = "Same visit")
  
  colocated_species_all_visits <- all_visits %>%
    group_by(assemblage) %>%
    mutate(reference_species = case_when(reference_rodent == "reference" ~ clean_names,
                                         TRUE ~ as.character(NA)),
           colocated_species = case_when(reference_rodent == "non_reference" ~ clean_names,
                                         TRUE ~ as.character(NA))) %>%
    tidyr::fill(reference_species, .direction = "updown") %>%
    tidyr::fill(colocated_species, .direction = "downup") %>%
    ungroup() %>%
    distinct(assemblage, reference_species, colocated_species) %>%
    mutate(colocated_species = str_to_sentence(str_replace_all(colocated_species, "_", " ")),
           reference_species = str_to_sentence(str_replace_all(reference_species, "_", " ")),
           colocated_species = replace_na(colocated_species, replace = "No co-located species")) %>%
    group_by(reference_species, colocated_species) %>%
    summarise(n = n()) %>%
    group_by(reference_species) %>%
    mutate(total = sum(n),
           proportion = round(n/total, 2))
  
  colocated_species_all_visits %<>%
    mutate(reference_species = factor(reference_species, levels = order_species, labels = str_replace_all(order_species, " ", "\n")),
           colocated_species = factor(colocated_species, levels = order_species, labels = str_replace_all(order_species, " ", "\n")))
  
  colocated_species_all_visits_plot <- ggplot(colocated_species_all_visits) +
    geom_tile(aes(x = colocated_species, y = reference_species, fill = proportion, width = 0.95, height = 0.95)) +
    geom_label(aes(x = colocated_species, y = reference_species, label = proportion)) +
    scale_fill_viridis_c() +
    theme_minimal() +
    labs(y = "Reference species",
         x = "Co-located species",
         fill = "Proportion",
         title = "All visits")
  
  combined_colocation <- plot_grid(plotlist = list(colocated_species_same_visit_plot,
                                                   colocated_species_all_visits_plot),
                                   ncol = 1)
  
  return(list(summary_plots = list(same_visit = summarise_assemblages_same_visit_plot,
                                   all_visits = summarise_assemblages_all_visits_plot),
              co_location_plots = list(same_visit = colocated_species_same_visit_plot,
                                       all_visits = colocated_species_all_visits_plot,
                                       combined_plot = combined_colocation)))
  
}
