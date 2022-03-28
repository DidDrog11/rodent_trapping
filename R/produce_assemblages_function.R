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
  
  same_visit %>%
    group_by(assemblage) %>%
    mutate(n_other_species = n()-1) %>%
    filter(reference_rodent == "reference") %>%
    ggplot() + 
    geom_violin(aes(x = clean_names, y = n_other_species)) +
    coord_flip()
  
  all_visits <- summarise_assemblages
  
  all_visits %>%
    group_by(assemblage) %>%
    mutate(n_other_species = n()-1) %>%
    filter(reference_rodent == "reference") %>%
    ggplot() + 
    geom_violin(aes(x = clean_names, y = n_other_species)) +
    coord_flip()
}
