plot_trap_success <- function(data = final_cleaned_trap_data$clean_sites, by_village = FALSE, save_plots = FALSE) {
  
  if(by_village == FALSE) {
    plot_data <- data %>%
      tibble() %>%
      mutate(trap_success = case_when(!is.na(rodent_uid) ~ "Success",
                                      TRUE ~ "Failure")) %>%
      group_by(village, visit) %>%
      count(trap_success) %>%
      mutate(perc_success = n/sum(n) * 100,
             village = str_to_sentence(village))
    
    mean_success <- plot_data %>%
      group_by(village) %>%
      summarise(perc_success = round(sum(n[trap_success == "Success"])/sum(n) * 100, 1))
    
    plot <- ggplot(plot_data) +
      geom_col(aes(x = visit, y = perc_success, fill = trap_success, group = village), position = "dodge") +
      geom_hline(data = mean_success, aes(yintercept = perc_success), linetype = 2, colour = "black") +
      geom_text(data = mean_success, aes(x = 5, y = perc_success, label = paste0(perc_success, "%"), vjust = -1)) +
      facet_wrap(~ village) +
      scale_fill_viridis_d(alpha = c(0.5, 1)) +
      labs(x = "Visit",
           y = "% of trap nights",
           fill = element_blank()) +
      theme_minimal()
  }
  
  if(by_village == TRUE) {
    
    plot_data <- data %>%
      tibble() %>%
      mutate(trap_success = case_when(!is.na(rodent_uid) ~ "Success",
                                      TRUE ~ "Failure")) %>%
      group_by(village, visit, grid_number) %>%
      count(trap_success) %>%
      mutate(perc_success = n/sum(n) * 100,
             village = str_to_sentence(village),
             grid_number = paste0("Site ", grid_number)) %>%
      group_by(village) %>%
      group_split()
    
    names(plot_data) <- levels(data$village)
    
    plot <- lapply(plot_data, function(x) {
      
      village_name = unique(x$village)
      
      max_visit = max(as.numeric(x$visit), na.rm = T) + 1
      
      plot_data <- x %>%
        expand(village, visit, grid_number, trap_success) %>%
        left_join(., x) %>%
        mutate(perc_success = case_when(is.na(perc_success) ~ 0,
                                        TRUE ~ perc_success),
               n = case_when(is.na(n) ~ as.integer(0),
                             TRUE ~ n)) %>%
        filter(as.numeric(visit) <= max_visit - 1)
      
      mean_success <- plot_data %>%
        group_by(village, grid_number) %>%
        summarise(perc_success = round(sum(n[trap_success == "Success"])/sum(n) * 100, 1))
      
      plot <- plot_data %>%
        ggplot() +
        geom_col(aes(x = visit, y = perc_success, fill = trap_success), position = "dodge") +
        geom_hline(data = mean_success, aes(yintercept = perc_success), linetype = 2, colour = "black") +
        geom_text(data = mean_success, aes(x = max_visit, y = perc_success, label = paste0(perc_success, "%"), vjust = -1, hjust = 1.2)) +
        facet_wrap(~ grid_number) +
        scale_fill_viridis_d(alpha = c(0.5, 1)) +
        labs(x = "Visit",
             y = "% of trap nights",
             fill = element_blank(),
             title = paste0(str_to_sentence(village_name), " trap success by site")) +
        theme_minimal()
      
      if(save_plots == TRUE) {save_plot(plot = plot, filename = here("output", "figures", "trap_success", paste0(str_to_lower(village_name), "_site_by_visit_success.png")),
                                        base_width = 12, base_height = 10)}
      
    })
    
  }
  
  return(plot = plot)
  
}
