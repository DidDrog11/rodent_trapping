plot_trap_success <- function(data = final_cleaned_trap_data$clean_sites) {
  
  plot_data <- data %>%
    tibble() %>%
    mutate(trap_success = case_when(!is.na(rodent_uid) ~ "Success",
                                    TRUE ~ "Failure")) %>%
    group_by(village, visit) %>%
    count(trap_success) %>%
    mutate(perc_success = n/sum(n) * 100,
           village = str_to_sentence(village)) %>%
    group_by(village) %>%
    mutate(mean_trap_success = sum(n[trap_success == "Success"])/sum(n) * 100)
  
  plot <- ggplot(plot_data) +
    geom_col(aes(x = visit, y = perc_success, fill = trap_success, group = village), position = "dodge") +
    stat_summary(fun = mean, geom = "hline", colour = "black", linetype = 2) +
    geom_text(aes(x = as.numeric(visit) + 1, y = unique(mean_trap_success), label = unique(mean_trap_success), vjust = -1)) +
    facet_wrap(~ village) +
    scale_fill_viridis_d(alpha = c(0.5, 1)) +
    labs(x = "Visit",
         y = "% of trap nights",
         fill = element_blank()) +
    theme_minimal()
  
  
}