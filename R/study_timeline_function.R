study_timeline <- function(data = final_cleaned_trap_data$clean_sites) {
  
  activity_summary <- data %>%
    group_by(village, visit, date_set) %>%
    summarise(n = n()) %>%
    group_by(village, visit) %>%
    mutate(start = min(date_set),
           end = max(date_set),
           trap_nights = sum(n)) %>%
    ungroup() %>%
    distinct(village, visit, start, end, trap_nights) %>%
    mutate(village = factor(str_to_sentence(village), levels = c("Lalehun", "Seilama", "Bambawo", "Lambayama", "Baiama")),
           research_visit = case_when(village %in% c("Bambawo", "Lambayama", "Baiama") ~ as.numeric(visit) + 1,
                                     village %in% c("Lalehun", "Seilama") ~ as.numeric(visit) - 1),
           research_visit = factor(research_visit, labels = c("Pilot", "1", "2", "3", "4")))
  
  timeline_plot <- ggplot(activity_summary) +
    geom_rect(aes(xmin = start, xmax = end, ymin = as.numeric(village) - 0.5, ymax = as.numeric(village) + 0.5, alpha = trap_nights, fill = village)) +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5), labels = c("Lalehun", "Seilama", "Bambawo", "Lambayama", "Baiama")) +
    scale_fill_manual(values = village_palette, guide = guide_legend(reverse = TRUE)) +
    labs(fill = element_blank(),
         alpha = "Trap nights") +
    annotate(geom = "rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-10-30"), ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) +
    theme_minimal()
    
  save_plot(here("output", "figures", "timeline_plot.png"), timeline_plot, base_width = 10, base_height = 4)
  
  return(timeline_plot)
}