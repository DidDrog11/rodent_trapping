describe_rodents_trapped <- function(data = final_cleaned_rodent_data, trap_data = final_cleaned_trap_data$clean_sites) {
  
  data <- data %>%
    mutate(village = str_extract(trap_uid, "[^_]+"))
  
  number_rodents <- nrow(data)
  
  n_rodents_village <- data %>% 
    janitor::tabyl(village)
  
  n_rodents_genus <- data %>% 
    janitor::tabyl(genus)
  
  rodent_order <- n_rodents_genus %>%
    arrange(-n) %>%
    pull(genus)
  
  genus_plot <- data %>%
    select(village, genus) %>%
    mutate(genus = factor(genus, levels = rodent_order, labels = str_to_sentence(rodent_order)),
           village = factor(str_to_sentence(village), levels = names(village_palette))) %>%
    ggplot() +
    geom_bar(aes(y = fct_rev(genus), fill = village)) +
    scale_fill_manual(values = village_palette) +
    theme_minimal() +
    labs(x = element_blank(),
         y = element_blank(),
         fill = "Village")
  
  n_rodents_genus_village <- data %>% 
    janitor::tabyl(village, genus)
  
  trap_data_rodent <- data %>%
    left_join(., trap_data %>%
                select(site_habitat, rodent_uid), by = "rodent_uid") %>%
    mutate(site_habitat = case_when(site_habitat %in% c("forested", "fallow_land") ~ "Forest/fallow land",
                                    TRUE ~ str_replace(str_to_sentence(site_habitat), "_", " ")),
           village = str_to_sentence(village),
           genus = factor(genus, levels = rodent_order),
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
  
  age_genus_data <- trap_data %>%
    left_join(., data %>%
                select(-notes, -key), by = c("village", "rodent_uid", "trap_uid")) %>%
    filter(!is.na(rodent_uid)) %>%
    mutate(site_habitat = case_when(site_habitat %in% c("forested", "fallow_land") ~ "Forest/fallow land",
                                    TRUE ~ str_replace(str_to_sentence(site_habitat), "_", " ")),
           genus = factor(genus, levels = rodent_order, labels = str_to_sentence(rodent_order)),
           age = case_when(sex == "female" & str_detect(vagina_perforate, "open|yes") ~ "adult",
                           sex == "female" & str_detect(vagina_perforate, "not_open|no") ~ "juvenile",
                           sex == "female" & str_detect(vagina_perforate, "not_recorded|na|not_noted") ~ "unknown",
                           sex == "male" & str_detect(genus, "Crocidura") & str_detect(seminal_vesicles, "developed") ~ "adult",
                           sex == "male" & str_detect(genus, "Crocidura") & str_detect(seminal_vesicles, "undeveloped") ~ "juvenile",
                           sex == "male" & str_detect(genus, "Crocidura") & str_detect(seminal_vesicles, "not_noted|na|yes|no") ~ "unknown",
                           sex == "male" & !str_detect(genus, "Crocidura") & str_detect(testes, "external|outside") ~ "adult",
                           sex == "male" & !str_detect(genus, "Crocidura") & str_detect(testes, "internal|inside") ~ "juvenile",
                           sex == "male" & !str_detect(genus, "Crocidura") & str_detect(testes, "not_noted|na") ~ "unknown",
                           TRUE ~ "missing")) %>%
    filter(age != "missing") %>%
    group_by(village, visit, genus, sex, age) %>%
    summarise(n_individuals = n()) %>%
    mutate(n_individuals = case_when(sex == "male" ~ 1-n_individuals,
                                     TRUE ~ as.numeric(n_individuals)),
           age = factor(age, levels = c("unknown", "juvenile", "adult", ""), labels = c("Unknown", "Juvenile", "Adult", "")),
           visit = factor(visit, levels = c(1:max(as.numeric(visit)))))
  
  x_scale <- c(age_genus_data %>% 
                 group_by(genus, sex, age) %>%
                 summarise(total = sum(n_individuals)) %>%
                 ungroup() %>%
                 filter(total == max(total)) %>%
                 pull(total) %>%
                 round(., -1) + 5)
  
  breaks <- seq(from = -abs(x_scale) + 5, to = x_scale - 5, by = 10)
  
  age_genus_plot <- ggplot(data = age_genus_data) +
    geom_col(aes(x = n_individuals, y = age, fill = fct_rev(visit))) +
    scale_x_continuous(limits = c(-abs(x_scale), x_scale), breaks = breaks, labels = abs(breaks)) +
    scale_y_discrete(drop = FALSE) +
    scale_fill_viridis_d() +
    annotate("text", x = -abs(x_scale) + 5, y = 4, label = "Male", alpha = 0.5) +
    annotate("text", x = abs(x_scale) - 5, y = 4, label = "Female", alpha = 0.5) +
    geom_vline(aes(xintercept = 0), colour = "black", alpha = 0.5) +
    theme_minimal() +
    labs(y = element_blank(),
         x = element_blank(),
         fill = "Visit") +
    facet_wrap(~ genus)
  
  output <- list(number_rodents = number_rodents,
                 n_rodents_village = n_rodents_village,
                 n_rodents_genus = n_rodents_genus,
                 n_rodents_genus_village = n_rodents_genus_village,
                 genus_plot = genus_plot,
                 trap_data_rodent = trap_data_rodent,
                 plot_combined = plot_combined,
                 plot_village = plot_village,
                 rodent_pop_pyramid = age_genus_plot)
  
  return(output)
}