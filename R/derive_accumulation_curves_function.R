derive_accumulation_curves <- function(rodent_data = final_cleaned_rodent_data, trap_data = final_cleaned_trap_data$clean_sites) {
  
  species_accumulation <- trap_data %>%
    dplyr::select(village, trap_uid, rodent_uid) %>%
    left_join(., rodent_data %>%
                dplyr::select(rodent_uid, trap_uid, genus), by = c("rodent_uid", "trap_uid"))
  
  number_trapnights <- species_accumulation %>%
    group_by(village) %>%
    summarise(n_trapnights = n()) %>%
    ungroup()
  
  species <- species_accumulation %>%
    group_by(village, genus) %>%
    tally %>%
    drop_na(genus) %>%
    arrange(-n) %>%
    ungroup()
  
  village_acc <- function(village_names) {
    
    village_acc <- list(try(c(number_trapnights %>%
                                filter(village == village_names[1]) %>%
                                pull(n_trapnights), 
                              species %>%
                                filter(village == village_names[1]) %>%
                                pull(n))),
                        try(c(number_trapnights %>%
                                filter(village == village_names[2]) %>%
                                pull(n_trapnights), 
                              species %>%
                                filter(village == village_names[2]) %>%
                                pull(n))),
                        try(c(number_trapnights %>%
                                filter(village == village_names[3]) %>%
                                pull(n_trapnights), 
                              species %>%
                                filter(village == village_names[3]) %>%
                                pull(n))),
                        try(c(number_trapnights %>%
                                filter(village == village_names[4]) %>%
                                pull(n_trapnights), 
                              species %>%
                                filter(village == village_names[4]) %>%
                                pull(n))),
                        try(c(number_trapnights %>%
                                filter(village == village_names[5]) %>%
                                pull(n_trapnights), 
                              species %>%
                                filter(village == village_names[5]) %>%
                                pull(n))))
    
    names(village_acc) <- village_names
    
    remove_unstable <- sapply(village_acc, function(x) length(x) > 2)
    
    x_axis <- seq(0, round(max(number_trapnights$n_trapnights) + 250, -2), 10)
    
    inc <- iNEXT(village_acc[remove_unstable], q = 0, datatype = "incidence_freq", size = x_axis)
    
    return(inc)
  }
  
  village_accumulation <- village_acc(village_names = c("lalehun", "seilama", "baiama", "bambawo", "lambayama"))
  
  plot_accumulation <- function(accumulation_data) {
    
    plot_acc <- fortify(accumulation_data, type = 1)
    
    accumulation_plot <- plot_acc %>%
      mutate(site = str_to_sentence(site)) %>%
      ggplot(aes(x = x, y = y, colour = site)) +
      geom_point(data = . %>%
                   filter(method == "observed"),
                 aes(x = x, y = y)) +
      geom_line(aes(linetype = method)) +
      scale_linetype_manual(values = c("dotdash", "solid", "solid"), guide = waiver()) +
      geom_ribbon(aes(ymin = y.lwr, ymax = y.upr,
                      fill = site, colour = NULL), alpha=0.2) +
      scale_fill_manual(values = village_palette) +
      scale_colour_manual(values = village_palette) +
      labs(title = "Genus accumulation",
           x = "Number of trapnights", y = "Genus diversity", colour = "Site", fill = "Site") +
      theme_minimal()
    
    return(accumulation_plot)
    
  }
  
  all_sites_accumulation <- plot_accumulation(accumulation_data = village_accumulation)
  
  return(all_sites_accumulation)
}
