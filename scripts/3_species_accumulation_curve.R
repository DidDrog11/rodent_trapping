source(here::here("scripts", "0_project_library.R"))

latest_rodent<- latest_data("rodents", clean = T)  %>%
  dplyr::select(rodent_id, initial_genus_id)
latest_trapsite <- latest_data("trap_sites")

species_acc <- latest_trapsite %>%
  left_join(., latest_rodent, by = "rodent_id")

number_trapnights <- species_acc %>%
  group_by(village) %>%
  summarise(n_trapnights = n()) %>%
  ungroup()

species <- species_acc %>%
  group_by(village, initial_genus_id) %>%
  tally %>%
  drop_na(initial_genus_id) %>%
  arrange(-n) %>%
  ungroup()

village_acc <- list(Lalehun = c(number_trapnights %>%
                                    filter(village == "lalehun") %>%
                                    pull(n_trapnights), 
                                  species %>%
                                    filter(village == "lalehun") %>%
                                    pull(n)),
                    Seilama = c(number_trapnights %>%
                                    filter(village == "seilama") %>%
                                    pull(n_trapnights), 
                                  species %>%
                                    filter(village == "seilama") %>%
                                    pull(n)),
                    Bambawo = c(number_trapnights %>%
                                    filter(village == "bambawo") %>%
                                    pull(n_trapnights),
                                  species %>%
                                    filter(village == "bambawo") %>%
                                    pull(n)) #,
                    # Lambayama = c(number_trapnights %>%
                    #                   filter(village == "lambayama") %>%
                    #                   pull(n_trapnights),
                    #                 species %>%
                    #                   filter(village == "lambayama") %>%
                    #                   summarise(n = sum(n)) %>%
                    #                   pull(n)),
                    # Baiama = c(number_trapnights %>%
                    #                   filter(village == "baiama") %>%
                    #                   pull(n_trapnights),
                    #                 species %>%
                    #                   filter(village == "baiama") %>%
                    #                   summarise(n = sum(n)) %>%
                    #                   pull(n))
)

x_axis <- seq(1, 3500, by = 10)
inc <- iNEXT(village_acc, q = 0, datatype = "incidence_freq", size = x_axis)

plot_acc <- fortify(inc, type = 1)

plot_acc %>%
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
       x = "Number of trapnights", y = "Genus diversity", colour = "Site", fill = "Site",
       caption = "Lambayama and Baiama estimates currently unstable") +
  theme_minimal()

ggsave(plot = last_plot(), here("reports", "figures", "species_accumulation.png"))
