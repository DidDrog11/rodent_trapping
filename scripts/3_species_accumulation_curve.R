source(here::here("scripts", "0_project_library.R"))
source(here("scripts", "import_latest_data_function.R"))

latest_rodent<- latest_data("rodents", clean = T)  %>%
  dplyr::select(rodent_id, initial_species_id)
latest_trapsite <- latest_data("trap_sites")

species_acc <- latest_trapsite %>%
  left_join(., latest_rodent, by = "rodent_id")

number_trapnights <- species_acc %>%
  group_by(visit, village) %>%
  summarise(n_trapnights = n()) %>%
  ungroup()

species <- species_acc %>%
  group_by(village, visit, initial_species_id) %>%
  tally %>%
  drop_na(initial_species_id) %>%
  arrange(-n) %>%
  ungroup()

village_acc <- list(Lalehun_1 = c(number_trapnights %>%
                                    filter(village == "lalehun" & visit == 1) %>%
                                    pull(n_trapnights), 
                                  species %>%
                                    filter(village == "lalehun" & visit == 1) %>%
                                    pull(n)),
                    Lalehun_2 = c(number_trapnights %>%
                                    filter(village == "lalehun" & visit == 2) %>%
                                    pull(n_trapnights), 
                                  species %>%
                                    filter(village == "lalehun" & visit == 2) %>%
                                    pull(n)),
                    Seilama_1 = c(number_trapnights %>%
                                    filter(village == "seilama" & visit == 1) %>%
                                    pull(n_trapnights), 
                                  species %>%
                                    filter(village == "seilama" & visit == 1) %>%
                                    pull(n)),
                    Seilama_2 = c(number_trapnights %>%
                                    filter(village == "seilama" & visit == 2) %>%
                                    pull(n_trapnights), 
                                  species %>%
                                    filter(village == "seilama" & visit == 2) %>%
                                    pull(n)),
                    Bambawo_1 = c(number_trapnights %>%
                                    filter(village == "bambawo" & visit == 1) %>%
                                    pull(n_trapnights),
                                  species %>%
                                    filter(village == "bambawo" & visit == 1) %>%
                                    pull(n))#,
                    # Lambayama_1 = c(number_trapnights %>%
                    #                   filter(village == "lambayama" & visit == 1) %>%
                    #                   pull(n_trapnights),
                    #                 species %>%
                    #                   filter(village == "lambayama" & visit == 1) %>%
                    #                   summarise(n = sum(n)) %>%
                    #                   pull(n)),
                    # Baiama_1 = c(number_trapnights %>%
                    #                   filter(village == "baiama" & visit == 1) %>%
                    #                   pull(n_trapnights),
                    #                 species %>%
                    #                   filter(village == "baiama" & visit == 1) %>%
                    #                   summarise(n = sum(n)) %>%
                    #                   pull(n))
)

x_axis <- seq(1, 1300, by = 10)
inc <- iNEXT(village_acc, q = 0, datatype = "incidence_freq", size = x_axis)

plot_acc <- fortify(inc, type = 1)

plot_acc %>%
  mutate(visit = case_when(str_detect(site, "_1") ~ 1,
                           str_detect(site, "_2") ~ 2),
         site = gsub('.{2}$', '', site)) %>%
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
  labs(x = "Number of trapnights", y = "Genus diversity", colour = "Site", fill = "Site") +
  facet_wrap(~ visit) +
  theme_minimal()

ggsave(plot = last_plot(), here("reports", "figures", "species_accumulation.png"))
