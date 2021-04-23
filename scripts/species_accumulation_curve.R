source(here::here("scripts", "project_library.R"))

village_palette <- c("#7a0177", "#fec44f")
names(village_palette) <-  c("Lalehun", "Seilama")

trap_data <- read_csv(here("data","trap_sites.csv"))
rodent_data <- read_csv(here("data", "rodents_trapped.csv")) %>%
  dplyr::select(rodent_id, initial_species_id)

species_acc <- trap_data %>%
  filter(visit == "1") %>%
  left_join(., rodent_data, by = "rodent_id")

number_trapnights <- c(nrow(species_acc %>%
                              filter(village == "lalehun")),
                       nrow(species_acc %>%
                              filter(village == "seilama")))
lalehun_species <- species_acc %>%
  filter(village == "lalehun") %>%
  group_by(initial_species_id) %>%
  tally %>%
  drop_na(initial_species_id) %>%
  arrange(-n)

seilama_species <- species_acc %>%
  filter(village == "seilama") %>%
  group_by(initial_species_id) %>%
  tally %>%
  drop_na(initial_species_id) %>%
  arrange(-n)

village_acc <- list(Lalehun = c(number_trapnights[1], lalehun_species$n), Seilama = c(number_trapnights[2], seilama_species$n))

x_axis <- seq(1, 1300, by = 10)
inc <- iNEXT(village_acc, q = 0, datatype = "incidence_freq", size = x_axis)

plot_acc <- fortify(inc, type = 1)

ggplot(plot_acc, aes(x = x, y = y, colour = site)) +
  geom_point(data = plot_acc %>%
               filter(method == "observed")) +
  geom_line(aes(linetype = method)) +
  scale_linetype_manual(values = c("dotdash", "solid", "solid"), guide = waiver()) +
  geom_ribbon(aes(ymin = y.lwr, ymax = y.upr,
                  fill = site, colour = NULL), alpha=0.2) +
  scale_fill_manual(values = village_palette) +
  scale_colour_manual(values = village_palette) +
  labs(x = "Number of trapnights", y = "Species diversity", colour = "Site", fill = "Site") +
  theme_minimal() +
  ggsave(here("reports", "figures", "species_accumulation.png"))
