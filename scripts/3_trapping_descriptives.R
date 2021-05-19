source(here::here("scripts", "0_project_library.R"))

trap_data <- read_csv(here("data","trap_sites.csv"))
rodent_data <- read_csv(here("data", "rodents_trapped.csv")) 

proximal_agriculture <- c("developing_banana_plantation", "palm_plantation", "proximal_agriculture")
distal_agriculture <- c("cassava_plantation", "cacao_coffee_plantation", "harvested_dry_rice_farm")
forest_fallow <- c("disturbed_forest, fallow_5y", "forest_fallow")
village <- c("village_periphery", "village")

pilot_study <- trap_data %>%
  filter(visit == 1) %>%
  group_by(village, grid_number) %>%
  mutate(habitat = case_when(habitat %in% proximal_agriculture ~ "Proximal agriculture",
                             habitat %in% distal_agriculture ~ "Distal agriculture",
                             habitat %in% forest_fallow ~ "Forest/fallow land",
                             TRUE ~ "Village"),
         habitat = factor(habitat, levels = c("Village", "Proximal agriculture", "Distal agriculture", "Forest/fallow land")))

ggplot(pilot_study) +
  geom_bar(aes(x = village, fill = habitat), position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  scale_x_discrete(labels = c("Lalehun N = 726", "Seilama N = 1120")) +
  scale_fill_manual(values = c("#7a0177","#fee391", "#fec44f",  "#00441b")) +
  labs(title = "Proportion of trap-nights obtained from each habitat",
       fill = "Habitat",
       x = "Village",
       y = "Percentage")
