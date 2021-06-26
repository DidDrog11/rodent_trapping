source(here::here("scripts", "0_project_library.R"))

trap_data <- read_csv(here("data","trap_sites.csv"))
rodent_data <- read_csv(here("data", "rodents_trapped.csv")) 

proximal_agriculture <- c("developing_banana_plantation", "palm_plantation", "proximal_agriculture")
distal_agriculture <- c("cassava_plantation", "cacao_coffee_plantation", "harvested_dry_rice_farm", "distal_agriculture")
forest_fallow <- c("disturbed_forest, fallow_5y", "forest_fallow", "fallow_land", "forested", "fallow_land open_land", "open_land fallow_land",
                   "open_land")
village <- c("village_periphery", "village")

trap_data %>%
  group_by(village) %>%
  summarise(n = n())
  
a <- trap_data %>%
  filter(village == "bambawo") %>%
  group_by(habitat) %>%
  summarise(n = n())

current_status <- trap_data %>%
  group_by(village, grid_number) %>%
  mutate(habitat = case_when(habitat %in% proximal_agriculture ~ "Proximal agriculture",
                             habitat %in% distal_agriculture ~ "Distal agriculture",
                             habitat %in% forest_fallow ~ "Forest/fallow land",
                             TRUE ~ "Village"),
         habitat = factor(habitat, levels = c("Village", "Proximal agriculture", "Distal agriculture", "Forest/fallow land")))

ggplot(current_status) +
  geom_bar(aes(x = village, fill = habitat), position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  scale_x_discrete(labels = c("Bambawo N = 804", "Lalehun N = 1870", "Seilama N = 2256")) +
  scale_fill_manual(values = c("#7a0177","#fee391", "#fec44f",  "#00441b")) +
  labs(title = "Proportion of trap-nights obtained from each habitat",
       fill = "Habitat",
       x = "Village",
       y = "Percentage")
