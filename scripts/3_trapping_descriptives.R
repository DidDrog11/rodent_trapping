source(here::here("scripts", "0_project_library.R"))

latest_rodent<- latest_data("rodents", clean = T)
latest_trapsite <- latest_data("trap_sites")

current_status <- latest_trapsite %>%
  group_by(village, grid_number) %>%
  mutate(habitat_group = recode(habitat,
                                "developing_banana_plantation" = "proximal_agriculture",
                                "palm_plantation" = "proximal_agriculture",
                                "cocoa_outskirts" = "proximal_agriculture",
                                "rice_field" = "proximal_agriculture",
                                "banana_plantation" = "proximal_agriculture",
                                "cocao_coffee_plantation" = "proximal_agriculture",
                                "dry_rice" = "proximal_agriculture",
                                "wet_rice" = "distal_agriculture",
                                "cassava_plantation" = "distal_agriculture",
                                "cacao_coffee_plantation" = "distal_agriculture",
                                "harvested_dry_rice_farm" = "distal_agriculture",
                                "disturbed_forest, fallow_5y" = "forest/fallow",
                                "forest_fallow" = "forest/fallow",
                                "village_periphery" = "village",
                                "houses" = "village",
                                "forest" = "forest/fallow",
                                "fallow_land" = "forest/fallow",
                                "forested" = "forest/fallow",
                                "fallow_land open_land" = "forest/fallow",
                                "open_land fallow_land" = "forest/fallow",
                                "open_land" = "forest/fallow",
                                "village_inside" = "village",
                                "open_land village_inside" = "village",
                                "village_outside" = "village",
                                "open_land agricultural" = "proximal_agriculture",
                                "agricultural forested" = "distal_agriculture",
                                "agricultural open_land" = "proximal_agriculture",
                                "forested fallow_land" = "forest/fallow",
                                "forested fallow_land open_land" = "forest/fallow",
                                "fallow_land forested" = "forest/fallow",
                                "forested open_land" = "forest/fallow"),
         habitat_group = recode_factor(habitat_group, !!!habitat_names),
         village = str_to_sentence(village)) %>%
  group_by(habitat_group)

trap_nights <- current_status %>%
  group_by(village) %>%
  summarise(n = n()) %>%
  mutate(label = paste0(str_to_sentence(village), " N = ", n))

ggplot(current_status %>%
         left_join(., trap_nights,
                   by = "village") %>%
         drop_na(habitat_group)) +
  geom_bar(aes(x = label, fill = habitat_group), position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  scale_fill_manual(values = trap_palette) +
  labs(title = "Proportion of trap-nights obtained from each habitat",
       fill = "Habitat",
       x = "Village",
       y = "Percentage") +
  coord_flip()

trap_success <- current_status %>%
  filter(rodent_trapped != "na") %>%
  drop_na(rodent_trapped, habitat_group) %>%
  mutate(village = snakecase::to_sentence_case(village)) %>%
  group_by(trap_night, village, habitat_group, rodent_trapped) %>%
  summarise(n = n()) %>%
  mutate(proportion = n/sum(n)) %>%
  ggplot() +
  geom_col(aes(x = trap_night, y = proportion, fill = village, alpha = rodent_trapped), position = "dodge") +
  facet_wrap(habitat_group ~ ., scales = "free") +
  theme_minimal() +
  scale_fill_manual(values = village_palette) +
  scale_alpha_discrete(labels = c("Trap empty", "Rodent trapped", "NA"),
                       range = c(0.05,1, 0.05)) +
  labs(fill = "Village",
       alpha = "Trap status",
       x = "Trap night",
       y = "Proportion")

ggsave2(here("reports", "figures", "trap_success_rate.png"), trap_success)  

latest_rodent %>%
  mutate(initial_species_id = snakecase::to_sentence_case(as.character(initial_species_id)),
         initial_species_id = case_when(initial_species_id == "Proamys spp" ~ "Praomys spp",
                                        initial_species_id == "Shrew spp" ~ "Crocidura spp",
                                        TRUE ~ initial_species_id)) %>%
  ggplot() +
  geom_bar(aes(x = fct_rev(fct_infreq(initial_species_id)), fill = str_to_sentence(village))) +
  theme_minimal() +
  scale_fill_manual(values = village_palette) +
  coord_flip() +
  labs(fill = "Village",
       y = element_blank(),
       x = "Species")

ggsave2(here("reports", "figures", "species_caught.png"), last_plot())

latest_rodent %>%
  mutate(genus = snakecase::to_sentence_case(as.character(genus)),
         genus = fct_infreq(as_factor(case_when(genus == "Proamys spp" ~ "Praomys",
                                                genus == "Dasymys spp" ~ "Dasymys",
                                                genus == "Lophuromys sikapusi" ~ "Lophuromys",
                                                genus == "Mastomys natalensis" ~ "Mastomys",
                                                genus == "Shrew spp" ~ "Crocidura",
                                                TRUE ~ genus)))) %>%
  group_by(genus, habitat_group, village) %>%
  summarise(n = n()) %>%
  group_by(genus) %>%
  mutate(trapped = sum(n)) %>%
  arrange(-trapped) %>%
  ggplot() +
  geom_waffle(aes(fill = habitat_group, values = n, alpha = village), color = "white", make_proportional = F, n_rows = 2, show.legend = T) +
  facet_wrap(~ genus) +
  scale_fill_manual(values = trap_palette) +
  theme_minimal() +
  theme_enhance_waffle() +
  labs(fill = "Habitat",
       caption = "1 box per trapped individual")

ggsave2(here("reports", "figures", "species_locations.png"), last_plot())
