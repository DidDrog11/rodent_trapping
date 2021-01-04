library("here")
source(here("scripts", "project_library.R"))
source(here("scripts", "download_data.R"))

trap_sites <- read_csv(here("data", "trap_sites.csv"))
traps <- trap_sites %>%
  mutate(habitat = recode(habitat,
         "developing_banana_plantation" = "proximal_agriculture",
         "palm_plantation" = "proximal_agriculture",
         "cassava_plantation" = "distal_agriculture",
         "cocaoo_coffee_plantation" = "distal_agriculture",
         "harvested_dry_rice_farm" = "distal_agriculture",
         "disturbed_forest, fallow_5y" = "forest/fallow",
         "forest_fallow" = "forest/fallow",
         "village_periphery" = "village")) %>%
  group_by(habitat)

N <- length(traps$trap_uid)

traps %>%
  summarise(proportion = n()/N)
traps %>%
  group_by(village) %>%
  summarise(n = n()/N)

location_rodents <- trapped_rodents %>%
  select(rodent_id, trap_night, trap_id, initial_species_id) %>%
  left_join(., traps, 
            by = c("rodent_id")) %>%
  select(rodent_id, trap_night, initial_species_id, village, habitat)

location_rodents %>%
  group_by(village, habitat) %>%
  summarise(n = n())
            
location_rodents %>%
  group_by(initial_species_id, village) %>%
  summarise(species_n = n())
