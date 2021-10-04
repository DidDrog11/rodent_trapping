source(here::here("scripts", "0_project_library.R"))

trapped_rodents <-latest_data("rodents", clean = F)
latest_trapsite <- latest_data("trap_sites", clean = F)

habitat_names <- c("Forest/fallow land", "Distal agriculture", "Proximal agriculture", "Village")
names(habitat_names) = c("forest/fallow", "distal_agriculture", "proximal_agriculture", "village")

trap_sites <- latest_trapsite %>%
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
         "agricultureal open_land" = "village",
         "forested fallow_land" = "forest/fallow",
         "forested fallow_land open_land" = "forest/fallow",
         "fallow_land forested" = "forest/fallow",
         "agricultural forested" = "distal_agriculture",
         "open_land agricultural" = "proximal_agriculture",
         "agricultural open_land" = "proximal_agriculture",
         "village_outside" = "village"),
         habitat_group = recode_factor(habitat_group, !!!habitat_names),
         village = str_to_sentence(village)) %>%
  group_by(habitat_group)

location_rodents <- trapped_rodents %>%
  dplyr::select(rodent_id, trap_night, trap_uid, initial_species_id) %>%
  left_join(., trap_sites, 
            by = c("rodent_id")) %>%
  mutate(trap_night = ifelse(trap_night.x == trap_night.y, trap_night.x, "WARNING")) %>%
  dplyr::select(rodent_id, trap_night, initial_species_id, village, habitat_group)

trapped_rodents %<>%
  mutate(initial_species_id = as_factor(initial_species_id))

species_id <- c("l_sikapusi" = "lophuromys_spp", 
                "crocidura_spp" = "crocidura_spp",
                "l_striatus" = "lemniscomys_spp",
                "m_minutoides" = "mus_spp",
                "malacomys_edwardsi" = "malacomys_spp",
                "praomys_spp" = "praomys_spp",
                "gerbillinae_spp" = "gerbillinae_spp",
                "mastomys_spp" = "mastomys_spp",
                "hylomyscus_spp" = "hylomyscus_spp",
                "hybomys_spp" = "hybomys_spp",
                "r_rattus" = "rattus_spp",
                "rattus_spp" = "rattus_spp",
                "mus_minutoides" = "mus_spp",
                "rattus_sp" = "rattus_spp",
                "lophuromys_spp" = "lophuromys_spp",
                "lemniscomys_spp" = "lemniscomys_spp",
                "lophuromys_sikapusi" = "lophuromys_spp",
                "mastomys_natalensis" = "mastomys_spp",
                "proamys_spp" = "praomys_spp",
                "rattus_rattus" = "rattus_spp",
                "shrew_spp" = "crocidura_spp")
genus_allocation <- c("l_sikapusi" = "lophuromys", 
                      "crocidura_spp" = "crocidura",
                      "l_striatus" = "lemniscomys",
                      "mus_spp" = "mus",
                      "m_minutoides" = "mus",
                      "malacomys_edwardsi" = "malacomys",
                      "praomys_spp" = "praomys",
                      "gerbillinae_spp" = "gerbillinae",
                      "mastomys_spp" = "mastomys",
                      "malacomys_spp" = "malacomys",
                      "hylomyscus_spp" = "hylomyscus",
                      "hybomys_spp" = "hybomys",
                      "r_rattus" = "rattus",
                      "rattus_spp" = "rattus",
                      "mus_minutoides" = "mus",
                      "rattus_sp" = "rattus",
                      "lophuromys_spp" = "lophuromys",
                      "lemniscomys_spp" = "lemniscomys",
                      "dasymys_spp" = "dasymys")
family_allocation <- c("lophuromys" = "muridae",
                       "crocidura" = "soricidae",
                       "lemniscomys" = "muridae",
                       "mus" = "muridae",
                       "malacomys" = "muridae",
                       "praomys" = "muridae",
                       "gerbillinae" = "muridae",
                       "mastomys" = "muridae",
                       "hylomyscus" = "muridae",
                       "hybomys" = "muridae",
                       "rattus" = "muridae",
                       "dasymys" = "muridae")

trapped_rodents <- trapped_rodents %>%
  mutate(initial_genus_id = recode(initial_species_id, !!!species_id),
         genus = recode(initial_genus_id, !!!genus_allocation),
         family = recode(genus, !!!family_allocation)) %>%
  rename("initial_id" = initial_species_id) %>%
  left_join(., trap_sites %>%
              dplyr::select(any_of(c("trap_uid", "rodent_id", "habitat_group", "trap_land_type", "weather", "lon", "lat", "geometry"))),
            by = c("rodent_id", "trap_uid"))

save_data(trapped_rodents, "rodents")
