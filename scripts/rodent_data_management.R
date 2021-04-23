source(here::here("scripts", "project_library.R"))

## For pilot study only

trapped_rodents <- readxl::read_xlsx(path = here("data", "trap_sites_all.xlsx"), sheet = 3) %>%
  mutate(date = as.Date(date, format = "%Y- %m-%d")) %>%
  filter(date <=	"2020-12-09")
habitat_names <- c("Forest/fallow land", "Distal agriculture", "Proximal agriculture", "Village")
names(habitat_names) = c("forest/fallow", "distal_agriculture", "proximal_agriculture", "village")

trap_sites <- read_csv(here("data", "trap_sites.csv")) %>%
  mutate(habitat = recode(habitat,
         "developing_banana_plantation" = "proximal_agriculture",
         "palm_plantation" = "proximal_agriculture",
         "cassava_plantation" = "distal_agriculture",
         "cacao_coffee_plantation" = "distal_agriculture",
         "harvested_dry_rice_farm" = "distal_agriculture",
         "disturbed_forest, fallow_5y" = "forest/fallow",
         "forest_fallow" = "forest/fallow",
         "village_periphery" = "village"),
         habitat = recode_factor(habitat, !!!habitat_names),
         village = case_when(village == "lalehun" ~ "Lalehun",
                             village == "seilama" ~ "Seilama")) %>%
  group_by(habitat) %>%
  filter(visit == 1)

location_rodents <- trapped_rodents %>%
  dplyr::select(rodent_id, trap_night, trap_id, initial_species_id) %>%
  left_join(., trap_sites, 
            by = c("rodent_id")) %>%
  mutate(trap_night = ifelse(trap_night.x == trap_night.y, trap_night.x, "WARNING")) %>%
  dplyr::select(rodent_id, trap_night, initial_species_id, village, habitat)

trapped_rodents %<>%
  mutate(initial_species_id = as_factor(initial_species_id))

genus_allocation <- c("l_sikapusi" = "lophuromys", 
                      "crocidura_spp" = "crocidura",
                      "l_striatus" = "lemniscomys",
                      "m_minutoides" = "mus",
                      "malacomys_edwardsi" = "malacomys",
                      "praomys_spp" = "praomys",
                      "gerbillinae_spp" = "gerbillinae",
                      "mastomys_spp" = "mastomys",
                      "hylomyscus_spp" = "hylomyscus",
                      "hybomys_spp" = "hybomys",
                      "r_rattus" = "rattus")
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
                       "rattus" = "muridae")
trapped_rodents %<>%
  mutate(genus = recode(initial_species_id, !!!genus_allocation),
         family = recode(genus, !!!family_allocation))