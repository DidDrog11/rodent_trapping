source(here::here("scripts", "project_library.R"))

trapped_rodents <- read_csv(here("data", "rodents_trapped.csv")) %>%
  filter(group == "rodent")
rodent_ids <- read_csv(here("data", "rodent_ids.csv")) %>%
  filter(group == "rodent") %>%
  separate(col = name, into = c("genus", "species"), sep = "_", remove = F) %>%
  mutate_all(~replace(., is.na(.), 0))

a <- NULL
for(n in 1:length(trapped_rodents$rodent_id)) {
  for(i in 1:length(rodent_ids$name)) {
  tmp <- trapped_rodents[n,] %>%
    mutate(tested_against = rodent_ids[i,]$name,
           weight_mean_match = ifelse(abs(weight - rodent_ids[i,]$weight_mean) <= rodent_ids[i,]$weight_mean/10, T, F), #This allows the mean to match +/- 10%
           weight_range_match = ifelse(weight >= rodent_ids[i,]$weight_min,
                                        ifelse(weight <= rodent_ids[i,]$weight_max, T, F),
                                        F),
           head_body_mean_match = ifelse(abs(head_body - rodent_ids[i,]$head_body_mean) <= rodent_ids[i,]$head_body_mean/10, T, F),
           head_body_range_match = ifelse(head_body >= rodent_ids[i,]$head_body_min,
                                          ifelse(head_body <= rodent_ids[i,]$head_body_max, T, F),
                                          F),
           tail_mean_match = ifelse(abs(tail - rodent_ids[i,]$tail_mean) <= rodent_ids[i,]$tail_mean/10, T, F),
           tail_range_match = ifelse(tail >= rodent_ids[i,]$tail_min,
                                     ifelse(tail <= rodent_ids[i,]$tail_max, T, F),
                                     F),
           hind_foot_mean_match = ifelse(abs(hind_foot - rodent_ids[i,]$hind_foot_mean) <= rodent_ids[i,]$hind_foot_mean/10, T, F),
           hind_foot_range_match = ifelse(hind_foot >= rodent_ids[i,]$hind_foot_min,
                                          ifelse(hind_foot <= rodent_ids[i,]$hind_foot_max, T, F),
                                          F),
           ear_mean_match = ifelse(abs(ear - rodent_ids[i,]$ear_mean) <= rodent_ids[i,]$ear_mean/10, T, F),
           ear_range_match = ifelse(ear >= rodent_ids[i,]$ear_min,
                                    ifelse(ear <= rodent_ids[i,]$ear_max, T, F),
                                    F),
           skull_mean_match = ifelse(length_skull == rodent_ids[i,]$length_skull_mean, T, F),
           skull_range_match = ifelse(length_skull >= rodent_ids[i,]$length_skull_min,
                                      ifelse(length_skull <= rodent_ids[i,]$length_skull_max, T, F),
                                      F),
           mean_match = sum(weight_mean_match, head_body_mean_match, tail_mean_match, hind_foot_mean_match, ear_mean_match, skull_mean_match)/6,
           range_match = sum(weight_range_match, head_body_range_match, tail_range_match, hind_foot_range_match, ear_range_match, skull_range_match)/6) %>%
    select(rodent_id, initial_species_id, tested_against, mean_match, range_match,
           weight_mean_match, weight_range_match, head_body_mean_match, head_body_range_match,
           tail_mean_match, tail_range_match, hind_foot_mean_match, hind_foot_range_match,
           ear_mean_match, ear_range_match, skull_mean_match, skull_range_match)
  a <- bind_rows(a, tmp) 
  }
}

rodent_matches <- a %>%
  group_by(rodent_id) %>%
  mutate(weighted_match = mean_match + range_match/2) %>% #a match to the mean is weighted twice as highly as a match to the range
  arrange(-weighted_match, .by_group = T) %>%
  rename("match_id" = "tested_against") %>%
  mutate(weighted_match = order(order(weighted_match, decreasing=TRUE))) %>%
  select(rodent_id, initial_species_id, match_id, weighted_match) %>%
  pivot_wider(., names_from = match_id, values_from = weighted_match)

trapped_shrews <- read_csv(here("data", "rodents_trapped.csv")) %>%
  filter(group == "shrew")
shrew_ids <- read_csv(here("data", "rodent_ids.csv")) %>%
  filter(group == "shrew") %>%
  separate(col = name, into = c("genus", "species"), sep = "_", remove = F) %>%
  mutate_all(~replace(., is.na(.), 0))

b <- NULL
for(n in 1:length(trapped_shrews$rodent_id)) {
  for(i in 1:length(shrew_ids$name)) {
    tmp <- trapped_shrews[n,] %>%
      mutate(tested_against = shrew_ids[i,]$name,
             weight_mean_match = ifelse(abs(weight - shrew_ids[i,]$weight_mean) <= shrew_ids[i,]$weight_mean/10, T, F), #This allows the mean to match +/- 10%
             weight_range_match = ifelse(weight >= shrew_ids[i,]$weight_min,
                                         ifelse(weight <= shrew_ids[i,]$weight_max, T, F),
                                         F),
             head_body_mean_match = ifelse(abs(head_body - shrew_ids[i,]$head_body_mean) <= shrew_ids[i,]$head_body_mean/10, T, F),
             head_body_range_match = ifelse(head_body >= shrew_ids[i,]$head_body_min,
                                            ifelse(head_body <= shrew_ids[i,]$head_body_max, T, F),
                                            F),
             tail_mean_match = ifelse(abs(tail - shrew_ids[i,]$tail_mean) <= shrew_ids[i,]$tail_mean/10, T, F),
             tail_range_match = ifelse(tail >= shrew_ids[i,]$tail_min,
                                       ifelse(tail <= shrew_ids[i,]$tail_max, T, F),
                                       F),
             hind_foot_mean_match = ifelse(abs(hind_foot - shrew_ids[i,]$hind_foot_mean) <= shrew_ids[i,]$hind_foot_mean/10, T, F),
             hind_foot_range_match = ifelse(hind_foot >= shrew_ids[i,]$hind_foot_min,
                                            ifelse(hind_foot <= shrew_ids[i,]$hind_foot_max, T, F),
                                            F),
             ear_mean_match = ifelse(abs(ear - shrew_ids[i,]$ear_mean) <= shrew_ids[i,]$ear_mean/10, T, F),
             ear_range_match = ifelse(ear >= shrew_ids[i,]$ear_min,
                                      ifelse(ear <= shrew_ids[i,]$ear_max, T, F),
                                      F),
             skull_mean_match = ifelse(length_skull == shrew_ids[i,]$length_skull_mean, T, F),
             skull_range_match = ifelse(length_skull >= shrew_ids[i,]$length_skull_min,
                                        ifelse(length_skull <= shrew_ids[i,]$length_skull_max, T, F),
                                        F),
             mean_match = sum(weight_mean_match, head_body_mean_match, tail_mean_match, hind_foot_mean_match, ear_mean_match, skull_mean_match)/6,
             range_match = sum(weight_range_match, head_body_range_match, tail_range_match, hind_foot_range_match, ear_range_match, skull_range_match)/6) %>%
      select(rodent_id, initial_species_id, tested_against, mean_match, range_match,
             weight_mean_match, weight_range_match, head_body_mean_match, head_body_range_match,
             tail_mean_match, tail_range_match, hind_foot_mean_match, hind_foot_range_match,
             ear_mean_match, ear_range_match, skull_mean_match, skull_range_match)
    b <- bind_rows(b, tmp) 
  }
}

all_individuals <- bind_rows(a,b)

shrew_matches <- b %>%
  group_by(rodent_id) %>%
  mutate(weighted_match = mean_match + range_match/2) %>% #a match to the mean is weighted twice as highly as a match to the range
  arrange(-weighted_match, .by_group = T) %>%
  rename("match_id" = "tested_against") %>%
  mutate(weighted_match = order(order(weighted_match, decreasing=TRUE))) %>%
  select(rodent_id, initial_species_id, match_id, weighted_match) %>%
  pivot_wider(., names_from = match_id, values_from = weighted_match)

write_rds(rodent_matches, here("data", "speciation", "rodent_matches.rds"))
write_rds(shrew_matches, here("data", "speciation", "shrew_matches.rds"))
