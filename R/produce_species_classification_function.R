produce_species_classification <- function(trapped_rodents = rodent_image_speciation, data = rodent_speciation, final_cleaned_rodent = final_cleaned_rodent_data) {
  
  identified_genus <- trapped_rodents %>%
    select(rodent_id, weight, head_body, tail, hind_foot, ear, length_skull,
           pelage, abdominal_colouring, coat_feature, ear_size, initial_species_id,
           photo_identification) %>%
    mutate(initial_species_id = case_when(initial_species_id == "proamys_spp" ~ "praomys_spp",
                                          TRUE ~ initial_species_id),
           initial_genus_id = str_split(initial_species_id, "_", simplify = TRUE)[, 1],
           photo_genus_id =  str_split(photo_identification, "_", simplify = TRUE)[, 1],
           initial_photo_match = case_when(initial_genus_id == photo_genus_id ~ TRUE,
                                           TRUE ~ FALSE)) %>%
    mutate(morphological_genus_id = case_when(initial_photo_match == TRUE ~ initial_genus_id,
                                              initial_photo_match == FALSE & is.na(photo_genus_id) ~ initial_genus_id,
                                              initial_photo_match == FALSE & !is.na(photo_genus_id) ~ photo_genus_id,
                                              TRUE ~ "check")) %>%
    left_join(., final_cleaned_rodent %>%
                select(rodent_uid, sex),
              by = c("rodent_id" = "rodent_uid"))
  
  # Granjon has a more helpful data structure but is not for all species of interest. Where this is available we use this source
  
  if(!file.exists(here("data", "speciation", "multinomial_model.rds"))) {
    
    granjon <- data %>%
      filter(source == "granjon") %>%
      filter(!str_detect(name, "arvicanthis")) %>% # we have not identified any arvicanthis
      mutate(genus = str_split(name, "_", simplify = TRUE)[, 1])
    
    # We use a truncated normal distribution to sample 10,000 values from these distributions. The min and max are set to 75% and 125% of the min and max respectively
    simulated_morphology_granjon <- list()
    
    for(i in 1:nrow(granjon)) {
      
      species = rep(granjon$name[i], each = 1000)
      sex = rep(granjon$sex[i], each = 1000)
      weight = rtruncnorm(n = 1000, mean = granjon$weight_mean[i], sd = granjon$weight_sd[i], a = granjon$weight_min[i]*0.75, b = granjon$weight_max[i]*1.25)
      head_body = rtruncnorm(n = 1000, mean = granjon$head_body_mean[i], sd = granjon$head_body_sd[i], a = granjon$head_body_min[i]*0.75, b = granjon$head_body_max[i]*1.25)
      tail = rtruncnorm(n = 1000, mean = granjon$tail_mean[i], sd = granjon$tail_sd[i], a = granjon$tail_min[i]*0.75, b = granjon$tail_max[i]*1.25)
      hind_foot = rtruncnorm(n = 1000, mean = granjon$hind_foot_mean[i], sd = granjon$hind_foot_sd[i], a = granjon$hind_foot_min[i]*0.75, b = granjon$hind_foot_max[i]*1.25)
      ear = rtruncnorm(n = 1000, mean = granjon$ear_mean[i], sd = granjon$ear_sd[i], a = granjon$ear_min[i]*0.75, b = granjon$ear_max[i]*1.25)
      hb_tail_ratio = tail/head_body
      
      simulated_morphology_granjon[[i]] <- tibble(species, sex, weight, head_body, tail, hb_tail_ratio, hind_foot, ear)
      
    }
    
    simulated_morphology_granjon <- bind_rows(simulated_morphology_granjon)
    
    others <- data %>%
      filter(source != "granjon") %>%
      filter(!str_detect(group, "squirrel")) %>%
      filter(!name %in% granjon$name) %>%
      filter(!str_detect(name, "arvicanthis|obscurior|denti")) %>%
      mutate(genus = str_split(name, "_", simplify = TRUE)[, 1])
    
    mean_sd <- others %>%
      mutate(weight_sd = (weight_max - weight_mean)/4,
             head_body_sd = (head_body_max - head_body_min)/4,
             tail_sd = (tail_max -tail_mean)/4,
             hind_foot_sd = (hind_foot_max - hind_foot_mean)/4,
             ear_sd = (ear_max - ear_min)/4,
             length_skull_sd = (length_skull_max - length_skull_mean)/4) %>%
      drop_na(contains("_mean"))
    
    simulated_morphology_others <- list()
    
    for(i in 1:length(unique(mean_sd$name))) {
      
      species = rep(mean_sd$name[i], each = 2000)
      weight = rtruncnorm(n = 2000, mean = mean_sd$weight_mean[i], sd = mean_sd$weight_sd[i], a = mean_sd$weight_min*0.75, b = mean_sd$weight_max*1.25)
      head_body = rtruncnorm(n = 2000, mean = mean_sd$head_body_mean[i], sd = mean_sd$head_body_sd[i], a = mean_sd$head_body_min*0.75, b = mean_sd$head_body_max*1.25)
      tail = rtruncnorm(n = 2000, mean = mean_sd$tail_mean[i], sd = mean_sd$tail_sd[i], a = mean_sd$tail_min*0.75, b = mean_sd$tail_max*1.25)
      hind_foot = rtruncnorm(n = 2000, mean = mean_sd$hind_foot_mean[i], sd = mean_sd$hind_foot_sd[i], a = mean_sd$hind_foot_min*0.75, b = mean_sd$hind_foot_max*1.25)
      ear = rtruncnorm(n = 2000, mean = mean_sd$ear_mean[i], sd = mean_sd$ear_sd[i], a = mean_sd$ear_min*0.75, b = mean_sd$ear_max*1.25)
      hb_tail_ratio = tail/head_body
      
      simulated_morphology_others[[i]] <- tibble(species, weight, head_body, tail, hb_tail_ratio, hind_foot, ear)
    }
    
    simulated_morphology_others <- bind_rows(simulated_morphology_others) %>%
      drop_na() %>%
      group_by(species) %>%
      slice_sample(n = 1000, replace = FALSE)
    
    simulated_morphology <- bind_rows(simulated_morphology_granjon, simulated_morphology_others)
    
    multinom_train <- simulated_morphology %>%
      mutate(species = factor(species))
    
    all_species_m1 <- vglm(species ~ weight + head_body + tail + hb_tail_ratio + hind_foot + ear,
                           data = multinom_train,
                           multinomial())
    
    write_rds(all_species_m1, here("data", "tmp_data", "speciation_model.rds"))
    
    multinom_test <- identified_genus %>%
      select(morphological_genus_id, photo_identification, sex, weight, head_body, tail, hind_foot, ear) %>%
      mutate(hb_tail_ratio = tail/head_body) %>%
      drop_na(weight, head_body, tail, hb_tail_ratio, hind_foot, ear)
    
    all <- data.frame(round(predict(all_species_m1, type = "response", multinom_test), 3))
    
    combine_multi <- bind_cols(multinom_test, all)
    
    
    
    
    
    
    simulated_morphology <- list()
    
    for(i in 1:length(unique(mean_sd$name))) {
      
      species = rep(mean_sd$name[i], each = 1000)
      weight = rnorm(n = 1000, mean = mean_sd$weight_mean[i], sd = mean_sd$weight_sd[i])
      hb_length = rnorm(n = 1000, mean = mean_sd$head_body_mean[i], sd = mean_sd$head_body_sd[i])
      tail = rnorm(n = 1000, mean = mean_sd$tail_mean[i], sd = mean_sd$tail_sd[i])
      hind_foot = rnorm(n = 1000, mean = mean_sd$tail_mean[i], sd = mean_sd$tail_sd[i])
      ear = rnorm(n = 1000, mean = mean_sd$ear_mean[i], sd = mean_sd$ear_sd[i])
      skull = rnorm(n = 1000, mean = mean_sd$length_skull_mean[i], sd = mean_sd$length_skull_sd[i])
      
      simulated_morphology[[i]] <- tibble(species, weight, hb_length, tail, hind_foot, ear, skull)
      
    }
    
    simulated_morphology <- bind_rows(simulated_morphology)
    
    
    rodent_speciation_literature <- list(naive_bayes_m1 = nb.m1,
                                         simulated_data = simulated_morphology,
                                         training_set = train,
                                         testing_set = test)
    
    write_rds(rodent_speciation_literature, here("data", "speciation", "bayes_classifier.rds"))
    
  } else {
    
    rodent_speciation_literature <- read_rds(here("data", "speciation", "bayes_classifier.rds"))
    
  }
  
  return(rodent_speciation_literature)
  
}
