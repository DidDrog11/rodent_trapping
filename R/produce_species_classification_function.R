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
                select(rodent_uid, "subfamily" = sub_family, sex, age_group),
              by = c("rodent_id" = "rodent_uid"))
  
  # Granjon has a more helpful data structure but is not for all species of interest. Where this is available we use this source
  
  if(!file.exists(here("data", "speciation", "multinomial_classification.rds"))) {
    
    granjon <- data %>%
      filter(source == "granjon") %>%
      mutate(genus = str_split(name, "_", simplify = TRUE)[, 1])
    
    # We use a truncated normal distribution to sample 10,000 values from these distributions. The min and max are set to 75% and 125% of the min and max respectively
    simulated_morphology_granjon <- list()
    
    for(i in 1:nrow(granjon)) {
      
      species = rep(granjon$name[i], each = 1000)
      subfamily = rep(granjon$sub_family[i], each = 1000)
      sex = rep(granjon$sex[i], each = 1000)
      weight = rtruncnorm(n = 1000, mean = granjon$weight_mean[i], sd = granjon$weight_sd[i], a = granjon$weight_min[i]*0.75, b = granjon$weight_max[i]*1.25)
      head_body = rtruncnorm(n = 1000, mean = granjon$head_body_mean[i], sd = granjon$head_body_sd[i], a = granjon$head_body_min[i]*0.75, b = granjon$head_body_max[i]*1.25)
      tail = rtruncnorm(n = 1000, mean = granjon$tail_mean[i], sd = granjon$tail_sd[i], a = granjon$tail_min[i]*0.75, b = granjon$tail_max[i]*1.25)
      hind_foot = rtruncnorm(n = 1000, mean = granjon$hind_foot_mean[i], sd = granjon$hind_foot_sd[i], a = granjon$hind_foot_min[i]*0.75, b = granjon$hind_foot_max[i]*1.25)
      ear = rtruncnorm(n = 1000, mean = granjon$ear_mean[i], sd = granjon$ear_sd[i], a = granjon$ear_min[i]*0.75, b = granjon$ear_max[i]*1.25)
      hb_tail_ratio = tail/head_body
      
      simulated_morphology_granjon[[i]] <- tibble(species, subfamily, sex, weight, head_body, tail, hb_tail_ratio, hind_foot, ear)
      
    }
    
    simulated_morphology_granjon <- bind_rows(simulated_morphology_granjon)
    
    others <- data %>%
      filter(source != "granjon") %>%
      filter(!str_detect(group, "squirrel")) %>%
      filter(!name %in% granjon$name) %>%
      filter(!str_detect(name, "obscurior|denti")) %>% # missing data on these two shrew species
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
      subfamily = rep(mean_sd$sub_family[i], each = 2000)
      weight = rtruncnorm(n = 2000, mean = mean_sd$weight_mean[i], sd = mean_sd$weight_sd[i], a = mean_sd$weight_min[i]*0.75, b = mean_sd$weight_max[i]*1.25)
      head_body = rtruncnorm(n = 2000, mean = mean_sd$head_body_mean[i], sd = mean_sd$head_body_sd[i], a = mean_sd$head_body_min[i]*0.75, b = mean_sd$head_body_max[i]*1.25)
      tail = rtruncnorm(n = 2000, mean = mean_sd$tail_mean[i], sd = mean_sd$tail_sd[i], a = mean_sd$tail_min[i]*0.75, b = mean_sd$tail_max[i]*1.25)
      hind_foot = rtruncnorm(n = 2000, mean = mean_sd$hind_foot_mean[i], sd = mean_sd$hind_foot_sd[i], a = mean_sd$hind_foot_min[i]*0.75, b = mean_sd$hind_foot_max[i]*1.25)
      ear = rtruncnorm(n = 2000, mean = mean_sd$ear_mean[i], sd = mean_sd$ear_sd[i], a = mean_sd$ear_min[i]*0.75, b = mean_sd$ear_max[i]*1.25)
      hb_tail_ratio = tail/head_body
      
      simulated_morphology_others[[i]] <- tibble(species, subfamily, weight, head_body, tail, hb_tail_ratio, hind_foot, ear)
    }
    
    simulated_morphology_others <- bind_rows(simulated_morphology_others)
    
    simulated_morphology <- bind_rows(simulated_morphology_granjon, simulated_morphology_others)
    
    ## Lophuromys are the only rodents in this region with russett coats and light orange abdomens
    ## Lemniscomys species are identifiable based on the structure of their stripes
    ## Gerbilliscus species are identifiable based on their body shapes
    ## Arvicanthis are identifiable but so far have not been trapped in our study
    ## Cricetomys are identifiable but so far have not been trapped in our study
    
    out_of_area <- c("arvicanthis|cricetomys")
    identified_otherwise <- c("tatera|gerbilliscus|lophuromys|lemniscomys")
    
    ## Multinomial model for murinae
    
    murinae_multinom_train <- simulated_morphology %>%
      mutate(species = factor(species)) %>%
      filter(!str_detect(species, paste0(out_of_area, "|", identified_otherwise))) %>%
      filter(str_detect(subfamily, "murinae")) %>%
      select(-sex)
    
    murinae_multinom_train$species <- droplevels(relevel(murinae_multinom_train$species, ref = "mus_minutoides"))
    
    split_data <- initial_split(murinae_multinom_train, prop = 0.7, strata = "species")
    train_data <- training(split_data)
    test_data <- testing(split_data)
    
    murinae_multinom_model_m1 <- multinom(species ~ weight + head_body + tail + hb_tail_ratio + hind_foot + ear, data = train_data,
                                  maxit = 1000)
    
    train_data$species_predicted <- predict(murinae_multinom_model_m1, newdata = train_data, response = "class")
    murine_train <- train_data
    tab <- table(murine_train$species, murine_train$species_predicted)
    # Accuracy
    murinae_accuracy <- round((sum(diag(tab))/sum(tab)) * 100, 2)
    
    # Variable importance
    varImp(murinae_multinom_model_m1)
    
    test_data$species_predicted <-  predict(murinae_multinom_model_m1, newdata = test_data, response = "class")
    murine_test <- test_data
    
    murinae_confusion <- bind_rows(murine_train %>%
                                     group_by(species, species_predicted) %>%
                                     summarise(n = n()) %>%
                                     mutate(prop = n/sum(n)) %>%
                                     mutate(data = "train"),
                                   murine_test %>%
                                     group_by(species, species_predicted) %>%
                                     summarise(n = n()) %>%
                                     mutate(prop = n/sum(n)) %>%
                                     mutate(data = "test"))
    
    levels(murinae_confusion$species) <- sort(as.character(levels(murinae_confusion$species)))
    levels(murinae_confusion$species_predicted) <- sort(as.character(levels(murinae_confusion$species_predicted)))
    
    murinae_confusion_matrix <- ggplot(murinae_confusion) +
      geom_tile(aes(x = species, y = species_predicted, fill = prop)) +
      facet_grid(~ data) +
      scale_fill_viridis_c() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      labs(title = paste("Murinae, accuracy =", murinae_accuracy))
    
    test_data$species_predicted <- predict(murinae_multinom_model_m1, newdata = test_data, response = "class")
    tab <- table(test_data$species, test_data$species_predicted)
    chisq.test(test_data$species, test_data$species_predicted)
    
    write_rds(murinae_multinom_model_m1, here("data", "tmp_data", "murinae_speciation_model.rds"))
    
    # Apply to observed data
    murinae_observed <- identified_genus %>%
      mutate(hb_tail_ratio = tail/head_body) %>%
      select(rodent_id, initial_species_id, photo_identification, morphological_genus_id, subfamily, age_group, weight, head_body, tail, hb_tail_ratio, hind_foot,
             ear, length_skull, pelage, abdominal_colouring, coat_feature, ear_size) %>%
      filter(!str_detect(morphological_genus_id, paste0(out_of_area, "|", identified_otherwise))) %>%
      filter(str_detect(subfamily, "murinae"))
    
    murinae_observed$class_allocation <- predict(murinae_multinom_model_m1, newdata = murinae_observed, type = "class")
    probabilities <- data.frame(round(predict(murinae_multinom_model_m1, newdata = murinae_observed, type = "probs"), 3))
    
    murinae_observed <- bind_cols(murinae_observed, probabilities)
    
    ## Multinomial model for Crocidurinae
    crocidurae_multinom_train <- simulated_morphology %>%
      mutate(species = factor(species)) %>%
      filter(!str_detect(species, paste0(out_of_area, "|", identified_otherwise))) %>%
      filter(str_detect(subfamily, "crocidurinae")) %>%
      select(-sex)
    
    crocidurae_multinom_train$species <- droplevels(relevel(crocidurae_multinom_train$species, ref = "crocidura_olivieri"))
    
    split_data <- initial_split(crocidurae_multinom_train, prop = 0.7, strata = "species")
    train_data <- training(split_data)
    test_data <- testing(split_data)
    
    crocidurae_multinom_model_m1 <- multinom(species ~ weight + head_body + tail + hb_tail_ratio + hind_foot + ear, data = train_data,
                                          maxit = 200)
    
    train_data$species_predicted <- predict(crocidurae_multinom_model_m1, newdata = train_data, response = "class")
    crocidura_train <- train_data
    tab <- table(crocidura_train$species, crocidura_train$species_predicted)
    # Accuracy
    crocidura_accuracy <- round((sum(diag(tab))/sum(tab)) * 100, 2)
    
    test_data$species_predicted <-  predict(crocidurae_multinom_model_m1, newdata = test_data, response = "class")
    crocidura_test <- test_data
    
    crocidura_confusion <- bind_rows(crocidura_train %>%
      group_by(species, species_predicted) %>%
      summarise(n = n()) %>%
      mutate(prop = n/sum(n)) %>%
      mutate(data = "train"),
      crocidura_test %>%
        group_by(species, species_predicted) %>%
        summarise(n = n()) %>%
        mutate(prop = n/sum(n)) %>%
        mutate(data = "test"))
    
    levels(crocidura_confusion$species) <- sort(as.character(levels(crocidura_confusion$species)))
    levels(crocidura_confusion$species_predicted) <- sort(as.character(levels(crocidura_confusion$species_predicted)))
    
    crocidura_confusion_matrix <- ggplot(crocidura_confusion) +
      geom_tile(aes(x = species, y = species_predicted, fill = prop)) +
      facet_grid(~ data) +
      scale_fill_viridis_c() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      labs(title = paste("Crocidura, accuracy =", crocidura_accuracy))
    
    write_rds(crocidurae_multinom_model_m1, here("data", "tmp_data", "crocidura_speciation_model.rds"))
    
    # Apply to observed data
    crocidura_observed <- identified_genus %>%
      mutate(hb_tail_ratio = tail/head_body) %>%
      select(rodent_id, initial_species_id, photo_identification, morphological_genus_id, subfamily, age_group, weight, head_body, tail, hb_tail_ratio, hind_foot,
             ear, length_skull, pelage, abdominal_colouring, coat_feature, ear_size) %>%
      filter(!str_detect(morphological_genus_id, paste0(out_of_area, "|", identified_otherwise))) %>%
      filter(str_detect(subfamily, "crocidurinae"))
    
    crocidura_observed$class_allocation <- predict(crocidurae_multinom_model_m1, newdata = crocidura_observed, type = "class")
    probabilities <- data.frame(round(predict(crocidurae_multinom_model_m1, newdata = crocidura_observed, type = "probs"), 3))
    
    crocidura_observed <- bind_cols(crocidura_observed, probabilities)
    
    ## Combine Murinae and Crocidura predictions
    observed_predicted <- bind_rows(murinae_observed, crocidura_observed)
    
    ## Add in those we are identifying based on phenotypic characteristics
    observed_non_predicted <- identified_genus %>%
      filter(!rodent_id %in% observed_predicted$rodent_id) %>%
      mutate(hb_tail_ratio = tail/head_body) %>%
      select(rodent_id, initial_species_id, photo_identification, morphological_genus_id, subfamily, age_group,
             weight, head_body, tail, hb_tail_ratio, hind_foot, ear, length_skull, pelage, abdominal_colouring,
             coat_feature, ear_size)
    
    combined_data <- bind_rows(observed_predicted %>%
                                 mutate(predicted = TRUE),
                               observed_non_predicted %>%
                                 mutate(predicted = FALSE)) %>%
      arrange(rodent_id)
    
    write_rds(combined_data, here("data", "speciation", "multinomial_classification.rds"))
    
  } else {
    
    combined_data <- read_rds(here("data", "speciation", "multinomial_classification.rds"))
    
  }
  
  ## The following plots visualise the classifications and probabilities produced by the model
  ## For ease of interpretation these have been split into the morphological identified genera
  plot_murinae <- combined_data %>%
    filter(predicted == TRUE) %>%
    filter(subfamily == "murinae") %>%
    select(rodent_id, morphological_genus_id, age_group, class_allocation, dasymys_rufulus, dephomys_defua, hybomys_planifrons, hybomys_trivirgatus,
           hylomyscus_baeri, hylomyscus_simus, malacomys_edwardsi, mastomys_erythroleucus, mastomys_natalensis, mus_baoulei, mus_mattheyi, mus_minutoides,
           mus_musculoides, mus_musculus, mus_setulosus, oenomys_ornatus, praomys_daltoni, praomys_rostratus, praomys_tullbergi, rattus_norvegicus,
           rattus_rattus) %>%
    pivot_longer(cols = 5:25, names_to = "species_name", values_to = "species_probability") %>%
    group_by(morphological_genus_id) %>%
    group_split()
  
  plot_murinae_classification <- lapply(plot_murinae, function(x) { 
    
    genus <- unique(x$morphological_genus_id)
    
    plot <- x %>%
      filter(str_detect(morphological_genus_id, genus)) %>%
      filter(!species_probability == 0) %>%
      ggplot() +
      geom_tile(aes(x = species_name, y = rodent_id, fill = species_probability), width = 0.95, height = 0.95) +
      facet_grid(age_group ~ ., scales = "free", space = "free") +
      theme_bw() +
      labs(x = element_blank(),
           y = "Rodent ID",
           fill = "Probability",
           title = str_to_sentence(genus)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            strip.text.y = element_text(angle = 270))
    
    return(plot)
    
    })
  
  names(plot_murinae_classification) <- bind_rows(plot_murinae) %>%
    distinct(morphological_genus_id) %>%
    pull()
  
  plot_crocidura <- combined_data %>%
    filter(predicted == TRUE) %>%
    filter(subfamily == "crocidurinae") %>%
    select(rodent_id, morphological_genus_id, age_group, class_allocation, crocidura_olivieri, crocidura_crossei,
           crocidura_fuscomurina, crocidura_lamottei, crocidura_muricauda, crocidura_poensis, suncus_megalura) %>%
    pivot_longer(cols = 5:11, names_to = "species_name", values_to = "species_probability") %>%
    filter(!species_probability == 0) %>%
    ggplot() +
    geom_tile(aes(x = species_name, y = rodent_id, fill = species_probability), width = 0.95, height = 0.95) +
    facet_grid(age_group ~ ., scales = "free", space = "free") +
    theme_bw() +
    labs(x = element_blank(),
         y = "Rodent ID",
         fill = "Probability",
         title = "Crocidura") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          strip.text.y = element_text(angle = 270))
  
  return(list(rodent_classification = combined_data,
              murinae_confusion = murinae_confusion_matrix,
              murinae_plots = plot_murinae_classification,
              crocidurinae_confusion = crocidura_confusion_matrix,
              crocidurinae_plot = plot_crocidura))
  
}
