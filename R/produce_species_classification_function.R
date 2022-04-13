produce_species_classification <- function(trapped_rodents = final_cleaned_rodent_data, data = rodent_speciation) {
  
  if(!file.exists(here("data", "speciation", "bayes_classifier.rds"))) {
    
    identified_genus <- 
    
    mean_sd <- data %>%
      mutate(weight_sd = (weight_max - weight_mean)/4,
             head_body_sd = (head_body_max - head_body_min)/4,
             tail_sd = (tail_max -tail_mean)/4,
             hind_foot_sd = (hind_foot_max - hind_foot_mean)/4,
             ear_sd = (ear_max - ear_min)/4,
             length_skull_sd = (length_skull_max - length_skull_mean)/4) %>%
      select(name, group, contains("mean"), contains("sd")) %>%
      drop_na()
    
    set.seed(123)
    
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
    
    squirrels_sim <- simulated_morphology %>%
      filter(species %in% squirrels)
    
    dormice_sim <- simulated_morphology %>%
      filter(species %in% dormice)
    
    rodent_sim <- simulated_morphology %>%
      filter(species %in% rodent)
    
    rodent_split <- initial_split(rodent_sim, prop = .7, strata = "species")
    rodent_train <- training(rodent_split)
    rodent_test  <- testing(rodent_split)
    
    # create response and feature data
    features <- setdiff(names(rodent_train), "species")
    x <- rodent_train[, features]
    y <- rodent_train$species
    
    # set up 10-fold cross validation procedure
    train_control <- trainControl(method = "cv", number = 10)
    
    # train model
    nb.m1 <- train(x = x, y = y, method = "nb", trControl = train_control)
    
    # results
    confusionMatrix(nb.m1)
    sim_pred <- predict(nb.m1, newdata = rodent_test)
    simulated_predictions <- confusionMatrix(sim_pred, factor(rodent_test$species))
    
    shrew_sim <- simulated_morphology %>%
      filter(species %in% shrew)
    
    
    
    
    
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
