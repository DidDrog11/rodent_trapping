run_classifier <- function(data = final_cleaned_rodent_data) {
  
  bayes_classifier <- read_rds(here("data", "speciation", "bayes_classifier.rds"))
  
  data_to_predict <- data %>%
    select(clean_names, sex, weight, hb_length = head_body, tail, hind_foot, ear, skull = length_skull)
  
  data_to_predict$predicted_species <- predict(nb.m1, newdata = data_to_predict)
  
}