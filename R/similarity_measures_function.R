compare_to_reference_species <- function(reference_data = rodent_speciation, individual_data = final_cleaned_rodent_data){
  
  reference <- reference_data %>%
    select(name, weight_mean, head_body_mean, tail_mean, hind_foot_mean, ear_mean, length_skull_mean)
  
  test <- matrix(data = c(test_1, test_2), nrow = 2, ncol = 6)
  
  field_genus_id <- individual_data %>%
    mutate(genus  = case_when(is.na(genus) ~ "not_stated",
                              TRUE ~ genus)) %>%
    pull(genus)
  
  cosine(vector_of_reference, vector_of_test)
}