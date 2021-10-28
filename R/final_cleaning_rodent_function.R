final_cleaning_rodents <- function(rodent_data){
  
  final_rodent <- rodent_data %>%
    dplyr::select(-genus) %>%
    left_join(., species_dictionary,
              by = c("initial_species_id" = "current_names")) %>%
    dplyr::select(rodent_uid, trap_uid, clean_names, genus, family, sex, weight, head_body, tail, cut_tail, hind_foot, ear, length_skull,
                  testes, seminal_vesicles, vagina_perforate, teats_visible, pairs_teats, number_embryos, notes, all_samples, dorsal_image_id,
                  ventral_image_id, key)
  
  return(final_rodent)
}