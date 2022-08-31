final_cleaning_rodents <- function(rodent_data){
  
  final_rodent <- rodent_data %>%
    dplyr::select(-genus) %>%
    left_join(., species_dictionary,
              by = c("initial_species_id" = "current_names")) %>%
    mutate(research_visit = case_when(village %in% c("bambawo", "lambayama", "baiama") ~ as.numeric(visit) + 1,
                                      village %in% c("lalehun", "seilama") ~ as.numeric(visit) - 1),
           research_visit = factor(research_visit, labels = c("Pilot", "1", "2", "3", "4", "5", "6", "7"))) %>%
    dplyr::select(rodent_uid, trap_uid, research_visit, clean_names, genus, family, sub_family, sex, age_group, weight, head_body, tail, cut_tail, hind_foot, ear, length_skull,
                  testes, seminal_vesicles, vagina_perforate, teats_visible, pairs_teats, number_embryos, notes, all_samples, dorsal_image_id,
                  ventral_image_id, key, source_data)
  
  return(final_rodent)
}