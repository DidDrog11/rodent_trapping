final_cleaning_rodents <- function(rodent_data){
  
  final_rodent <- rodent_data %>%
    dplyr::select(-genus) %>%
    left_join(., species_dictionary,
              by = c("initial_species_id" = "current_names")) %>%
    mutate(research_visit = case_when(village %in% c("lalehun", "seilama") & visit == "1" ~ 0,
                                      village %in% c("lalehun", "seilama") & visit == "2" ~ 1,
                                      village %in% c("lalehun", "seilama") & visit == "3" ~ 2,
                                      village %in% c("lalehun", "seilama") & visit == "4" ~ 3,
                                      village %in% c("lalehun", "seilama") & visit == "5" ~ 4,
                                      village %in% c("lalehun", "seilama") & visit == "6" ~ 5,
                                      village %in% c("lalehun", "seilama") & visit == "7" ~ 6,
                                      village %in% c("lalehun", "seilama") & visit == "8" ~ 7,
                                      village %in% c("bambawo", "baiama", "lambayama") & visit == "1" ~ 2,
                                      village %in% c("bambawo", "baiama", "lambayama") & visit == "2" ~ 3,
                                      village %in% c("bambawo", "baiama", "lambayama") & visit == "3" ~ 4,
                                      village %in% c("bambawo", "baiama", "lambayama") & visit == "4" ~ 5,
                                      village %in% c("bambawo", "baiama", "lambayama") & visit == "7" ~ 6,
                                      village %in% c("bambawo", "baiama", "lambayama") & visit == "8" ~ 7,
                                      TRUE ~ as.numeric(visit)),
           research_visit = factor(research_visit, levels = c(0, 1, 2, 3, 4, 5, 6, 7), labels = c("Pilot", "1", "2", "3", "4", "5", "6", "7"))) %>%
    dplyr::select(rodent_uid, trap_uid, research_visit, clean_names, genus, family, sub_family, sex, age_group, weight, head_body, tail, cut_tail, hind_foot, ear, length_skull,
                  testes, seminal_vesicles, vagina_perforate, teats_visible, pairs_teats, number_embryos, notes, all_samples, dorsal_image_id,
                  ventral_image_id, key, source_data)
  
  return(final_rodent)
}