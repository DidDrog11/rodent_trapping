habitat_dictionary <- tibble(current_name = c("developing_banana_plantation", "palm_plantation", "cocoa_outskirts",
                                              "rice_field", "banana_plantation", "cocao_coffee_plantation", "dry_rice",
                                              "wet_rice", "cassava_plantation", "cacao_coffee_plantation", "harvested_dry_rice_farm",
                                              "disturbed_forest, fallow_5y", "forest_fallow", "village_periphery", "houses",
                                              "forest", "fallow_land", "forested", "fallow_land open_land", "open_land fallow_land",
                                              "open_land", "village_inside", "open_land village_inside", "agricultureal open_land",
                                              "forested fallow_land", "forested fallow_land open_land", "fallow_land forested",
                                              "agricultural forested", "open_land agricultural", "agricultural open_land", "village_outside",
                                              "forested open_land", "agricultural fallow_land", "village_outside agricultural"),
                             habitat_group = c("proximal_agriculture", "proximal_agriculture", "proximal_agriculture",
                                               "proximal_agriculture", "proximal_agriculture", "proximal_agriculture",
                                               "proximal_agriculture", "distal_agriculture", "distal_agriculture", "distal_agriculture",
                                               "distal_agriculture", "forest/fallow", "forest/fallow", "village", "village", "forest/fallow",
                                               "forest/fallow", "forest/fallow", "forest/fallow", "forest/fallow", "forest/fallow", "village",
                                               "village", "village", "forest/fallow", "forest/fallow", "forest/fallow", "distal_agriculture",
                                               "proximal_agriculture", "proximal_agriculture", "village", "forest/fallow", "proximal_agriculture",
                                               "village"),
                             clean_name = c("banana", "palm", "cacao", "rice", "banana", "cacao", "rice", "rice", "cassava", "cacao", "rice",
                                            "forest_disturbed", "forest_fallow", "village", "housing", "forest", "fallow", "forest", "fallow",
                                            "fallow", "open", "housing", "housing", "open", "forest_fallow", "forest_fallow", "forest_fallow",
                                            "forest", "open", "open", "village", "forest", "fallow", "village")
                             )

clean_habitat_group <- habitat_dictionary$habitat_group
names(clean_habitat_group) <- habitat_dictionary$current_name


clean_habitat <- habitat_dictionary$clean_name
names(clean_habitat) <- habitat_dictionary$current_name

