testing_priority <- function(data = final_cleaned_rodent_data, filter_id = all_rodents) {
  
  if(!file.exists(here("data", "clean_data", "lab", "allocated_priority.rds"))) {
    
    dir.create(here("data", "clean_data", "lab"))
    
    process <- read_xlsx(here("data", "clean_data", "lab", "processed_lab.xlsx")) %>%
      mutate(rodent_uid = as.character(rodent_uid))
    
    filter_id <- filter_id %>%
      select(rodent_uid, filter_label)
    
    data_sub <- data %>%
      select(rodent_uid, trap_uid, research_visit, genus)
    
    combined <- left_join(data_sub, filter_id) %>%
      distinct() %>%
      mutate(genus = factor(genus, levels = c("mastomys", "mus", "praomys", "rattus", "hylomyscus", "hybomys",
                                              "dasymys", "lemniscomys", "malacomys", "gerbillinae", "crocidura",
                                              "lophuromys")),
             species_priority = case_when(as.numeric(genus) <= 4 ~ 1,
                                          as.numeric(genus) <= 9 ~ 2,
                                          TRUE ~ 3)) %>%
      arrange(species_priority, research_visit) %>%
      mutate(sample_priority = row_number()) %>%
      left_join(process) %>%
      mutate(elisa = case_when(!is.na(elisa) ~ "ELISA completed",
                               TRUE ~ "ELISA awaited"),
             pcr = case_when(!is.na(pcr) ~ "PCR completed",
                             TRUE ~ "PCR awaited"))
    
    write_rds(combined, here("data", "clean_data", "lab", "allocated_priority.rds")) } else {
      
      allocated_priority <- read_rds(here("data", "clean_data", "lab", "allocated_priority.rds"))
      
      process <- read_xlsx(here("data", "clean_data", "lab", "processed_lab.xlsx")) %>%
        mutate(rodent_uid = as.character(rodent_uid),
               elisa = factor(case_when(!is.na(elisa) ~ "ELISA completed",
                                 TRUE ~ "ELISA awaited")),
               pcr = factor(case_when(!is.na(pcr) ~ "PCR completed",
                               TRUE ~ "PCR awaited")))
      
      allocated_priority_progress <- allocated_priority %>%
        select(-elisa, -pcr) %>%
        left_join(process) %>%
        mutate(elisa = factor(case_when(!is.na(elisa) ~ "ELISA completed",
                                        TRUE ~ "ELISA awaited")),
               pcr = factor(case_when(!is.na(pcr) ~ "PCR completed",
                                      TRUE ~ "PCR awaited")))
      
      filter_id <- filter_id %>%
        select(rodent_uid, filter_label)
      
      data_sub <- data %>%
        select(rodent_uid, trap_uid, research_visit, genus)
      
      combined <- left_join(data_sub, filter_id) %>%
        distinct() %>%
        filter(!rodent_uid %in% allocated_priority_progress$rodent_uid) %>%
        mutate(genus = factor(genus, levels = c("mastomys", "mus", "praomys", "rattus", "hylomyscus", "hybomys",
                                                "dasymys", "lemniscomys", "malacomys", "gerbillinae", "crocidura",
                                                "lophuromys")),
               species_priority = case_when(as.numeric(genus) <= 4 ~ 1,
                                            as.numeric(genus) <= 9 ~ 2,
                                            TRUE ~ 3)) %>%
        arrange(species_priority, research_visit) %>%
        mutate(sample_priority = row_number() + max(allocated_priority$sample_priority)) %>%
        left_join(process) %>%
        bind_rows(allocated_priority_progress, .)
      
      write_rds(combined, here("data", "clean_data", "lab", paste0("allocated_priority_", str_replace_all(Sys.Date(), "-", "_"), ".rds")))
      
    }
  
}
  