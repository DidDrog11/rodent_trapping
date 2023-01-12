add_molecular_identification <- function(data) {
  
  combined_data <- readRDS(gzcon(url("https://github.com/DidDrog11/SL_rodent_PCR/raw/main/data/output/rodent_sequences.rds")))
  
  require_checking <- combined_data$require_checking %>%
    select(rodent_uid, blastn, blastn_identity, field_identification = clean_names)
  
  rodent_sequences <- combined_data$rodent_sequences %>%
    select(rodent_uid, species = blastn, sequence_identity = blastn_identity)
  
  rodent_sequences <- data %>%
    rename(field_id = clean_names) %>%
    left_join(rodent_sequences, by = c("rodent_uid")) %>%
    select(any_of(c("rodent_uid", "trap_uid", "research_visit", "species", "sequence_identity", "field_id", "genus", "sub_family", "family",
                    "sex", "age_group", "weight", "head_body", "cut_tail", "hind_foot", "ear", "length_skull", "testes", "seminal_vesicles",
                    "vagina_perforate", "teats_visible", "pairs_teats", "number_embryos", "notes", "all_samples", "dorsal_image_id",
                    "ventral_image_id", "source_data")))

  return(list(require_checking,
              rodent_sequences))
}

