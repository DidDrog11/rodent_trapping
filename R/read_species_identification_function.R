read_species_characteristics <- function(){
  
  rodent_identification <- readxl::read_xlsx(path = here("data", "trap_sites_all.xlsx"), sheet = 4) %>%
    write_csv(here("data", "rodent_ids.csv"))
  
  return(rodent_identification)
  
}