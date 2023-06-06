get_ODK <- function(force_update){
  
  t <- tempdir()
  
  # I have previously set up the ODK central access variables in the .Renviron
  ru_setup(verbose = T) # Using R environment variables
  
  all_forms <- form_list()$name
  
  if (force_update == TRUE) {
    
    download_new = TRUE
    
  } else if(file.exists(here("data", "raw_odk", "last_submission.rds"))) {
    last_submission <- read_rds(here("data", "raw_odk", "last_submission.rds"))
    
    recent_submission <- max(form_list()$last_submission, na.rm = TRUE)
    
    download_new <- recent_submission > last_submission
    
    write_rds(recent_submission, here("data", "raw_odk", "last_submission.rds"))
    
  } else {
    
    download_new = TRUE
    
  } 
  
  if(download_rodent_pictures == TRUE) {
    
    if(download_new == TRUE) {
      
      rodent_data <- submission_export(local_dir = here("data", "raw_odk"), fid = "rodent_v1", media = T) %>%
        unzip(., exdir = here("data", "raw_odk", paste0("rodent_data", "_", Sys.Date())))
      
      trap_sites <- submission_export(local_dir = here("data", "raw_odk"), fid = "site_setup_v2", media = F) %>%
        unzip(., exdir = here("data", "raw_odk", paste0("trap_sites", "_", Sys.Date())))
      
      trap_check <- submission_export(local_dir = here("data", "raw_odk"), fid = "trap_check_v1", media = F) %>%
        unzip(., exdir = here("data", "raw_odk", paste0("trap_check", "_", Sys.Date())))
      
    } 
  } else {
    
    rodent_data <- submission_export(local_dir = here("data", "raw_odk"), fid = "rodent_v1", media = F) %>%
      unzip(., exdir = here("data", "raw_odk", paste0("rodent_data", "_", Sys.Date())))
    
    trap_sites <- submission_export(local_dir = here("data", "raw_odk"), fid = "site_setup_v2", media = F) %>%
      unzip(., exdir = here("data", "raw_odk", paste0("trap_sites", "_", Sys.Date())))
    
    trap_check <- submission_export(local_dir = here("data", "raw_odk"), fid = "trap_check_v1", media = F) %>%
      unzip(., exdir = here("data", "raw_odk", paste0("trap_check", "_", Sys.Date())))
    
  }
}