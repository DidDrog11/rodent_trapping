ODK_paper_combine <- function(ODK_data = ODK_traps) {
  
  drive_download("https://drive.google.com/file/d/1kxpH6RvWgAMwhpqZ4yoH_6SYso06uuDa/view?usp=sharing",
                 path = here("data", "trap_sites_all.xlsx"), overwrite = T) # Download data that was captured initially on paper forms
  
  
  
}