library("here")
source(here("scripts", "project_library.R"))

drive_download("https://drive.google.com/file/d/1kxpH6RvWgAMwhpqZ4yoH_6SYso06uuDa/view?usp=sharing", path = here("data", "trap_sites_all.xlsx"), overwrite = T)

readxl::read_xlsx(path = here("data", "trap_sites_all.xlsx"), sheet = 2) %>%
  write_csv(here("data", "trap_sites.csv")) #Read the data file from excel document and save within the repo as csv
trapped_rodents <- readxl::read_xlsx(path = here("data", "trap_sites_all.xlsx"), sheet = 3) %>%
  write_csv(here("data", "rodents_trapped.csv"))
rodent_ids <- readxl::read_xlsx(path = here("data", "trap_sites_all.xlsx"), sheet = 4) %>%
  write_csv(here("data", "rodent_ids.csv"))