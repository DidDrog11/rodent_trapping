source(here::here("scripts", "0_project_library.R"))
source(here("scripts", "0_DdM_to_decimal_degrees.R"))

drive_download("https://drive.google.com/file/d/1kxpH6RvWgAMwhpqZ4yoH_6SYso06uuDa/view?usp=sharing", path = here("data", "trap_sites_all.xlsx"), overwrite = T)

readxl::read_xlsx(path = here("data", "trap_sites_all.xlsx"), sheet = 2) %>%
  mutate(lon_DdM = paste(paste(lon_degree, lon_dec, sep = "_"), "'", sep = ""),
         lat_DdM = paste(paste(lat_degree, lat_dec, sep = "_"), "'", sep = ""),
         lon = -1 * dg2dec(var = lon_DdM, Dg = "_", Min = "'"),
         lat = dg2dec(var = lat_DdM, Dg = "_", Min = "'")) %>%
  dplyr::select(-c("lat_degree", "lat_dec", "lon_degree", "lon_dec", "lon_DdM", "lat_DdM")) %>%
  write_csv(here("data", "trap_sites.csv")) #Read the data file from excel document and save within the repo as csv
  
trapped_rodents <- readxl::read_xlsx(path = here("data", "trap_sites_all.xlsx"), sheet = 3) %>%
  mutate(visit = case_when(str_starts(rodent_id, "[A-Z]") ~ "1",
                           TRUE ~ str_sub(rodent_id, start = 1, end = 1))) %>%
  write_csv(here("data", "rodents_trapped.csv"))
rodent_ids <- readxl::read_xlsx(path = here("data", "trap_sites_all.xlsx"), sheet = 4) %>%
  write_csv(here("data", "rodent_ids.csv"))
