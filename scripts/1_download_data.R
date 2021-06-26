source(here::here("scripts", "0_project_library.R"))
source(here("scripts", "DdM_to_decimal_degrees_function.R"))
source(here("scripts", "ODK_sites_function.R"))

drive_download("https://drive.google.com/file/d/1kxpH6RvWgAMwhpqZ4yoH_6SYso06uuDa/view?usp=sharing", path = here("data", "trap_sites_all.xlsx"), overwrite = T)
drive_download("https://drive.google.com/file/d/1Dou7eF6c0WoUA2zS3HkOT-zZ6ETOjRIB/view?usp=sharing", path = here("data", "ODK_trap_sites.csv"), overwrite = T)
drive_download("https://drive.google.com/file/d/1FYiGyZi_Nk3nXBwckfxBQ0JMTErj-FFf/view?usp=sharing", path = here("data", "ODK_trap_locations.csv"), overwrite = T)

odk_forms_trap <- clean_odk_sites()

paper_forms_trap <- readxl::read_xlsx(path = here("data", "trap_sites_all.xlsx"), sheet = 2) %>%
  mutate(lon_DdM = paste(paste(lon_degree, lon_dec, sep = "_"), "'", sep = ""),
         lat_DdM = paste(paste(lat_degree, lat_dec, sep = "_"), "'", sep = ""),
         lon = -1 * dg2dec(var = lon_DdM, Dg = "_", Min = "'"),
         lat = dg2dec(var = lat_DdM, Dg = "_", Min = "'"),
         date_set = as.Date(date_set)) %>%
  dplyr::select(-c("lat_degree", "lat_dec", "lon_degree", "lon_dec", "lon_DdM", "lat_DdM"))

trap_sites <- bind_rows(paper_forms_trap, odk_forms_trap) %>%
  select(-`...20`) %>%
  write_csv(here("data", "trap_sites.csv")) #Read the data file from excel document and save within the repo as csv
  
trapped_rodents <- readxl::read_xlsx(path = here("data", "trap_sites_all.xlsx"), sheet = 3) %>%
  mutate(visit = case_when(str_starts(rodent_id, "[A-Z]") ~ "1",
                           TRUE ~ str_sub(rodent_id, start = 1, end = 1))) %>%
  write_csv(here("data", "rodents_trapped.csv"))

rodent_ids <- readxl::read_xlsx(path = here("data", "trap_sites_all.xlsx"), sheet = 4) %>%
  write_csv(here("data", "rodent_ids.csv"))
