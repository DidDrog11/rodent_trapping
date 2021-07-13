library(ruODK)
t <- tempdir()

ru_setup(verbose = T) # Using R environment variables

all_forms <- form_list()$name

rodent_data <- submission_export(local_dir = here("data", "raw_odk"), fid = all_forms[4], media = F) %>%
  unzip(., exdir = here("data", "raw_odk", "rodent_data")) %>%
  read_csv()

trap_sites <- submission_export(local_dir = here("data", "raw_odk"), fid = all_forms[5], media = F) %>%
  unzip(., exdir = here("data", "raw_odk", "trap_sites")) %>%
  read_csv()

trap_check <- submission_export(local_dir = here("data", "raw_odk"), fid = all_forms[6], media = F) %>%
  unzip(., exdir = here("data", "raw_odk", "trap_check")) %>%
  read_csv()
