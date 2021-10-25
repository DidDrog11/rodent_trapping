# Use tar_make() to run this script

# Load packages and functions
suppressPackageStartupMessages(source(here::here("packages.R")))
walk(dir_ls(here("R")),  ~try(source(.)))

# Upate the data if required
source(here::here("R", "get_ODK_function.R"))

# Set up parallelism (no of targets that can run simultaneously)
tar_config_set(workers = get_num_cores())

# Targets
list(
  
  # Clean the site data from ODK
  tar_target(ODK_sites, clean_site_ODK()),
  
  # Clean the trap location data from ODK
  tar_target(ODK_traps, clean_trap_locations_ODK(trap_sites = ODK_sites$trap_sites)),
  
  # Clean the trap check data from ODK
  tar_target(ODK_trap_check, clean_trap_check_ODK()),
  
  # Clean the rodent data from ODK
  tar_target(ODK_rodents, clean_rodent_data_ODK()),
  
  # Combine the ODK forms into a single dataset
  tar_target(ODK_combined, combine_ODK_data(trap = ODK_traps, check = ODK_trap_check, rodent = ODK_rodents)))
  # The initial visits to Lalehun and Seilama used paper forms for data collection we add these to the ODK data
  # tar_target(all_traps, ODK_paper_combine(ODK_data = ODK_traps))
