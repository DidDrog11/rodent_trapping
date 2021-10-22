# Use tar_make() to run this script

# Load packages and functions
suppressPackageStartupMessages(source(here::here("packages.R")))
walk(dir_ls(here("R")),  ~try(source(.)))

# Set up parallelism (no of targets that can run simultaneously)
tar_config_set(workers = get_num_cores())

# Targets
list(
  
  # Access the data from ODK
  tar_target(ODK_api, get_ODK())
)