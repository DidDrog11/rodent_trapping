# Use tar_make() to run this script

# Load packages and functions
suppressPackageStartupMessages(source(here::here("packages.R")))
walk(dir_ls(here("R")),  ~try(source(.)))

# Update the data if required
get_ODK()

# Update the local data
ODK_sites <- clean_site_ODK()
ODK_traps <- clean_trap_locations_ODK()
ODK_trap_check <- clean_trap_check_ODK()
ODK_rodents <- clean_rodent_data_ODK()

# Combine the forms
ODK_combined <- combine_ODK_data(trap = ODK_traps$full_trap_locations, check = ODK_trap_check, rodent = ODK_rodents)

all_traps <- ODK_paper_combine(ODK_data = ODK_combined)
all_rodents <- ODK_paper_combine_rodent(ODK_data = ODK_rodents)

# Associate trapped rodents with locations
final_cleaned_trap_data <- final_cleaning(trap_data = all_traps, rodent_data = all_rodents)
final_cleaned_rodent_data <- final_cleaning_rodents(rodent_data = all_rodents)

# Rodent speciation values
rodent_speciation <- read_species_characteristics()

# SLE Raster
sle_raster <- generate_raster()

# Landuse plots
landuse_plots <- plot_landuse(data = sle_raster)

# save plots if not previously saved
save_landuse_plots()

# View traps on leaflet maps
view_traps <- plot_traps_interactively(final_cleaned_trap_data$clean_sites)

# Produce a species accumulation curve for the study up to the current trapping visit
species_accumulation <- derive_accumulation_curves()

# Trap site landuse
landuse_trap_site <- plot_landuse_trapsites(data = sle_raster)

# Trap success rate
trap_success_plot <- plot_trap_success(data = final_cleaned_trap_data$clean_sites)


# Set up parallelism (no of targets that can run simultaneously)
tar_config_set(workers = get_num_cores())

# Targets
list(
  
  ### DATA CLEANING STEPS ###
  
  # Clean the site data from ODK
  # tar_target(ODK_sites, clean_site_ODK()),
  
  # Clean the trap location data from ODK
  # tar_target(ODK_traps, clean_trap_locations_ODK(trap_sites = ODK_sites$trap_sites)),
  
  # Clean the trap check data from ODK
  # tar_target(ODK_trap_check, clean_trap_check_ODK()),
  
  # Clean the rodent data from ODK
  # tar_target(ODK_rodents, clean_rodent_data_ODK()),
  
  # Combine the ODK forms into a single dataset
  # tar_target(ODK_combined, combine_ODK_data(trap = ODK_traps, check = ODK_trap_check, rodent = ODK_rodents)),
  
  # The initial visits to Lalehun and Seilama used paper forms for data collection we add these to the ODK data
  # tar_target(all_traps, ODK_paper_combine(ODK_data = ODK_combined)),
  
  # We combine these data for trapped rodents too
  # tar_target(all_rodents, ODK_paper_combine_rodent(ODK_data = ODK_rodents)),
  
  # Ensure all trapped rodents are associated with their trapping location
  # tar_target(final_cleaned_trap_data, final_cleaning(trap_data = all_traps, rodent_data = all_rodents)),
  
  # Producing the final cleaned rodent data
  # tar_target(final_cleaned_rodent_data, final_cleaning_rodents(rodent_data = all_rodents)),
  
  # I am developing a tool to assist in the identification of rodent species, this loads in the data
  # tar_target(rodent_speciation, read_species_characteristics()),
  
  ### ANALYSIS STEPS ###
  
  # Produce landuse raster
  # tar_target(sle_raster, generate_raster()),
  
  # Plot landuse raster for the eastern province and study sites
  # tar_target(landuse_plots, plot_landuse(data = sle_raster))
  
  )
