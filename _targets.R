# Use tar_make() to run this script
# Load packages and functions
suppressPackageStartupMessages(source(here::here("packages.R")))
# Load in functions and dictionaries, ignoring miscellaneous scripts
walk(dir_ls(here("R"))[str_detect(dir_ls(here("R")), "_function.R|dictionary|project_wide")],  ~try(source(.)))

# Visits have been confusing for the field team. Read in an .xlsx file which contains the year, month, day, village and visit number to use as a reference based on when files were uploaded to central.
visit_dates <- read_xlsx(here("data", "visit_dates.xlsx"))

# If using rodent images set to TRUE
download_rodent_pictures = TRUE

# Update the data if required
get_ODK()

# Update the local data
# If the study visit has been incorrectly input into the forms, correct in the following function
ODK_sites <- clean_site_ODK()
# If coordinates have been incorrectly input into the forms, correct in the following function
ODK_traps <- clean_trap_locations_ODK()
# fix the obvious errors, i.e. swapped lat/lon and including the degrees before changing the improbables manually in specific the function
# check using mapview::mapview(ODK_traps$full_trap_locations_spatial, z = "grid_number")

# Impute locations of traps with missing data using an .xlsx file to identify the grids and visits needed to be imputed
# Imputation will occur using the closest location of that trap number in time, i.e., last value carried forward
impute_grids <- read_xlsx(here("data", "missing_grids.xlsx"))
# The traps are slightly off for a lot of the visit 10 grids, these will be corrected using the imputation where necessary
# This was because a different GPS device was used which is older and apparently has some offset
imputed_traps <- impute_traps()

# To add the imputed traps uncomment this line, but check the necessity of including them in missing_grids.xlsx first
# Currently locations of 1027 traps are imputed
ODK_traps$full_trap_locations <- bind_rows(ODK_traps$full_trap_locations,
                                           imputed_traps)

ODK_trap_check <- clean_trap_check_ODK()
# Visit dates need to be added within the function before running
ODK_rodents <- clean_rodent_data_ODK()

# Combine the ODK forms
ODK_combined <- combine_ODK_data(trap = ODK_traps$full_trap_locations, check = ODK_trap_check, rodent = ODK_rodents)
# 9 rodents currently unmatched

# Combine the paper and ODK forms
all_traps <- ODK_paper_combine(ODK_data = ODK_combined)
all_rodents <- ODK_paper_combine_rodent(ODK_data = ODK_rodents)

# Site consistency relabel trap grids based on locations, rather than numbers that the study team may have incorrectly put in
# The numbering used at site setup will be used as the reference
# consistent_traps <- harmonise_sites()

# Assess final locations and grid numbers
test_locations <- st_as_sf(all_traps %>%
                             distinct(village, visit, grid_number, trap_number, .keep_all = TRUE),
                           coords = c("lon", "lat"),
                           crs = project_crs)
mapview::mapview(test_locations, z = "grid_number")

# Rename images stored in data/rodent_images only needed if download_rodent_pictures was set to TRUE, otherwise would have previously been done.
all_images <- rename_images(new_images = TRUE, delete_old_images = TRUE)

# 92 expected images are not provided
table(is.na(all_images$file))

# Associate trapped rodents with locations
# Assign habitats based on initial classification
# Assign current use based on data entry for the that visit
final_cleaned_trap_data <- final_cleaning(trap_data = all_traps, rodent_data = all_rodents, site_data = ODK_sites$site_habitats)
final_cleaned_rodent_data <- final_cleaning_rodents(rodent_data = all_rodents)

# Sequencing of rodents is being completed and processed in a different repository SL_rodent_PCR (https://github.com/DidDrog11/SL_rodent_PCR)
rodent_sequences <- add_molecular_identification(data = final_cleaned_rodent_data)

need_checking <- rodent_sequences[[1]]

final_rodent_data <- rodent_sequences[[2]]

# Spatial dataframe of trap site locations
final_cleaned_trap_data$spatial_data <- st_as_sf(final_cleaned_trap_data$clean_sites, coords = c("lon", "lat")) %>%
  st_set_crs(value = project_crs)

write_rds(final_cleaned_trap_data$spatial_data, here("data", "clean_data", "spatial_traps", "trap_spatial.rds"))

# Repositories have been created for chapter 3 and 4. These use this cleaned data, the next function produces these datasets.
save_for_chapters()
