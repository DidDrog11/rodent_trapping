# Use tar_make() to run this script

# Load packages and functions
suppressPackageStartupMessages(source(here::here("packages.R")))
walk(dir_ls(here("R")),  ~try(source(.)))

# Visits have been confusing for the field team. Set up dataframe to handle visits based on when the forms were submitted.
visit_dates <- tibble(year = c(rep(2021, 11), rep(2022, 17)),
                      month = c(6, 6, 7, rep(7, 3), rep(10, 4), 11, rep(1, 4), 2, rep(4, 4), rep(8, 3), 10, rep(10, 2), rep(11, 2)),
                      village = c(rep("bambawo", 1), rep("lambayama", 2), "baiama", "lalehun", "seilama", "lalehun", "seilama", "baiama", rep("lambayama", 2), "lalehun", "seilama", "baiama", rep("lambayama", 2), "lalehun", "seilama", "baiama","lambayama", "lalehun", "baiama", "lambayama", "seilama", "lalehun", "lambayama", "baiama", "seilama"),
                      visit = c(rep(1, 4), rep(3, 2), rep(4, 2), rep(2, 3), rep(5, 2), rep(3, 3), rep(6, 2), rep(4, 2), rep(7, 4), rep(8, 4)))

# If using rodent images set to TRUE
download_rodent_pictures = FALSE

# Update the data if required
get_ODK()

# Update the local data
# If the study visit has been incorrectly input into the forms, correct in the following function
ODK_sites <- clean_site_ODK()
# If coordinates have been incorrectly input into the forms, correct in the following function
ODK_traps <- clean_trap_locations_ODK()
# fix the obvious errors, i.e. swapped lat/lon and including the degrees before changing the improbables manually in specific the function
# check using mapview::mapview(ODK_traps$coord_error$spatial)

ODK_trap_check <- clean_trap_check_ODK()
# Visit dates need to be added within the function before running
ODK_rodents <- clean_rodent_data_ODK()

# Combine the ODK forms
ODK_combined <- combine_ODK_data(trap = ODK_traps$full_trap_locations, check = ODK_trap_check, rodent = ODK_rodents)
# 20 rodents currently unmatched

# Combine the paper and ODK forms
all_traps <- ODK_paper_combine(ODK_data = ODK_combined)
all_rodents <- ODK_paper_combine_rodent(ODK_data = ODK_rodents)

# Site consistency relabel trap grids based on locations, rather than numbers that the study team may have incorrectly put in
# The numbering used at site setup will be used as the reference
consistent_traps <- harmonise_sites()

# Rename images stored in data/rodent_images only needed if download_rodent_pictures was set to TRUE, otherwise would have previously been done.
all_images <- rename_images(new_images = FALSE, delete_old_images = TRUE)

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
