impute_traps <- function() {
  
  known_traps <- ODK_traps$full_trap_locations %>%
    select(village, visit, grid_number, trap_number, lon, lat) %>%
    distinct()
  
  unrecorded_traps <- impute_grids %>%
    uncount(n_traps, .id = "trap_number") %>%
    mutate(trap_number = starting_trap + (trap_number - 1)) %>%
    select(village, visit, grid_number, trap_number) %>%
    mutate(grid_number = factor(grid_number)) 
  
  imputed_traps <- unrecorded_traps %>%
    left_join(known_traps, by = c("village", "grid_number", "trap_number")) %>%
    mutate(difference_visits = abs(visit.x - visit.y)) %>%
    group_by(village, visit.x, grid_number, trap_number) %>%
    arrange(difference_visits) %>%
    slice(1) %>%
    select(village, visit = visit.x, grid_number, trap_number, lon, lat)
  
  add_date <- ODK_traps$full_trap_locations %>%
    select(date_set, village, visit, trap_night) %>%
    group_by(village, visit) %>%
    filter(trap_night == 1) %>%
    filter(date_set == median(date_set)) %>%
    distinct() %>%
    right_join(imputed_traps, by = c("village", "visit")) %>%
    mutate(date_set = dt_case_when(is.na(date_set) ~ ymd("2023-02-17"),
                                   TRUE ~ date_set))
  
  add_habitat <- ODK_traps$full_trap_locations %>%
    select(village, trap_number, site_use, intensity, crop_type, habitat, proximity, trap_land_type) %>%
    drop_na(habitat) %>%
    distinct(village, trap_number, .keep_all = TRUE) %>%
    right_join(add_date, by = c("village", "trap_number"))
  
  add_trap_night <- add_habitat %>%
    mutate(trap_night = 4) %>%
    uncount(trap_night, .id = "trap_night")
  
  final_imputed <- add_trap_night %>%
    select(any_of(names(ODK_traps$full_trap_locations))) %>%
    mutate(trap_uid = paste0(village, "_", visit, "_", trap_night, "_", grid_number, "_", trap_number))
  
  return(final_imputed)

}
