save_for_chapters <- function(trap_data = final_cleaned_trap_data$spatial_data, rodent_data = final_rodent_data) {
  
  combined_data <- list(trap_data = trap_data,
                        rodent_data = rodent_data)
  
  write_rds(combined_data, here("data", "data_for_export", "combined_data.rds"))
  
}
