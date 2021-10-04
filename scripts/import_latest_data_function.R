latest_data <- function(start_of_filename = c("rodents", "trap_sites"), clean = FALSE, suppress = FALSE) {
  if(clean == FALSE) { 
    filename <- tibble(files = list.files(path = here("data", "clean_data", start_of_filename)[1], full.names = F)) %>%
      arrange(files) %>%
      filter(!str_starts(files, "clean_")) %>%
      tail(., n=1) %>%
      pull() %>%
      paste0(here("data", "clean_data", start_of_filename), "/", .)
  
    if(suppress == FALSE) {
    message(cat(
      paste0("Read in file: ", filename)
    ))
    }
    
  return(read_csv(filename, col_types = cols()))
  } else {
    filename <- tibble(files = list.files(path = here("data", "clean_data", start_of_filename)[1], full.names = F)) %>%
      arrange(files) %>%
      filter(str_starts(files, "clean_")) %>%
      tail(., n=1) %>%
      pull() %>%
      paste0(here("data", "clean_data", start_of_filename), "/", .)
    
    if(suppress == FALSE) {
      message(cat(
        paste0("Read in file: ", filename)
      ))
    }
    
    return(read_csv(filename, col_types = cols()))
  }
}

save_data <- function(data = "object_name", start_of_filename = c("rodents", "trap_sites")) {
  filename <- tibble(files = list.files(path = here("data", "clean_data", start_of_filename)[1])) %>%
    mutate(files = as_factor(files)) %>%
    arrange(files) %>%
    tail(., n=1)
  
  write_csv(data, here("data", "clean_data", start_of_filename, paste0("clean_", pull(filename))))
}
