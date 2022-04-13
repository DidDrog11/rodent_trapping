rename_images <- function(data = all_rodents) {
  
  filenames <- tibble(full_name = list.files(here("data", "rodent_images", "odk_named"), pattern="*.jpg", full.names = TRUE)) %>%
    mutate(base_name = basename(full_name))
  
  outdirectory <- here("data", "rodent_images", "rodent_matched")
  
  dir.create(outdirectory)
  
  matched_dorsal <- data %>%
    select(rodent_uid, dorsal_image_id) %>%
    left_join(., filenames, by = c("dorsal_image_id" = "base_name")) %>%
    mutate(new_name = paste0(rodent_uid, "a.jpg")) %>%
    rename(current_filename = dorsal_image_id) %>%
    drop_na()
  
  matched_ventral <- data %>%
    select(rodent_uid, ventral_image_id)%>%
    left_join(., filenames, by = c("ventral_image_id" = "base_name")) %>%
    mutate(new_name = paste0(rodent_uid, "b.jpg")) %>%
    rename(current_filename = ventral_image_id) %>%
    drop_na()
  
  matched_both <- bind_rows(matched_dorsal, matched_ventral) %>%
    select(full_name, new_name)
  
  file.rename(matched_both$full_name, paste0(here("data", "rodent_images", "rodent_matched/"), matched_both$new_name))
  
}