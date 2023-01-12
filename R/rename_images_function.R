rename_images <- function(data = all_rodents, new_images = TRUE, delete_old_images = TRUE) {
  
  if(new_images == TRUE) {
    
    if(delete_old_images == TRUE) {
      
      media_files <- sort(list.files(here("data", "raw_odk"), pattern = "rodent_data_", full.names = TRUE)) %>%
        tail(1)
      
      filenames <- tibble(full_name = list.files(here(media_files, "media"), pattern = ".jpg", full.names = TRUE)) %>%
        mutate(base_name = basename(full_name))
      
      outdirectory <- here("data", "rodent_images", "rodent_matched")
      
      dir.create(here("data", "rodent_images"))
      dir.create(outdirectory)
      file_delete(list.files(outdirectory, full.names = TRUE))
      
      message("Previous images deleted")
      
      matched_dorsal <- data %>%
        select(rodent_uid, dorsal_image_id) %>%
        drop_na(dorsal_image_id) %>%
        left_join(., filenames, by = c("dorsal_image_id" = "base_name")) %>%
        mutate(new_name = paste0(rodent_uid, "a.jpg")) %>%
        rename(current_filename = dorsal_image_id)
      
      matched_ventral <- data %>%
        select(rodent_uid, ventral_image_id) %>%
        drop_na(ventral_image_id) %>%
        left_join(., filenames, by = c("ventral_image_id" = "base_name")) %>%
        mutate(new_name = paste0(rodent_uid, "b.jpg")) %>%
        rename(current_filename = ventral_image_id)
      
      matched_both <- bind_rows(matched_dorsal, matched_ventral) %>%
        select(full_name, new_name)
      
      unmatched <- filenames %>%
        filter(!full_name %in% matched_both$full_name)
      
      if(nrow(unmatched) == 0) message("No unmatched images") else message("Unmatched images require checking")
      
      file.rename(matched_both$full_name, paste0(here("data", "rodent_images", "rodent_matched//"), matched_both$new_name)) 
      
    } else {
      
      media_files <- sort(list.files(here("data", "raw_odk"), pattern = "rodent_data_", full.names = TRUE)) %>%
        tail(1)
      
      filenames <- tibble(full_name = list.files(here(media_files, "media"), pattern = ".jpg", full.names = TRUE)) %>%
        mutate(base_name = basename(full_name))
      
      outdirectory <- here("data", "rodent_images", "rodent_matched")
      
      dir.create(here("data", "rodent_images"))
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
      
      unmatched <- filenames %>%
        filter(!full_name %in% matched_both$full_name)
      
      if(nrow(unmatched) == 0) message("No unmatched images") else message("Unmatched images require checking")
      
      file.rename(matched_both$full_name, paste0(here("data", "rodent_images", "rodent_matched//"), matched_both$new_name))   
      
      
    }
    
  } else if(new_images == FALSE) {
    
    message("No new images downloaded")
    
  }
  
  file_copy(path = list.files(here("data", "rodent_images", "from_email"), pattern = ".jpg", full.names = TRUE), new_path = here("data", "rodent_images", "rodent_matched"), overwrite = TRUE)
  
  message("Emailed images copied into rodent_matched folder")
  
  all_images <- tibble(file = list.files(here("data", "rodent_images", "rodent_matched"))) %>% 
    regex_right_join(data %>%
                       select(rodent_uid), by = c(file = "rodent_uid"))
  
  return(all_images)
}