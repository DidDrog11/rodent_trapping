plot_traps_interactively <- function(data = final_cleaned_trap_data$spatial_data) {
  
  plot_traps <- function(village_name) {
    
    pal <- colorFactor(palette = c("#f1a340", "#998ec3"), domain = data$rodent_trapped)
    
    capture <- data %>%
      filter(village == village_name & rodent_trapped == "yes") %>%
      distinct(visit, trap_number, .keep_all = TRUE)
    
    non_capture <- data %>%
      filter(village == village_name & rodent_trapped == "no") %>%
      distinct(visit, trap_number, .keep_all = TRUE) %>%
      filter(!geometry %in% capture$geometry)
    
    combined <- bind_rows(capture, non_capture)
    
    leaflet(combined) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addCircleMarkers(color = ~pal(rodent_trapped),
                       radius = 3, 
                       stroke = FALSE,
                       fillOpacity = 1,
                       popup = paste("Trap UID:", combined$trap_uid,
                                     "Rodent UID:", combined$rodent_uid),
                       popupOptions = popupOptions(closeOnClick = TRUE)) %>%
      addLegend("topright", pal = pal, values = ~rodent_trapped,
                title = "Successful capture")
    
  }
  
  lalehun <- plot_traps("lalehun")
  seilama <- plot_traps("seilama")
  baiama <- plot_traps("baiama")
  bambawo <- plot_traps("bambawo")
  lambayama <- plot_traps("lambayama")
  
  return(list(lalehun_leaflet = lalehun,
              sailama_leaflet = seilama,
              baiama_leaflet = baiama,
              bambawo_leaflet = bambawo,
              lambayama_leaflet = lambayama))
  
}
