harmonise_sites_function <- function(data = all_traps) {
  
  # convert all indoor traps to grid_number = 7
  data <- data %>%
    mutate(grid_number = case_when(str_detect(trap_land_type, "indoors") ~ as.numeric(7),
                                   !is.na(number_of_adults) ~ as.numeric(7),
                                   TRUE ~ as.numeric(grid_number)))
  
  ## Lalehun
  
  reference_sites_lal <- data %>%
    filter(village == "lalehun" & visit == 1) %>%
    st_as_sf(coords = c("lon", "lat"), crs = project_crs) %>%
    group_by(visit, grid_number) %>%
    summarise(geometry = st_union(geometry)) %>%
    st_convex_hull() %>%
    st_buffer(dist = units::as_units(50, "metres")) %>%
    group_by(grid_number) %>%
    group_split()
  
  check_lal_grids <- data %>%
    filter(village == "lalehun" & visit != 1 & grid_number != 7) %>%
    st_as_sf(coords = c("lon", "lat"), crs = project_crs) %>%
    group_by(grid_number) %>%
    group_split()
    
  lal_check <- list()
  
  for(i in 1:length(reference_sites_lal)) {
  
  reference <- bind_rows(reference_sites_lal)
      
  df <- check_lal_grids[[i]][!st_within(check_lal_grids[[i]], reference_sites_lal[[i]]) %>%
    lengths > 0, ]
   
  plot <- try(leaflet(reference) %>%
                addPolygons(popup = paste0("Grid number, ", reference$grid_number)) %>%
                addTiles() %>%
                addCircleMarkers(data = df, stroke = FALSE, weight = 1, radius = 4,
                                 popup = paste0(df$trap_uid, ", grid number ", df$grid_number)))
   
  lal_check[[i]] <- list(df = df,
                     plot = plot)
  }
  
  ## Seilama
  
  reference_sites_sei <- data %>%
    filter(village == "seilama" & visit == 1) %>%
    st_as_sf(coords = c("lon", "lat"), crs = project_crs) %>%
    group_by(visit, grid_number) %>%
    summarise(geometry = st_union(geometry)) %>%
    st_convex_hull() %>%
    st_buffer(dist = units::as_units(50, "metres")) %>%
    group_by(grid_number) %>%
    group_split()
  
  check_sei_grids <- data %>%
    filter(village == "seilama" & visit != 1 & grid_number != 7) %>%
    st_as_sf(coords = c("lon", "lat"), crs = project_crs) %>%
    group_by(grid_number) %>%
    group_split()
  
  sei_check <- list()
  
  for(i in 1:length(reference_sites_sei)) {
    
    reference <- bind_rows(reference_sites_sei)
    
    df <- check_sei_grids[[i]][!st_within(check_sei_grids[[i]], reference_sites_sei[[i]]) %>%
                                 lengths > 0, ]
    
    plot <- try(leaflet(reference) %>%
                  addPolygons(popup = paste0("Grid number, ", reference$grid_number)) %>%
                  addTiles() %>%
                  addCircleMarkers(data = df, stroke = FALSE, weight = 1, radius = 4,
                                   popup = paste0(df$trap_uid, ", grid number ", df$grid_number)))
    
    sei_check[[i]] <- list(df = df,
                           plot = plot)
  }
  
  ## Baiama
  
  reference_sites_bai <- data %>%
    filter((village == "baiama" & visit == 1 & grid_number %in% c(1, 2, 3, 4)) |
             (village == "baiama" & visit == 2 & grid_number %in% c(5, 6))) %>%
    st_as_sf(coords = c("lon", "lat"), crs = project_crs) %>%
    group_by(visit, grid_number) %>%
    summarise(geometry = st_union(geometry)) %>%
    st_convex_hull() %>%
    st_buffer(dist = units::as_units(50, "metres")) %>%
    group_by(grid_number) %>%
    group_split()
  
  check_bai_grids <- data %>%
    filter(village == "baiama" & visit != 1 & grid_number != 7) %>%
    st_as_sf(coords = c("lon", "lat"), crs = project_crs) %>%
    group_by(grid_number) %>%
    group_split()
  
  bai_check <- list()
  
  for(i in 1:length(reference_sites_bai)) {
    
    reference <- bind_rows(reference_sites_bai)
    
    df <- check_bai_grids[[i]][!st_within(check_bai_grids[[i]], reference_sites_bai[[i]]) %>%
                                 lengths > 0, ]
    
    plot <- try(leaflet(reference) %>%
                  addPolygons(popup = paste0("Grid number, ", reference$grid_number)) %>%
                  addTiles() %>%
                  addCircleMarkers(data = df, stroke = FALSE, weight = 1, radius = 4,
                                   popup = paste0(df$trap_uid, ", grid number ", df$grid_number)))
    
    bai_check[[i]] <- list(df = df,
                           plot = plot)
  }
  
  ## Lambayama
  
  reference_sites_lam <- data %>%
    filter((village == "lambayama" & visit == 1 & grid_number %in% c(1, 2, 3, 4)) |
             (village == "lambayama" & visit == 2 & grid_number %in% c(5, 6))) %>%
    st_as_sf(coords = c("lon", "lat"), crs = project_crs) %>%
    group_by(visit, grid_number) %>%
    summarise(geometry = st_union(geometry)) %>%
    st_convex_hull() %>%
    st_buffer(dist = units::as_units(50, "metres")) %>%
    group_by(grid_number) %>%
    group_split()
  
  check_lam_grids <- data %>%
    filter(village == "lambayama" & visit != 1 & grid_number != 7) %>%
    st_as_sf(coords = c("lon", "lat"), crs = project_crs) %>%
    group_by(grid_number) %>%
    group_split()
  
  lam_check <- list()
  
  for(i in 1:length(reference_sites_lam)) {
    
    reference <- bind_rows(reference_sites_lam)
    
    df <- check_lam_grids[[i]][!st_within(check_lam_grids[[i]], reference_sites_lam[[i]]) %>%
                                 lengths > 0, ]
    
    plot <- try(leaflet(reference) %>%
                  addPolygons(popup = paste0("Grid number, ", reference$grid_number)) %>%
                  addTiles() %>%
                  addCircleMarkers(data = df, stroke = FALSE, weight = 1, radius = 4,
                                   popup = paste0(df$trap_uid, ", grid number ", df$grid_number)))
    
    lam_check[[i]] <- list(df = df,
                           plot = plot)
  }
  
}
