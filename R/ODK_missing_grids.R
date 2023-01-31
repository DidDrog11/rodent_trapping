a <- trap %>% 
  group_by(village, visit, grid_number) %>%
  summarise(n = n())


lal_4_traps <- trap %>%
  filter(village == "lalehun" & visit == 4) %>%
  distinct(trap_number, .keep_all = TRUE)


# Lalehun visit 4 ---------------------------------------------------------


## Lalehun grid 4 ----------------------------------------------------------

lal_4 <- trap %>%
  filter(village == "lalehun" & grid_number == 4) %>%
  distinct(lon, lat, .keep_all = TRUE) %>%
  st_as_sf(., coords = c("lon", "lat"), crs = project_crs)

SL_UTM <- "EPSG:32629"

cell_size <- 7
units(cell_size) <- "m"

mapview::mapview(lal_4, map.types = "Esri.WorldImagery")

cell_polygons <- lal_4 %>%
  st_transform(crs = SL_UTM) %>%
  st_make_grid(cellsize =  cell_size) %>%
  st_transform(crs = project_crs)

lal_4_4 <- lal_4 %>%
  mutate(cell_number = st_intersects(., cell_polygons, sparse = TRUE))

set.seed(123)

sample_49 <- sample(unique(as.numeric(lal_4_4$cell_number)), 49, replace = FALSE)

lal_4_4_sample <- lal_4_4 %>%
  filter(cell_number %in% sample_49) %>%
  group_by(cell_number) %>%
  slice_sample(n = 1) %>%
  select(-cell_number) %>%
  mutate(visit = 4,
         site_use = factor("Forest"),
         key = NA,
         trap_image = NA,
         trap_number = c(seq(from = 196, to = 224, by = 1), seq(from = 322, to = 341, by = 1)),
         SubmissionDate = min(lal_4_traps$SubmissionDate, na.rm = TRUE),
         date_set = min(lal_4_traps$date_set, na.rm = TRUE),
         lon = st_coordinates(geometry)[,1],
         lat = st_coordinates(geometry)[,2]) %>%
  tibble() %>%
  select(-geometry) %>%
  select(colnames(lal_4_traps))

## Lalehun grid 5 ----------------------------------------------------------

lal_5 <- trap %>%
  filter(village == "lalehun" & grid_number == 5) %>%
  distinct(lon, lat, .keep_all = TRUE) %>%
  st_as_sf(., coords = c("lon", "lat"), crs = project_crs)

mapview::mapview(lal_5, map.types = "Esri.WorldImagery")

cell_polygons <- lal_5 %>%
  st_transform(crs = SL_UTM) %>%
  st_make_grid(cellsize =  cell_size) %>%
  st_transform(crs = project_crs)

lal_5_4 <- lal_5 %>%
  mutate(cell_number = st_intersects(., cell_polygons, sparse = TRUE))

sample_49 <- sample(unique(as.numeric(lal_5_4$cell_number)), 49, replace = FALSE)

lal_5_4_sample <- lal_5_4 %>%
  filter(cell_number %in% sample_49) %>%
  group_by(cell_number) %>%
  slice_sample(n = 1) %>%
  select(-cell_number) %>%
  mutate(visit = 4,
         site_use = factor("Agriculture"),
         key = NA,
         trap_image = NA,
         trap_number = seq(from = 147, to = 195, by = 1),
         SubmissionDate = min(lal_4_traps$SubmissionDate, na.rm = TRUE),
         date_set = min(lal_4_traps$date_set, na.rm = TRUE),
         lon = st_coordinates(geometry)[,1],
         lat = st_coordinates(geometry)[,2]) %>%
  tibble() %>%
  select(-geometry) %>%
  select(colnames(lal_4_traps))


## Lalehun grid 7 ----------------------------------------------------------

lal_7 <-  trap %>%
  filter(village == "lalehun" & grid_number == 7 & !str_detect(trap_land_type, "indoor")) %>%
  distinct(lon, lat, .keep_all = TRUE) %>%
  st_as_sf(., coords = c("lon", "lat"), crs = project_crs)

mapview::mapview(lal_7, map.types = "Esri.WorldImagery")

cell_polygons <- lal_7 %>%
  st_transform(crs = SL_UTM) %>%
  st_make_grid(cellsize =  cell_size) %>%
  st_transform(crs = project_crs)

lal_7_4 <- lal_7 %>%
  mutate(cell_number = st_intersects(., cell_polygons, sparse = TRUE))

sample_49 <- sample(unique(as.numeric(lal_7_4$cell_number)), 49, replace = FALSE)

lal_7_4_sample <- lal_7_4 %>%
  filter(cell_number %in% sample_49) %>%
  group_by(cell_number) %>%
  slice_sample(n = 1) %>%
  select(-cell_number) %>%
  mutate(visit = 4,
         site_use = factor("Village"),
         key = NA,
         trap_image = NA,
         trap_number = c(seq(from = 225, to = 245, by = 1), seq(from = 294, to = 321, by = 1)),
         SubmissionDate = min(lal_4_traps$SubmissionDate, na.rm = TRUE),
         date_set = min(lal_4_traps$date_set, na.rm = TRUE),
         lon = st_coordinates(geometry)[,1],
         lat = st_coordinates(geometry)[,2]) %>%
  tibble() %>%
  select(-geometry) %>%
  select(colnames(lal_4_traps))


# Seilama visit 4 ---------------------------------------------------------

sei_4_traps <- trap %>%
  filter(village == "seilama" & visit == 4) %>%
  distinct(trap_number, .keep_all = TRUE)

## Seilama grid 5 ----------------------------------------------------------

sei_5 <- trap %>%
  filter(village == "seilama" & grid_number == 5) %>%
  distinct(lon, lat, .keep_all = TRUE) %>%
  st_as_sf(., coords = c("lon", "lat"), crs = project_crs)

mapview::mapview(sei_5, map.types = "Esri.WorldImagery")

cell_polygons <- sei_5 %>%
  st_transform(crs = SL_UTM) %>%
  st_make_grid(cellsize =  cell_size) %>%
  st_transform(crs = project_crs)

sei_5_4 <- sei_5 %>%
  mutate(cell_number = st_intersects(., cell_polygons, sparse = TRUE))

sample_49 <- sample(unique(as.numeric(sei_5_4$cell_number)), 49, replace = FALSE)

sei_5_4_sample <- sei_5_4 %>%
  filter(cell_number %in% sample_49) %>%
  group_by(cell_number) %>%
  slice_sample(n = 1) %>%
  select(-cell_number) %>%
  mutate(visit = 4,
         site_use = factor("Forest"),
         key = NA,
         trap_image = NA,
         trap_number = seq(from = 197, to = 245, by = 1),
         SubmissionDate = min(lal_4_traps$SubmissionDate, na.rm = TRUE),
         date_set = min(lal_4_traps$date_set, na.rm = TRUE),
         lon = st_coordinates(geometry)[,1],
         lat = st_coordinates(geometry)[,2]) %>%
  tibble() %>%
  select(-geometry) %>%
  select(colnames(sei_4_traps))

combined_sample <- bind_rows(lal_4_4_sample, lal_5_4_sample, lal_7_4_sample, sei_5_4_sample) %>%
  group_by_all() %>%
  expand(trap_night = 1:4) %>%
  mutate(date_set = date_set + (as.numeric(trap_night)-1),
         trap_uid = paste0(village, "_", visit, "_", trap_night, "_", grid_number, "_", trap_number))

write_csv(combined_sample, file = here("data", "raw_odk", "ODK_missing_grids.csv"))
