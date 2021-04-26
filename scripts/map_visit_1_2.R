source(here::here("scripts", "project_library.R"))

trap_sites <- read_csv(here("data", "trap_sites.csv"))

pal <- colorFactor(palette = c("#f1a340", "#998ec3"), domain = trap_sites$visit)

distinct_traps <- trap_sites %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F) %>%
  distinct(geometry, .keep_all = T)

leaflet(distinct_traps) %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addCircleMarkers(color = ~pal(visit),
                   radius = 3,
                   stroke = F,
                   fillOpacity = 1,
                   popup = paste(distinct_traps$trap_uid),
                   popupOptions = popupOptions(closeOnClick = T))  %>%
  addLegend("topright", pal = pal, values = ~visit,
            title = "Visit")
