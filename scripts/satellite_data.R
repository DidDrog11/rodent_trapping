library("here")
# load spatial packages


landsat_bands_nov <- list.files(here("data", "satellite"), pattern = glob2rx("*20201106**B*.TIF$")) #lists all the files with both spectral band B and ending in TIF
landsat_bands_dec <- list.files(here("data", "satellite"), pattern = glob2rx("*20201211**B*.TIF$"))

seilama_bbox <- seilama_traps$geometry
lalehun_bbox <- st_bbox(lalehun_traps$geometry)

nov_stack <- stack(here("data", "satellite", landsat_bands_nov))
nov_brick <- brick(nov_stack)

lsat_seilama <- nov_brick %>%
  crop(., res) %>%
  mask(., res)

plot(nov_brick)
par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(nov_brick,
        r = 4, g = 3, b = 2,
        stretch = "lin",
        axes = TRUE,
        main = "RGB composite image\n Landsat Bands 4, 3, 2")
box(col = "white")