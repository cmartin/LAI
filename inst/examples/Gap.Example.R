library(raster)
gap_fraction(
 unimodal_threshold(
   raster(system.file("extdata", "IMG_0005.JPG", package = "LAI"), band = 3)
 )
)
