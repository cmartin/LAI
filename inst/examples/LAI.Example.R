## @knitr LAIExample

library(raster)

test_image <-  system.file("extdata", "IMG_7595.JPG", package = "LAI")

# See the image
plotRGB(
  brick(test_image),
  asp = 1
)

# Calculate LAI
LAI_from_gf_at_57(test_image)
