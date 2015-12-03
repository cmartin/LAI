library(raster)
file_path = system.file("extdata", "IMG_7595.JPG", package = "LAI")

# View the original image
plotRGB(
  brick(file_path),
  asp = 1
)

# View the binarized image
plot(
  unimodal_threshold(
    raster(file_path, band = 3)
  )
  ,useRaster = TRUE
  ,asp = 1
)
