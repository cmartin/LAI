#' Estimate LAI from gap fraction at 57.5 degrees
#' Example usage :
#' LAI.from.GF.at.angle(calculate.GF(unimodal.threhsold.img("images/IMG_0005.JPG")))
#' 

calculate_GF <- function(binary_img) {
  freqs <- as.data.frame(raster::freq(binary_img))
  if (nrow(freqs) < 2) {
    1
  } else {
    freqs[freqs["value"] == 0,"count"] /
      (freqs[freqs["value"] == 0,"count"] +
      freqs[freqs["value"] == 1,"count"])
  }
}

LAI_from_GF_at_angle <- function(
  GF,
  angle=57.5 # degrees
) {
  rad_angle <- angle * 0.0174532925

  return(
    - (
      cos(rad_angle) / .5
      ) * log (GF)
  )
}

crop_around_angle <- function(
  img, # raster object
  camera_horiz_FOV = 73.7, # degrees
  # angle at which the camera was pointing
  focal_angle = 45, # degrees
  # crop box
  crop_top_angle = 57.5 + 5,
  crop_bottom_angle = 57.5 - 5
) {

  # pixel/degree ratio
  pixel_degree_ratio <- img@nrows / camera_horiz_FOV

  # at what angle is the picture bottom pointing to
  bottom_angle <- focal_angle - camera_horiz_FOV / 2

  crop_top_pixel <- (crop_top_angle - bottom_angle) * pixel_degree_ratio
  crop_bottom_pixel <- (crop_bottom_angle - bottom_angle) * pixel_degree_ratio
  e <- raster::extent(c(0,img@ncols,crop_bottom_pixel,crop_top_pixel) )

  return (raster::crop(img,e))

}
