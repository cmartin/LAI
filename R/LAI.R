#' Calculate leaf area index (LAI) from an image in a band around 57.5 degrees.
#'
#' @param image_path Path to the image to analyze.
#' @return The calculated LAI value.
#' @examples
#' LAI_from_gf_at_57(system.file("extdata", "IMG_7595.JPG", package = "LAI"))
#' @export
LAI_from_gf_at_57 <- function(image_path) {
  LAI_from_GF(
    GF(
      unimodal_threhsold(
        crop_around_angle(
          raster::raster(image_path, band = 3) # only the blue band is necessary for calculations
        )
      )
    )
  )
}

#' Calculate gap fraction (GF) from a binary image
GF <- function(binary_img) {
  freqs <- as.data.frame(raster::freq(binary_img))
  if (nrow(freqs) < 2) {
    1
  } else {
    freqs[freqs["value"] == 0,"count"] /
      (freqs[freqs["value"] == 0,"count"] +
      freqs[freqs["value"] == 1,"count"])
  }
}

#' Calculate LAI from a gap fraction (GF) ratio
LAI_from_GF <- function(
  GF,
  angle=57.5 # degrees
) {
  rad_angle <- angle * 0.0174532925

  return(
    -(
      cos(rad_angle) / .5
      ) * log(GF)
  )
}

#' Crop a band at a particular angle from a raster image
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

  return(raster::crop(img,e))

}
