#' Calculate leaf area index (LAI) from an image in a band around 57.5 degrees.
#'
#' This functions first seperates sky from vegetation pixels using the
#' histogram-shape method described in Macfarlane 2011. Based on the
#' focal angle and camera field of view provided, it then extracts a narrow
#' band around a 57.5 degrees zenith angle (Baret et al. 2010). The gap fraction of this band is then used to
#' calculated an indirect LAI value (e.g. Confalonieri et al. 2013).
#'
#' @param image_path Path to the image to analyze.
#' @param camera_horiz_FOV Camera horizontal field of view (in degrees)
#' @param focal_angle Angle at which the camera was pointing (degrees). 0 is
#' horizontal, 90 is vertical
#' @return The calculated LAI value.
#' @examples
#' LAI_from_gf_at_57(
#'   system.file("extdata", "IMG_7595.JPG", package = "LAI")
#' )
#' @references
#'   Baret, F., de Solan, B., Lopez-Lozano, R., Ma, K., & Weiss, M. (2010).
#' GAI estimates of row crops from downward looking digital photos taken
#' perpendicular to rows at 57.5° zenith angle: Theoretical considerations
#' based on 3D architecture models and application to wheat crops.
#' Agricultural and Forest Meteorology, 150(11), 1393–1401.
#'
#'   Confalonieri, R., Foi, M., Casa, R., Aquaro, S., Tona, E., Peterle,
#' M., … Acutis, M. (2013). Development of an app for estimating leaf area
#' index using a smartphone. Trueness and precision determination and
#' comparison with other indirect methods. Computers and Electronics in
#' Agriculture, 96, 67–74.
#'
#'   Macfarlane, C. (2011). Classification method of mixed pixels does not affect
#' canopy metrics from digital images of forest overstorey. Agricultural and
#' Forest Meteorology, 151(7), 833–840.
#' @export
LAI_from_gf_at_57 <- function(image_path,
                              camera_horiz_FOV = 73.7,
                              focal_angle = 45
                              ) {
  LAI_from_GF(
    GF(
      unimodal_threhsold(
        crop_around_angle(
          # only the blue band is necessary for calculations
          raster::raster(image_path, band = 3),
          camera_horiz_FOV = camera_horiz_FOV,
          focal_angle = focal_angle
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
  camera_horiz_FOV, # degrees
  focal_angle, # degrees angle at which the camera was pointing (degrees)

  # crop box
  crop_top_angle = (90 - 57.5) + 5, # our angles are calculated from the ground
  crop_bottom_angle = (90 - 57.5) - 5
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
