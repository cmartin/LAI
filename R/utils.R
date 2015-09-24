unimodal_threhsold_img <- function(path_to_file, crop = TRUE) {
  # read only the blue band
  if (crop) {
    img <- crop_around_angle(raster::raster(path_to_file, band = 3))
  } else {
    img <- raster::raster(path_to_file, band=3)
  }
  values <- raster::getValues(img)

  DN_list <- 0:255
  DN_freq <- lapply(DN_list, function(x) {
    sum(values == x, na.rm=TRUE)
  })

  hst <- data.frame (
    DN = DN_list,
    freq = unlist(DN_freq)
  )

  # Flatten spurious values
  hst[hst$freq < 5, "freq"] <- 0

  # Find left maximum
  DN_L1 <- 5
  DN_L2 <- 55
  repeat {
    range_L <- (DN_L1 + 1):(DN_L2 - 1)
    DN_max_L <- range_L [which.max(hst[hst$DN %in% range_L, "freq"])]

    # Is the criteria met?
    if (DN_L2 - DN_max_L >= 10) break;
    DN_L2 <- DN_L2 + 25
  }

  # Find right maximum
  DN_R1 <- 205

  # MacFarlane suggests to use 200-250,
  # but photo IMG_7595.JPG has a peak at 255...
  DN_R2 <- 255
  repeat {
    range_R <- (DN_R1 + 1):(DN_R2 - 1)
    DN_max_R <- range_R [which.max(hst[hst$DN %in% range_R, "freq"])]

    if (DN_max_R - DN_R1 >= 10) break;
    DN_R1 <- DN_R1 - 25
  }
  if (DN_max_R < DN_max_L) {
    DN_max_R <- 255
  }

  # Find first empty value after the peak
  first_empty <-
    suppressWarnings(
      min(hst[hst$freq == 0 & hst$DN > DN_max_L, "DN"])
    )
  if (is.infinite(first_empty)) first_empty <- 256

  # Calculate slope between left maximum and first empty bin
  freq_max <- hst[hst$DN == DN_max_L, "freq"]

  # MacFarlane's change to Rosin's technique :
  # use mean freq instead of max to reduce slope
  freq_max <- min(mean(hst$freq), freq_max)

  m <- (0 - freq_max) / (first_empty - DN_max_L)
  b <- 0 - m * first_empty
  #' hist(values, breaks=255)
  #' abline(b,m)
  # Find point farthest from the slope
  # http://math.ucsd.edu/~wgarner/math4c/derivations/distance/distptline.htm
  #' dist = abs(y1-m*x1-b) / sqrt(m^2+1)
  to_test <- DN_max_L:DN_max_R # Range of values to test

  # Left corner (i.e. inflection point)
  DN_lc <- to_test[which.min(
    (hst[which(hst$DN %in% to_test), "freq"] - m * to_test - b) /
      sqrt(m ^ 2 + 1)
  )]

  if (DN_max_R == DN_max_L) {
    threshold <- DN_lc
  } else {

    # repeat previous steps to find the right corner
    first_empty <-
      suppressWarnings(
        max(hst[hst$freq == 0 & hst$DN < DN_max_R, "DN"])
      )
    if (is.infinite(first_empty)) first_empty <- 0
    freq_max <- hst[hst$DN == DN_max_R, "freq"]

    freq_max <- min(mean(hst$freq), freq_max)

    m <- (0 - freq_max) / (first_empty - DN_max_R)
    b <- 0 - m * first_empty
    #' abline(b,m)

    to_test <- DN_max_L:DN_max_R
    DN_uc <- to_test[which.min(
      (hst[which(hst$DN %in% to_test), "freq"] - m * to_test - b)
      / sqrt(m ^ 2 + 1)
    )]

    threshold <- DN_lc + (DN_uc - DN_lc) / 2
  }
  rec_matrix <- matrix(
    c(0, threshold, 1, threshold, 255, 0),
    nrow = 2, ncol = 3, byrow = TRUE
  )
  return(raster::reclassify(img, rec_matrix))

}
