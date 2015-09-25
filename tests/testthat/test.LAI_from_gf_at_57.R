library(LAI)
context("Full LAI computations")

expect_that(
  LAI_from_gf_at_57(
    system.file("extdata", "IMG_0005.JPG", package = "LAI")
  ),
  equals(0.33, tolerance = .1)
)

expect_that(
  LAI_from_gf_at_57(
    system.file("extdata", "IMG_7818.JPG", package = "LAI")
  ),
  equals(1.8, tolerance = .1)
)

expect_that(
  LAI_from_gf_at_57(
    system.file("extdata", "IMG_9362.JPG", package = "LAI")
  ),
  equals(2.7, tolerance = .1)
)

expect_that(
  LAI_from_gf_at_57(
    system.file("extdata", "IMG_7595.JPG", package = "LAI")
  ),
  equals(2.5, tolerance = .1)
)

expect_that(
  LAI_from_gf_at_57(
    system.file("extdata", "IMG_0044.JPG", package = "LAI")
  ),
  equals(2.8, tolerance = .1)
)
