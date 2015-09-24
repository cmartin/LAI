library(LAI)
context("Full LAI computations")

expect_that(
  LAI_from_gf_at_57(
    system.file("extdata", "IMG_0005.JPG", package = "LAI")
  ),
  equals(0.6, tolerance = .1)
)

expect_that(
  LAI_from_gf_at_57(
    system.file("extdata", "IMG_0307.JPG", package = "LAI")
  ),
equals(0)
)

expect_that(
  LAI_from_gf_at_57(
    system.file("extdata", "IMG_9362.JPG", package = "LAI")
  ),
  equals(2.8, tolerance = .1)
)

expect_that(
  LAI_from_gf_at_57(
    system.file("extdata", "IMG_9783.JPG", package = "LAI")
  ),
  equals(4.4, tolerance = .1)
)

expect_that(
  LAI_from_gf_at_57(
    system.file("extdata", "IMG_7595.JPG", package = "LAI")
  ),
  equals(2.6, tolerance = .1)
)
