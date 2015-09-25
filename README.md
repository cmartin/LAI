# LAI package for R

This package calculates leaf area index (LAI) from digital images.

The LAI value is computed from the gap fraction of a binarized vegetation image classified with the histogram-based unimdal threshold method.

## To install : 
```{r}
library(devtools)
devtools::install_github("cmartin/LAI")
```

## To try the code with one of the example images : 
```{r}
library(LAI)
library(raster)
test_image <-  system.file("extdata", "IMG_7595.JPG", package = "LAI")

# See the image
plotRGB(
  brick(test_image),
  asp=1
)

# Calculate LAI
LAI_from_gf_at_57(test_image)
```

Please report any bugs to the [GitHub issue tracker](https://github.com/cmartin/LAI/issues) and write any questions to <charles.martin1@uqtr.ca>
