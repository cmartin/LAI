# Estimate LAI from gap fraction at 57.5 degrees

# Example usage :
# LAI.from.GF.at.angle(calculate.GF(unimodal.threhsold.img("images/IMG_0005.JPG")))
calculate.GF = function(binary.img) { 	
  freqs = as.data.frame(raster::freq(binary.img))
  if (nrow(freqs)<2) {
    1
  } else {
    freqs[freqs["value"]==0,"count"] / (freqs[freqs["value"]==0,"count"] + freqs[freqs["value"]==1,"count"])
  }
}

LAI.from.GF.at.angle = function(
  GF, 
  angle=57.5 # degrees
) {
  rad.angle = angle * 0.0174532925
  
  return( - (
    cos(rad.angle) / .5
  ) * log (GF)	)
}

crop.around.angle = function(
  img, # raster object
  camera.horiz.FOV = 73.7, # degrees
  # angle at which the camera was pointing
  focal.angle = 45, # degrees
  # crop box 
  crop.top.angle = 57.5+5,
  crop.bottom.angle = 57.5-5
) {
  
  
  # pixel/degree ratio
  pixel.degree.ratio = img@nrows / camera.horiz.FOV
  
  # at what angle is the picture bottom pointing to
  bottom.angle = focal.angle - camera.horiz.FOV/2
  
  crop.top.pixel = (crop.top.angle - bottom.angle) * pixel.degree.ratio
  crop.bottom.pixel =(crop.bottom.angle - bottom.angle) * pixel.degree.ratio
  e = raster::extent(c(0,img@ncols,crop.bottom.pixel,crop.top.pixel) )
  
  return (raster::crop(img,e))
  
}