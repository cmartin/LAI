unimodal.threhsold.img = function(path.to.file, crop=TRUE) {
  # read only the blue band
  if (crop) {
    img = crop.around.angle(raster::raster(path.to.file, band=3))
  } else {
    img = raster(path.to.file, band=3)
  }
  values = raster::getValues(img)
  
  DN.list = 0:255
  DN.freq = lapply(DN.list,function(x) {
    sum(values==x,na.rm=TRUE)
  })
  
  hst = data.frame (
    DN = DN.list,
    freq = unlist(DN.freq)
  )
  
  # Flatten spurious values
  hst[hst$freq<5,"freq"]=0
  
  # Find left maximum
  DN.L1 = 5
  DN.L2 = 55
  repeat {
    range.L = (DN.L1+1):(DN.L2-1)
    DN.max.L = range.L [which.max(hst[hst$DN %in% range.L,"freq"])]
    
    # Is the criteria met?
    if (DN.L2 - DN.max.L >= 10) break;
    DN.L2 = DN.L2 + 25
  }
  
  # Find right maximum
  DN.R1 = 205
  DN.R2 = 255 # MacFarlane suggests to use 200-250, but photo IMG_7595.JPG has a peak at 255...
  repeat {
    range.R = (DN.R1+1):(DN.R2-1)  
    DN.max.R = range.R [which.max(hst[hst$DN %in% range.R,"freq"])]
    
    if (DN.max.R - DN.R1 >= 10) break;
    DN.R1 = DN.R1-25
  }
  if (DN.max.R < DN.max.L) {
    DN.max.R = 255
  }
  
  # Find first empty value after the peak
  first.empty = 
    suppressWarnings(
      min(hst[hst$freq==0 & hst$DN > DN.max.L,"DN"])
    )
  if (is.infinite(first.empty)) first.empty = 256
  
  # Calculate slope between left maximum and first empty bin
  freq.max = hst[hst$DN == DN.max.L, "freq"]
  
  # MacFarlane's change to Rosin's technique : use mean freq instead of max to reduce slope
  freq.max = min(mean(hst$freq), freq.max)
  
  m = (0 - freq.max) / (first.empty - DN.max.L)
  b = 0 - m * first.empty
  #hist(values, breaks=255)
  #abline(b,m)
  
  # Find point farthest from the slope
  # http://math.ucsd.edu/~wgarner/math4c/derivations/distance/distptline.htm
  # dist = abs(y1-m*x1-b) / sqrt(m^2+1)
  to_test = DN.max.L:DN.max.R # Range of values to test
  
  # Left corner (i.e. inflection point)
  DN.lc = to_test[which.min(
    (hst[which(hst$DN %in% to_test),"freq"]-m*to_test-b) / sqrt(m^2+1)
  )]
  
  if (DN.max.R == DN.max.L) {
    threshold = DN.lc
  } else {
    
    # repeat previous steps to find the right corner
    first.empty = 
      suppressWarnings(
        max(hst[hst$freq==0 & hst$DN < DN.max.R,"DN"])
      )
    if (is.infinite(first.empty)) first.empty =0
    freq.max = hst[hst$DN == DN.max.R, "freq"]
    
    freq.max = min(mean(hst$freq), freq.max)
    
    m = (0 - freq.max) / (first.empty - DN.max.R)
    b = 0 - m * first.empty
    #abline(b,m)
    
    to_test = DN.max.L:DN.max.R
    DN.uc = to_test[which.min(
      (hst[which(hst$DN %in% to_test),"freq"]-m*to_test-b) / sqrt(m^2+1)
    )]
    
    threshold = DN.lc + (DN.uc-DN.lc)/2
  }	
  rec.matrix = matrix(c(0,threshold,1,threshold,255,0),nrow=2,ncol=3,byrow=TRUE)
  return( raster::reclassify(img, rec.matrix))
  
}