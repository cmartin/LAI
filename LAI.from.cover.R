as.data.frame.rle <- function(x, ...) do.call(data.frame, x)

LAI.from.cover = function(
	image.path, 
	k = 0.5, # e.g. Macfarlane et al. 2007
	iterations = 300) {
	
	#### Step 1 : Get image mask	
	img = unimodal.threhsold.img(image.path, crop=FALSE)
		
	surface1.3 = img@ncols*img@nrows * 0.013
	longueur1.3 = sqrt(surface1.3/pi) * 2 # s = pi * r^2
	total.black = 0
	total.white = 0
	total.gray = 0
	for (i in 1:iterations) {
		
		####### Step 2 : sample random transects from canopy
		if (sample(c(0,1),1) == 0) {
			# Vertical
			v = getValuesBlock(img, 
							   col=sample(c(img@extent@xmin:img@extent@xmax),1),
							   row=1,
							   nrows = img@extent@ymax - img@extent@ymin,
							   ncols=1
			)
		} else {
			# Horizontal
			v = getValuesBlock(img, 
							   row=sample(c(img@extent@ymin:img@extent@ymax),1),
							   col=1,
							   nrows = 1,
							   ncols=img@extent@xmax - img@extent@xmin
			)
		}
		
		##### Step 3 : Analyse sequence lengths to categorize gaps
		s = rle(unlist(v))
		s = data.frame(s)
		w = s[s$value==0,]
		b = s[s$value==1,]
		
		black = sum(b$lengths) # vegetation
		white = sum(w[w$lengths<longueur1.3,"lengths"]) # gaps inside crowns
		gray = sum(w[w$lengths>longueur1.3,"lengths"]) # gaps between crowns
		
		total.black = total.black+black
		total.white = total.white+white
		total.gray = total.gray+gray
		
		
		
	}

	########## Step 4 : Calculate metrics on totals
	f.f = total.black / (total.black+total.white+total.gray) # foliage cover
	f.c = (total.black+total.white) / (total.black+total.white+total.gray) # crown cover
	phi = 1 - f.f / f.c # crown porosity
	lai = -f.c * log (phi) / k
	omega.0 = ((1-phi) * log(1-f.f)) / (log(phi) * f.f) # zenithal clumping index
	
	return (list(
		crown.porosity=phi,
		lai = lai,
		clumping.index = omega.0,
		effective.lai = lai*omega.0
	))
	
	
}

# LAI.from.cover("images/canopy/IMG_0048.JPG", iterations=300)
# LAI.from.cover("images/canopy/IMG_0100.JPG", iterations=300)
# LAI.from.cover("images/canopy/IMG_0009.JPG", iterations=300)
# LAI.from.cover("images/canopy/IMG_7585.JPG", iterations=300)
