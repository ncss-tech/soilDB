library(aqp)
library(soilDB)
library(terra)
library(spData)
library(sf)

data("us_states")
us_states <- st_transform(us_states, 5070)

# convert to SpatVect
us_states <- vect(us_states)

# tight bounding box around CONUS
b <- ext(us_states)

s <- OSDquery(everything = 'graphit:*')
x <- fetchOSD(s$series)

par(mar = c(0, 0, 2, 3))
plotSPC(x, name.style = 'center-center', width = 0.35)
title('OSDs with "graphit" in the narrative')

e <- lapply(s$series, seriesExtent, type = 'raster')


plot(e[[1]])

# remove NULL
e <- e[which(!sapply(e, is.null))]

# combine into SpatRasterCollection
ee <- sprc(e)

# combine into single SpatRaster
ee <- mosaic(ee, fun = 'sum')

# values > 100 are rounding errors
ee[ee > 100] <- 100

# give merged raster a name
names(ee) <- 'Graphite Mentioned in OSD'

# aggregate to 5x larger grid, sum of cell percent cover
a <- aggregate(ee, fact = 5, fun = sum, na.rm = TRUE)

# rescale percent cover to larger grid size
a <- a / 5^2

# CONUS map
plot(a, axes = FALSE, col = 'royalblue', ext = b, mar = c(1, 1, 3, 4), main = names(a), legend = FALSE)
lines(us_states, col = 'grey')
