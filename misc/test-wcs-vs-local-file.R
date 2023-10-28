library(terra)
library(soilDB)

# bbox for testing
# GCS WGS84
bb <- vect(
  'POLYGON((-90.4804 38.8577,-90.4804 38.9215,-90.3517 38.9215,-90.3517 38.8577,-90.4804 38.8577))',
  crs = 'epsg:4326'
)

# work in 5070 for simplicity
bb <- project(bb, 'epsg:5070')

# WCS
mu.wcs <- mukey.wcs(bb, db = 'gssurgo')

# local, CONUS grid, same source data
mu <- crop(rast('E:/gis_data/mukey-grids/gSSURGO-mukey.tif'), bb)
mu <- as.factor(mu)

# extents do not align exactly
ext(mu.wcs)
ext(mu)

# dimensions off by 1 row
dim(mu.wcs)
dim(mu)

# resampling to the same grid, all values identical
a <- resample(mu.wcs, mu, method = 'near')
plot(mu == a, axes = FALSE)

a <- resample(mu, mu.wcs, method = 'near')
plot(mu.wcs == a, axes = FALSE)

