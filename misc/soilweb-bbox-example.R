library(soilDB)
library(raster)
library(rasterVis)
library(viridisLite)
library(sf)


bb <- '-121.8655 38.8834,-121.8655 39.0117,-121.5878 39.0117,-121.5878 38.8834,-121.8655 38.8834'
wkt <- sprintf('POLYGON((%s))', bb)

x <- st_as_sfc(wkt)
st_crs(x) <- 4326
plot(x)


# mapview::mapview(x)

(mu <- mukey.wcs(aoi = x, db = 'gnatsgo'))

levelplot(mu, att = 'ID', margin = FALSE, colorkey = FALSE, col.regions = viridis)
