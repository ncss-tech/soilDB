library(soilDB)
library(sp)
library(rgdal)
library(raster)
library(rasterVis)
library(viridis)


# AOI corners in WGS84 GCS
# xmin, ymin, xmax, ymax
a <- c(-114.16, 47.65, -114.08, 47.68)

# too big for SDA geometry (>32Mb WKT serialization)
# a <- c(-114.5, 47, -114, 47.5)

# fetch gNATSGO map unit keys at native resolution
x <- gn.30m <- mukey.wcs(var = 'gnatsgo', aoi = a)

# OK
levelplot(x, att = 'ID', margin = FALSE, colorkey = FALSE, col.regions = viridis)

# convert raster extent into vector 
g <- as(extent(x), 'SpatialPolygons')
proj4string(g) <- projection(x)

# get intersecting SSURGO linework as SpatialPolygonsDataFrame from SDA
p <- SDA_spatialQuery(g, what = 'geom', geomIntersection = TRUE)

# transform to AEA CRS
p <- spTransform(p, CRS(projection(x)))


levelplot(
  x, att = 'ID', margin = FALSE, colorkey = FALSE,
  col.regions = viridis,
  panel = function(...) {
    panel.levelplot(...)
    sp.lines(p, col = 'white')
  }
)
