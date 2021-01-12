library(soilDB)
library(sp)
library(rgdal)
library(raster)
library(rasterVis)
library(viridis)

# AOI corners in WGS84 GCS
# xmin, ymin, xmax, ymax
a <- list(
  aoi = c(-114.16, 47.65, -114.08, 47.68),
  crs = '+init=epsg:4326'
)

### The following are valid (and equivalent to mukey.wcs()) {sf} object based AOIs

# # bbox, with WKT CRS
# a1 <- sf::st_bbox(c(xmin = -114.16, xmax = -114.08, ymin = 47.65, ymax = 47.68), crs = sf::st_crs(4326))

# # sfc_POLYGON
# a2 <- sf::st_as_sfc(a1)

# # sf
# a3 <- sf::st_as_sf(a2)

### And this is a {sp} SpatialPolygons AOI (created from the sf object)
# a4 <- sf::as_Spatial(a3)

# too big for SDA geometry (>32Mb WKT serialization)
# but will work just fine with WCS

# a <- list(
#   aoi = c(-114.5, 47, -114, 47.5),
#   crs = '+init=epsg:4326'
# )

# fetch gNATSGO map unit keys at native resolution (30m)
# get gSSURGO grid with db = 'gssurgo'
(x <- mukey.wcs(aoi = a, db = 'gnatsgo'))

# OK
levelplot(x, att = 'ID', margin = FALSE, colorkey = FALSE, col.regions = viridis)

# convert raster extent into vector
g <- as(extent(x), 'SpatialPolygons')
proj4string(g) <- projection(x)

# get intersecting SSURGO linework as SpatialPolygonsDataFrame from SDA
p <- SDA_spatialQuery(g, what = 'geom', geomIntersection = TRUE)

# transform to AEA CRS
p <- spTransform(p, CRS(projection(x)))

# plot together
levelplot(
  x, att = 'ID', margin = FALSE, colorkey = FALSE,
  col.regions = viridis,
  panel = function(...) {
    panel.levelplot(...)
    sp.lines(p, col = 'white')
  }
)


# thematic mapping via muaggatt table
# available water storage to several depths

# get unique mukeys fromgrid
ll <- levels(x)[[1]]
# convert into an SQL "IN" statement
IS <- format_SQL_in_statement(ll$ID)

# query SDA by mukey
sql <- sprintf("SELECT mukey, aws025wta, aws050wta, aws0100wta, aws0150wta FROM muaggatt WHERE mukey IN %s", IS)
tab <- SDA_query(sql)

# merge AWS by mukey into raster attribute table (RAT)
rat <- merge(ll, tab, by.x = 'ID', by.y = 'mukey', sort = FALSE, all.x = TRUE)

# re-pack RAT
levels(x) <- rat

# check: note RAT
x

# raster + RAT --> grid of values
# when more than a single attribute, result is a raster stack
# note this is a raster stack with 4 layers
(aws <- deratify(x))

# graphical check
levelplot(
  aws,
  main = 'gNATSGO WCS + SDA',
  margin = FALSE,
  scales = list(draw = FALSE),
  col.regions = viridis,
  panel = function(...) {
    panel.levelplot(...)
    sp.lines(p, col = 'white')
  }
  )



## next steps
# robust / complete / fast approach for getting / aggregating / linking via SDA




