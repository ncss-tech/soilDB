library(terra)
library(sf)
library(soilDB)

## WCS demo (raster-based, needs an update)
# http://ncss-tech.github.io/AQP/soilDB/WCS-demonstration-01.html

## get the latest gNATSGO / gSSURGO grids here:
## these are CONUS (AEA) grids at 30m
# https://nrcs.app.box.com/v/soils/folder/149859400396


# mukey grid
mu <- rast('E:/gis_data/mukey-grids/gNATSGO-mukey.tif')

## MT AOI from Jay
# make a bounding box and assign a CRS (4326: GCS, WGS84)
a <- st_bbox(
  c(xmin = -113.65, xmax = -112.51, ymin = 46.69, ymax = 47.18),
  crs = st_crs(4326)
)

# convert to polygons -> transform to CRS of mukey grid
a <- st_as_sfc(a)
a <- st_transform(a, 5070)

# crop CONUS grid
x <- crop(mu, a)

## TODO: double-check this
# ratify
uids <- terra::unique(x)[,1]
rat <- data.frame(value = uids, mukey = uids)
x <- terra::categories(x, layer = 1, rat)

# set layer name in object
names(x) <- 'mukey'

# ok
plot(x)

# extract RAT for thematic mapping
rat <- cats(x)[[1]]

vars <- c("sandtotal_r", "silttotal_r", "claytotal_r", "om_r", "cec7_r", "ph1to1h2o_r")

# weighted mean over components to account for large misc. areas
# depth-weighted average 0-25cm
p <-  get_SDA_property(
  property = vars,
  method = "Weighted Average", 
  mukeys = as.integer(rat$mukey),
  top_depth = 0,
  bottom_depth = 25,
  include_minors = TRUE, 
  miscellaneous_areas = FALSE
)

# merge aggregate data into RAT
rat <- merge(rat, p, by.x = 'mukey', by.y = 'mukey', sort = FALSE, all.x = TRUE)
levels(x) <- rat

# grid + RAT -> stack of numerical grids
system.time(x.stack <- catalyze(x))

# keep only properties / remove IDs
x.stack <- x.stack[[vars]]

# check
plot(x.stack, maxcell = 1e4, col = viridis::viridis(50))




