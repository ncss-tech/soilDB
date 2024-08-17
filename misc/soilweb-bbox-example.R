# need latest soilDB from GH
library(soilDB)
library(terra)
library(rasterVis)
library(viridisLite)
library(sf)
library(aqp)

# https://twitter.com/MoreorLoess/status/1471935030746304521
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=41.83547,-90.12201,z16

## copy / paste from SoilWeb
## 'b' keypress
bb <- '-90.1378 41.8273,-90.1378 41.8420,-90.1051 41.8420,-90.1051 41.8273,-90.1378 41.8273'


## Northern CA
bb <- '-122.5164 41.6966,-122.5164 41.7267,-122.4540 41.7267,-122.4540 41.6966,-122.5164 41.6966'


# near Ithica, NY
# bb <- '-76.6811 42.3178,-76.6811 42.3526,-76.5987 42.3526,-76.5987 42.3178,-76.6811 42.3178'


# convert to WKT -> sf
wkt <- sprintf('POLYGON((%s))', bb)

x <- st_as_sfc(wkt)
st_crs(x) <- 4326

## double-check BBOX: ok
# mapview::mapview(x)


# get gSSURGO grid here
# result is a terra SpatRaster
mu <- mukey.wcs(aoi = x, db = 'gssurgo')

# extract mukeys / RAT for thematic mapping
rat <- cats(mu)[[1]]

# note SSA boundary
levelplot(mu, att = 'mukey', margin = FALSE, colorkey = FALSE, col.regions = viridis)

# get thematic data from SDA
# weighted mean
# 
# depth-weighted average
# sand, silt, clay (RV)
p <-  get_SDA_property(property = c("sandtotal_r","silttotal_r","claytotal_r"),
                       method = "Weighted Average", 
                       miscellaneous_areas = FALSE, 
                       include_minors = TRUE,
                       mukeys = as.integer(rat$mukey),
                       top_depth = 25,
                       bottom_depth = 50)

head(p)

# re-create raster attribute table with aggregate soil properties
rat <- merge(rat, p, by.x = 'mukey', by.y = 'mukey', sort = FALSE, all.x = TRUE)

# 
rat$mukey <- as.integer(rat$mukey)

# re-pack RAT
levels(mu) <- rat

# convert raster + RAT --> stack of values
# note this includes extra, ID rasters
ssc <- catalyze(mu)

# extract specific soil properties
ssc <- ssc[[c("sandtotal_r","silttotal_r","claytotal_r")]]


# graphical check
# note implicit simplification via maxpixels
levelplot(
  ssc, 
  main = 'Sand, Silt, Clay (RV) 25-50cm\nDominant Component',
  margin = FALSE, 
  scales = list(draw = FALSE), 
  col.regions = viridis,
  maxpixels = 1e4
)

## soil texture class of fine earth fraction

# copy grid, will replace cell values
ssc.class <- ssc[[1]]

# classify sand, clay fractions
# retain all possible texture classes
values(ssc.class) <- ssc_to_texcl(
  sand = values(ssc[['sandtotal_r']]), 
  clay = values(ssc[['claytotal_r']]), 
  droplevels = FALSE
)

# name for RAT
names(ssc.class) <- 'soil.texture'

# check
plot(ssc.class)

# note that all possible texture classes are included in the RAT
cats(ssc.class)[[1]]

