library(soilDB)
library(raster)
library(rasterVis)
library(viridisLite)
library(sf)


bb <- '-91.6853 36.4617,-91.6853 36.5281,-91.5475 36.5281,-91.5475 36.4617,-91.6853 36.4617'
wkt <- sprintf('POLYGON((%s))', bb)

x <- st_as_sfc(wkt)
st_crs(x) <- 4326

# double-check BBOX: ok
mapview::mapview(x)


# get gSSURGO grid here
mu <- mukey.wcs(aoi = x, db = 'gssurgo')

# unique map unit keys
ll <- levels(mu)[[1]]

# note SSA boundary
levelplot(mu, att = 'ID', margin = FALSE, colorkey = FALSE, col.regions = viridis)

# get thematic data from SDA
# dominant component
# depth-weighted average
# sand, silt, clay (RV)
p <-  get_SDA_property(property = c("sandtotal_r","silttotal_r","claytotal_r"),
                       method = "Dominant Component (Numeric)", 
                       mukeys = ll$ID,
                       top_depth = 25,
                       bottom_depth = 50)

head(p)

# re-create raster attribute table with aggregate soil properties
rat <- merge(ll, p, by.x = 'ID', by.y = 'mukey', sort = FALSE, all.x = TRUE)

# re-pack RAT
levels(mu) <- rat

# convert raster + RAT --> stack of values
ssc <- deratify(mu, c("sandtotal_r","silttotal_r","claytotal_r"))

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




