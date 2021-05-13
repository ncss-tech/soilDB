library(aqp)
library(soilDB)
library(sharpshootR)
library(sf)
library(sp)

# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=38.03552,-120.71194,z13
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=38.64382,-121.78619,z13

# init some points the sf way
p <- st_sfc(
  st_point(c(-120.71194, 38.03552), dim = 'XY'),
  st_point(c(-121.78619, 38.64382), dim = 'XY')
)
st_crs(p) <- 4326

# dang, downgrading to sp object until soilDB catches up...
p <- as(p, 'Spatial')

# query SSURGO database using these points
# results are intersecting mukeys
x <- SDA_spatialQuery(geom = p, what = 'mukey', db = 'SSURGO')

# query SDA for component records
qq <- sprintf(
  "SELECT mukey, cokey, compname, comppct_r FROM component WHERE mukey IN %s ;", 
  format_SQL_in_statement(x$mukey)
)

s <- SDA_query(qq)

# check
s

# get parsed OSDs for reference
# non-series names are silently ignored
osds <- fetchOSD(unique(s$compname))

# pretty pictures
par(mar = c(0, 0, 0, 0))

plotSPC(osds, plot.depth.axis = FALSE, name.style = 'center-center', width = 0.3, hz.depths = TRUE)

SoilTaxonomyDendrogram(osds, plot.depth.axis = FALSE, name.style = 'center-center', width = 0.3, hz.depths = TRUE, y.offset = 0.5)

# get OSD text
osd.text <- get_OSD_JSON(unique(s$compname))



