library(aqp)
library(soilDB)
library(sf)
library(rasterVis)
library(viridisLite)



# SJQ | Amador Co.

# Amador | Pentz
# bb <- '-121.1074 38.3756,-121.1074 38.4412,-120.9702 38.4412,-120.9702 38.3756,-121.1074 38.3756'

# Mokelumne | Sed. Rock Land
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=38.46643,-121.02565,z14
bb <- '-121.1099 38.4288,-121.1099 38.4942,-120.9728 38.4942,-120.9728 38.4288,-121.1099 38.4288'
wkt <- sprintf('POLYGON((%s))', bb)

x <- st_as_sfc(wkt)
st_crs(x) <- 4326

(mu <- mukey.wcs(aoi = x, db = 'gnatsgo'))

levelplot(mu, att = 'ID', margin = FALSE, colorkey = FALSE, col.regions = viridis)

# get unique mukeys from grid
ll <- levels(mu)[[1]]

s <- get_SDA_property(property = 'pH 1:1 water - Rep Value', method = 'Weighted Average', mukeys = ll$ID, top_depth = 0, bottom_depth = 25)

## !! what? 0s?
s[which(s$ph1to1h2o_r == 0), ]
z <- fetchSDA("mukey = '461915'")
z$ph1to1h2o_r

### TODO: figure this one out
## dilution of weighted mean due to weights associated with NULL values
## document this
# another issue: weighted averages 
z <- fetchSDA("mukey = '461984'")
z$ph1to1h2o_r


# merge pH by mukey into raster attribute table (RAT)
rat <- merge(ll, s[, c('mukey', 'ph1to1h2o_r')], by.x = 'ID', by.y = 'mukey', sort = FALSE, all.x = TRUE)

# re-pack RAT
levels(mu) <- rat

ph_025 <- deratify(mu)
levelplot(ph_025, margin = FALSE, col.regions = viridis, scales = list(draw = FALSE))

# ph_025.800 <- ISSR800.wcs(aoi = x, var = 'ph_025cm')
# levelplot(ph_025.800, margin = FALSE, col.regions = viridis, scales = list(draw = FALSE))


# west side of SJV
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=35.80752,-119.55113,z12

bb <- '-119.6811 36.2854,-119.6811 36.4108,-119.3977 36.4108,-119.3977 36.2854,-119.6811 36.2854'
wkt <- sprintf('POLYGON((%s))', bb)

x <- st_as_sfc(wkt)
st_crs(x) <- 4326

(mu <- mukey.wcs(aoi = x, db = 'gnatsgo'))

levelplot(mu, att = 'ID', margin = FALSE, colorkey = FALSE, col.regions = viridis)

# get unique mukeys from grid
ll <- levels(mu)[[1]]

s <- get_SDA_property(property = 'Total Clay - Rep Value', method = 'Weighted Average', mukeys = ll$ID, top_depth = 0, bottom_depth = 25)

# merge AWS by mukey into raster attribute table (RAT)
rat <- merge(ll, s[, c('mukey', 'claytotal_r')], by.x = 'ID', by.y = 'mukey', sort = FALSE, all.x = TRUE)

# re-pack RAT
levels(mu) <- rat

clay_025 <- deratify(mu)
levelplot(clay_025, margin = FALSE, col.regions = viridis, scales = list(draw = FALSE))

clay_025.800 <- ISSR800.wcs(aoi = x, var = 'clay_025cm')
levelplot(clay_025.800, margin = FALSE, col.regions = viridis, scales = list(draw = FALSE))

# 
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=41.00507,-103.97018,z14

#
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=44.19580,-97.19309,z17

#
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=30.28822,-100.37524,z15

# WV
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=37.62841,-82.21441,z15

x <- st_sfc(st_point(c(-97.19309, 44.19580)))
st_crs(x) <- 4326

x <- as(x, 'Spatial')
mu <- SDA_spatialQuery(x)
IS <- format_SQL_in_statement(mu$mukey)

s <- fetchSDA(WHERE = sprintf("mukey IN %s", IS), duplicates = TRUE)
plotSPC(s, label = 'compname', color = 'caco3_r')


bb <- '-97.2020 44.1922,-97.2020 44.1992,-97.1843 44.1992,-97.1843 44.1922,-97.2020 44.1922'
wkt <- sprintf('POLYGON((%s))', bb)

x <- st_as_sfc(wkt)
st_crs(x) <- 4326
plot(x)

(mu <- mukey.wcs(aoi = x, db = 'gnatsgo'))

levelplot(mu, att = 'ID', margin = FALSE, colorkey = FALSE, col.regions = viridis)

