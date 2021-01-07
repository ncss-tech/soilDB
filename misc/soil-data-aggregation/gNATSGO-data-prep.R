library(aqp)
library(soilDB)
library(reshape2)

library(sp)
library(raster)
library(rasterVis)
library(viridis)

source('local-functions.R')

# AOI corners in WGS84 GCS
# xmin, ymin, xmax, ymax

# tiny BBOX in MT
a <- c(-114.16, 47.65, -114.08, 47.68)

## CA example
# a <- c(-121, 37, -120, 38)
# 
## MO/AR border
# a <- c(-91, 36, -90, 37)
# 
## NC
# a <- c(-76, 35, -75, 36)

## central NY
a <- c(-77, 42, -76, 43)

# simplest AOI specification for mukey.wcs
aoi <- list(
  aoi = a,
  crs = '+init=EPSG:4326'
)


# fetch gNATSGO map unit keys at native resolution
x <- mukey.wcs(db = 'gnatsgo', aoi = aoi)


## TODO: this is wasteful, each call to linkComponentHorizonTabular() requests a lot of data from SDA

## major time-sinks:
# deratify() [60%]
# fetchSDA() [35%]
# slab()     [ 5%]

clay_0_5 <- linkComponentHorizonTabular(x, vars = c('claytotal_l', 'claytotal_r', 'claytotal_h'), interval = c(0, 5))

pH_0_5 <- linkComponentHorizonTabular(x, vars = c('ph1to1h2o_l', 'ph1to1h2o_r', 'ph1to1h2o_h'), interval = c(0, 5))


clay_30_60 <- linkComponentHorizonTabular(x, vars = c('claytotal_l', 'claytotal_r', 'claytotal_h'), interval = c(30, 60))


pH_30_60 <- linkComponentHorizonTabular(x, vars = c('ph1to1h2o_l', 'ph1to1h2o_r', 'ph1to1h2o_h'), interval = c(30, 60))


levelplot(clay_0_5, margin = FALSE, main = '', scales = list(draw = FALSE), maxpixels = 1e6, col.regions = viridis)
levelplot(pH_0_5, margin = FALSE, main = '', scales = list(draw = FALSE), maxpixels = 1e6, col.regions = viridis)

levelplot(pH_30_60[[2]], margin = FALSE, main = '', scales = list(draw = FALSE), maxpixels = 1e6, col.regions = viridis)


levelplot(
  stack(pH_0_5[[2]], pH_30_60[[2]]), 
  margin = FALSE, main = 'gNATSGO + SDA', 
  scales = list(draw = FALSE), 
  maxpixels = 1e6, 
  col.regions = viridis,
  names.attr = c('pH 1:1 H2O (0-5cm)', 'pH 1:1 H2O (30-60cm)')
  )


# double-check with mapview
library(mapview)
mapview(pH_30_60[[2]], maxpixels = 1e5)



