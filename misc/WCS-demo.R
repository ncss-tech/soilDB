library(aqp)
library(soilDB)
library(reshape2)

library(sp)
library(raster)
library(rasterVis)

# note that grid resolution isn't quite right

# BBOX in WGS84 coordinates
a <- c(-121,37,-120,38)

# attempt in AEA ~ 800m
pH_05cm <- ISSR800.wcs(var = 'ph_05cm', aoi = a)
pH_3060cm <- ISSR800.wcs(var = 'ph_3060cm', aoi = a)

clay_05cm <- ISSR800.wcs(var = 'clay_05cm', aoi = a)
clay_3060cm <- ISSR800.wcs(var = 'clay_3060cm', aoi = a)

drainage_class <- ISSR800.wcs(var = 'drainage_class', aoi = a)
weg <- ISSR800.wcs(var = 'weg', aoi = a)
str <- ISSR800.wcs(var = 'str', aoi = a)

# attempt in GCS ~ 600m
pH_05cm.gcs <- ISSR800.wcs(var = 'ph_05cm', aoi = a, res = 0.004, crs = 'EPSG:4326')
drainage_class.gcs <- ISSR800.wcs(var = 'drainage_class', aoi = a, res = 0.004, crs = 'EPSG:4326')
weg.gcs <- ISSR800.wcs(var = 'weg', aoi = a, res = 0.004, crs = 'EPSG:4326')



# AEA
levelplot(stack(pH_05cm, pH_3060cm), margin = FALSE)

levelplot(stack(clay_05cm, clay_3060cm), margin = FALSE)

levelplot(drainage_class, margin = FALSE)
levelplot(weg, margin = FALSE)
levelplot(str, margin = FALSE)

# GCS
levelplot(pH_05cm.gcs, margin = FALSE)
levelplot(drainage_class.gcs, margin = FALSE)
levelplot(weg.gcs, margin = FALSE)


##
## gNATSGO
##

# resampled to ~300m (643kb)
gn.300m <- mukey.wcs(var = 'gnatsgo', aoi = a, res = 300)

# native ~ 30m (64Mb)
# gn.30m <- mukey.wcs(var = 'gnatsgo', aoi = a, res = 30)

# AEA
levelplot(gn.300m, att = 'ID', margin = FALSE, colorkey = FALSE)


## overly-simplistic aggregation of tabular data from SDA which must be generalized / abstracted
## this will eventually be available as a macro / stored procedure in SDA

# get unique mukey
ll <- levels(gn.300m)[[1]]
IS <- format_SQL_in_statement(ll$ID)

# query SDA by mukey
# this will bring down most of the interesting site / horizon level attributes from SSURGO/STATSGO
ws <- sprintf("mukey IN %s", IS)
x <- fetchSDA(WHERE = ws, duplicates = TRUE, droplevels = TRUE, stringsAsFactors = FALSE)


# component level aggregation for variables and depth intervals of interest
# note that we get an "extra" depth interval of 5-30
x.a <- slab(x, cokey ~ ph1to1h2o_r + claytotal_r, slab.structure = c(0, 5, 30, 60), slab.fun = mean, na.rm = TRUE)

# remove 5-30cm interval
x.a <- x.a[x.a$top != 5 & x.a$bottom != 30, ]

# make an ID for reshaping
x.a$variable.id <- sprintf("%s%s%s", x.a$variable, x.a$top, x.a$bottom)

# long -> wide format
w <- dcast(x.a, cokey ~ variable.id, value.var = 'value')

# check: ok
head(w)

# MU-level aggregation / subset
s <- site(x)[, c('mukey', 'cokey', 'comppct_r')]
s <- merge(s, w, by = 'cokey', sort = FALSE)

# STATSGO map unit for testing
# l <- split(s, s$mukey)
# i <- i <- l[['660849']]

## TODO: generalize
wt.mean.component <- function(i, var) {
  # remove NA first
  i <- na.omit(i)
  # weighted mean
  wm <- sum(i[[var]]* i$comppct_r) / sum(i$comppct_r)
  
  # pack results
  res <- data.frame(
    mukey = i$mukey[1],
    var = wm,
    stringsAsFactors = FALSE
  )
  
  # re-name for convenience later
  names(res)[2] <- var
  
  return(res)
}


# component percentage weighted mean
ss <- split(s, s$mukey)

## TODO: generalize this
clay05 <- lapply(ss, wt.mean.component, var = 'claytotal_r05')
clay05 <- do.call('rbind', clay05)

clay3060 <- lapply(ss, wt.mean.component, var = 'claytotal_r3060')
clay3060 <- do.call('rbind', clay3060)

ph05 <- lapply(ss, wt.mean.component, var = 'ph1to1h2o_r05')
ph05 <- do.call('rbind', ph05)

ph3060 <- lapply(ss, wt.mean.component, var = 'ph1to1h2o_r3060')
ph3060 <- do.call('rbind', ph3060)

# merge all aggregate data into RAT
rat <- merge(ll, clay05, by.x = 'ID', by.y = 'mukey', sort = FALSE, all.x = TRUE)
rat <- merge(rat, clay3060, by.x = 'ID', by.y = 'mukey', sort = FALSE, all.x = TRUE)
rat <- merge(rat, ph05, by.x = 'ID', by.y = 'mukey', sort = FALSE, all.x = TRUE)
rat <- merge(rat, ph3060, by.x = 'ID', by.y = 'mukey', sort = FALSE, all.x = TRUE)

# NA present?
# if so, does it matter?

# preservation of mukey?
nrow(ll) == nrow(rat)

# re-pack RAT
levels(gn.300m) <- rat

# convert to raster of values via RAT
gn.300m.pH_05cm <- deratify(gn.300m, att = 'ph1to1h2o_r05')
gn.300m.pH_3060cm <- deratify(gn.300m, att = 'ph1to1h2o_r3060')
gn.300m.clay_05cm <- deratify(gn.300m, att = 'claytotal_r05')
gn.300m.clay_3060cm <- deratify(gn.300m, att = 'claytotal_r3060')

# hey, it worked!
levelplot(gn.300m.pH_05cm, margin = FALSE, main = 'gNATSGO 1:1 H2O pH 0-5cm')
levelplot(gn.300m.pH_3060cm, margin = FALSE, main = 'gNATSGO 1:1 H2O pH 30-60cm')

levelplot(gn.300m.clay_05cm, margin = FALSE, main = 'gNATSGO % clay 0-5cm')
levelplot(gn.300m.clay_3060cm, margin = FALSE, main = 'gNATSGO % clay 30-60cm')


## this has to be done with identical grid topology
# compare
pH_05cm <- ISSR800.wcs(var = 'ph_05cm', aoi = a, res = 300)

rs <- stack(pH_05cm, gn.300m.pH_05cm)
names(rs) <- c('ISSR-800', 'gNATSGO')

levelplot(rs, margin = FALSE, main = '1:1 H2O pH 0-5cm', scales = list(draw = FALSE), maxpixels = 1e6)








##
## notes
##

# https://www.mapserver.org/ogc/wcs_server.html?highlight=web%20coverage%20service#configuring-your-mapfile-to-serve-wcs-layers

# https://mapserver.org/mapfile/projection.html


# https://cran.r-project.org/web/packages/slga/vignettes/slga.html

# construct URLs and compute image dimensions:
# https://github.com/obrl-soil/slga/blob/master/R/url_generate.R
# https://github.com/obrl-soil/slga/tree/master/R


# ISSR-800 / gNATSGO CRS may be EPSG:6350
# https://www.fisheries.noaa.gov/inport/help/components/crs/1044
#
# BBOX from WGS84 are ~ 1m offset vs. +proj=aea ... init


# # ISSR-800 native CRS
# prepareAOI(c(-121,37,-120,38), res = 800)
# 
# # gNATSGO native CRS
# # height is slightly larger than Mapserver default
# 
# prepareAOI(c(-121,37,-120,38), res = 30)

## TODO: after I define a new EPSG code the WCS will 
#        be able to skip the server-side raster warping
#        and resampling

# 
# ## tempoary interface
# # raster warping / resampling done server-side for now (not ideal!)
# # var: raster data source name
# # aoi: BBOX in WGS84 GCS ~ c(-121,37,-120,38)
# # fmt: datatype
# # res: resolution in GCS (... yes I know this is dumb)
# # crs: EPSG code for BBOX coordinates and resulting image 
# WCS.demo <- function(var, aoi, fmt, res = 0.002, crs = '4326') {
#   
#   ## TODO: make sure this is correct in general
#   ## TODO: think about resampling issues
#   ## TODO: data should be stored so that resampling / warping is not neccessary
#   # compute image dimensions 
#   w <- round(abs(aoi[3] - aoi[1]) / res)
#   h <- round(abs(aoi[4] - aoi[2]) / res)
#   
#   ## possible formats: 
#   # GEOTIFF_BYTE (8bit unsigned integers)
#   # GEOTIFF_16 (16bit signed integers)
#   # GEOTIFF_FLOAT (32bit floating point)
#   
#   # base URL + parameters
#   # double-check version spec 1.0.0?
#   u <- sprintf(
#     'https://soilmap2-1.lawr.ucdavis.edu/cgi-bin/mapserv?map=/soilmap2/website/wcs/mukey-WCS.map&SERVICE=WCS&VERSION=1.0.0&REQUEST=GetCoverage&CRS=EPSG:%s&coverage=%s&FORMAT=%s&BBOX=%s&WIDTH=%s&HEIGHT=%s',
#     crs, var, fmt, paste(aoi, collapse = ','), w, h
#   )
#   
#   
#   # make space for the resulting GeoTiff
#   tf <- tempfile()
#   download.file(u, destfile = tf, mode = 'wb')
#   
#   # load pointer to file and return
#   res <- raster(tf)
#   
#   ## TODO: this isn't correct for all data (e.g. SAR), how do we set this server-side?
#   # specification of NODATA
#   NAvalue(res) <- 0
#   
#   return(res)
# }


## try it out with a couple of demo raster data sources

# 1 minute = 1 degree / 60 (minutes / degree) = 0.01666 degree
# 1 second = 1 second / 60 / 60 = 0.0002777778 degree
#
# 1 arc-second DEM are roughly 30m res
# 30m res ~ 0.0002777778 degree
# this translates to roughly 3600x3600px for a 1x1 degree block (~50Mb file)

# res = 0.002 degrees ~ 300m
# 
# # pH 0-5cm ISSR-800
# pH_05cm <- WCS.demo(var = 'ph_05cm', fmt = 'GEOTIFF_FLOAT', aoi = c(-121,37,-120,38), res = 0.002)
# 
# 
# 
# # survey type ISSR-800
# survey.type <- WCS.demo(var = 'survey_type', fmt = 'GEOTIFF_BYTE', aoi = c(-121,37,-120,38), res = 0.004)
# 
# # WEG ISSR-800
# weg <- WCS.demo(var = 'weg', fmt = 'GEOTIFF_BYTE', aoi = c(-121,37,-120,38), res = 0.002)
# 
# ## I made a small subset 
# # map unit keys
# # file is downloaded as floating point values, must be converted to integers
# gNATSGO_mukey <- WCS.demo(var = 'gnatsgo', fmt = 'GEOTIFF_FLOAT', aoi = c(-121,37,-120,38), res = 0.002)
# 
# # attempt in AEA (~62Mb)
# # it works, resolution is close to 30m
# gNATSGO_mukey.aea <- WCS.demo(var = 'gnatsgo', fmt = 'GEOTIFF_32', aoi = prepareAOI(c(-121,37,-120,38), res = 30)$aoi, res = 30, crs = '6350')
# 
# # read into memory
# pH_05cm <- readAll(pH_05cm)
# pH_05cm.aea <- readAll(pH_05cm.aea)
# 
# survey.type <- readAll(survey.type)
# weg <- readAll(weg)
# gNATSGO_mukey <- readAll(gNATSGO_mukey)
# 
# # check AEA version
# # resolution is a little funky
# pH_05cm.aea
# 
# 
# # convert categorical data -> integer-keyed values (RAT)
# survey.type <- ratify(survey.type)
# weg <- ratify(weg)
# gNATSGO_mukey <- ratify(gNATSGO_mukey)
# 
# # looks good
# levelplot(pH_05cm, margin = FALSE)
# 
# # artifacts?
# levelplot(pH_05cm.aea, margin = FALSE)
# 
# # looks good
# levelplot(weg, att = 'ID', margin = FALSE)
# 
# # integer map unit keys, colors don't mean anything
# levelplot(gNATSGO_mukey, att = 'ID', margin = FALSE, colorkey = FALSE)
# 
# # all SSURGO
# # "holes" are small water features which have been back-filled with STATSGO, not ideal
# levelplot(survey.type, att = 'ID', margin = FALSE, colorkey = FALSE)
# 
# # these are SSURGO / STATSGO map unit keys
# ll <- levels(gNATSGO_mukey)[[1]]
# head(ll, 10)
# nrow(ll)

# use SDA + SSURGO aggregation engine (SQL mostly) to aggregate data
# link aggregated data to RAT
# convert to grid of values
# done!


## alternatively, use SOD SQL:
# https://github.com/ncss-tech/ssurgoOnDemand/blob/master/SOD/SDA_Properties.py

# manual aggregation = large requests / results from SDA

##
## Dylan's notes for later
##

# 
# library(raster)
# 
# x <- raster('E:/gis_data/MapunitRaster_30m.tif')
# x
# 
# # INT4U should suffice
# dataType(x)
# 
# # ~ 9Mb
# file.size('E:/gis_data/MapunitRaster_30m.tif') / 1024 / 1024
# 
# projectExtent(x, '+proj=longlat +datum=NAD83')
# 
# # BBOX=minx,miny,maxx,maxy: Bounding box corners (lower left, upper right)
# 
# 

