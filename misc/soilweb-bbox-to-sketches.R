library(aqp)
library(soilDB)
library(sf)

# need latest sharpshootR from GH
library(sharpshootR)


# https://twitter.com/MoreorLoess/status/1471935030746304521
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=41.83547,-90.12201,z16

## copy / paste from SoilWeb
## 'b' keypress
bb <- '-90.1378 41.8273,-90.1378 41.8420,-90.1051 41.8420,-90.1051 41.8273,-90.1378 41.8273'

# # https://casoilresource.lawr.ucdavis.edu/gmap/?loc=38.54538,-121.74458,z14
# bb <- '-121.8100 38.5145,-121.8100 38.5762,-121.6792 38.5762,-121.6792 38.5145,-121.8100 38.5145'
# 
# # 
# bb <- '-120.5453 37.5718,-120.5453 37.5796,-120.5289 37.5796,-120.5289 37.5718,-120.5453 37.5718'


## assemble AOI polygon into WKT
wkt <- sprintf('POLYGON((%s))', bb)

## init sf polygon
x <- st_as_sfc(wkt)
# GCS WGS84
st_crs(x) <- 4326

## get overlapping map unit keys
# could also use SDA_query() with more elaborate SQL
m <- SDA_spatialQuery(x, what = 'mukey')

## compose SQL to return component details for these map unit keys
sql <- sprintf("SELECT mukey, cokey, compname, comppct_r FROM component WHERE mukey IN %s AND majcompflag = 'Yes'", format_SQL_in_statement(m$mukey))

## send to SDA, result is a data.frame
s <- SDA_query(sql)

## get OSD morphology + extended summaries 
osd <- fetchOSD(unique(s$compname), extended = TRUE)

## convert horizon boundary distinctness -> vertical distance
# see manual page
osd$SPC$hzd <- hzDistinctnessCodeToOffset(
  osd$SPC$distinctness, 
  codes = c('very abrupt', 'abrubt', 'clear', 'gradual', 'diffuse')
)

## arrange sketches according to soil classification
SoilTaxonomyDendrogram(
  osd$SPC, 
  y.offset = 0.4, 
  scaling.factor = 0.0135, 
  cex.taxon.labels = 1, 
  width = 0.3, 
  name.style = 'center-center', 
  plot.depth.axis = FALSE, 
  hz.depths = TRUE, 
  hz.distinctness.offset = 'hzd'
)


## 3D geomorphic summary
res <- vizGeomorphicComponent(osd$geomcomp)
print(res$fig)

# This will fail when geomorphic summary or SPC contain a subset of the other
nm <- intersect(profile_id(osd$SPC), osd$geomcomp$series)

# keep only those series that exist in both
sub <- subset(osd$SPC, profile_id(osd$SPC) %in% nm)

# arrange according to clustering of geomorphic component
plotProfileDendrogram(
  sub,
  clust = res$clust,
  dend.y.scale = 3,
  scaling.factor = 0.01,
  width = 0.3,
  name.style = 'center-center',
  plot.depth.axis = FALSE,
  hz.depths = TRUE,
  hz.distinctness.offset = 'hzd',
  cex.names = 0.6,
  cex.id = 0.6
)


## 2D geomorphic summary
res <- vizHillslopePosition(osd$hillpos)
print(res$fig)

# This will fail when geomorphic summary or SPC contain a subset of the other
nm <- intersect(profile_id(osd$SPC), osd$hillpos$series)

# keep only those series that exist in both
sub <- subset(osd$SPC, profile_id(osd$SPC) %in% nm)

# arrange according to clustering of hillslope position
plotProfileDendrogram(
  osd$SPC, 
  clust = res$clust, 
  dend.y.scale = 3, 
  scaling.factor = 0.01, 
  width = 0.3, 
  name.style = 'center-center', 
  plot.depth.axis = FALSE, 
  hz.depths = TRUE, 
  hz.distinctness.offset = 'hzd', 
  cex.names = 0.6, 
  cex.id = 0.6
)


