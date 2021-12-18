library(aqp)
library(soilDB)
library(sharpshootR)
library(sf)

# https://twitter.com/MoreorLoess/status/1471935030746304521
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=41.83547,-90.12201,z16

## copy / paste from SoilWeb
## 'b' keypress
bb <- '-90.1378 41.8273,-90.1378 41.8420,-90.1051 41.8420,-90.1051 41.8273,-90.1378 41.8273'

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

## TODO: function should return clustering object 
# plotProfileDendrogram(osd$SPC, dend.y.scale = 3, scaling.factor = 0.01, clust = res$clust, width = 0.3, name.style = 'center-center', plot.depth.axis = FALSE, hz.depths = TRUE, hz.distinctness.offset = 'hzd')

## 2D geomorphic summary
res <- vizHillslopePosition(osd$hillpos)
print(res$fig)

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


