library(aqp)
library(soilDB)
library(sf)

# need latest sharpshootR from GH
library(sharpshootR)


## copy / paste viewport bounding-box from SoilWeb
## click somewhere on the map
## press 'b', BBOX is copied to the clipboard


# # https://twitter.com/MoreorLoess/status/1471935030746304521
# # https://casoilresource.lawr.ucdavis.edu/gmap/?loc=41.83547,-90.12201,z16
# bb <- '-90.1378 41.8273,-90.1378 41.8420,-90.1051 41.8420,-90.1051 41.8273,-90.1378 41.8273'

# # https://casoilresource.lawr.ucdavis.edu/gmap/?loc=38.54538,-121.74458,z14
# bb <- '-121.8100 38.5145,-121.8100 38.5762,-121.6792 38.5762,-121.6792 38.5145,-121.8100 38.5145'
 
 
# bb <- '-120.5453 37.5718,-120.5453 37.5796,-120.5289 37.5796,-120.5289 37.5718,-120.5453 37.5718'

# near Ithica, NY
bb <- '-76.6811 42.3178,-76.6811 42.3526,-76.5987 42.3526,-76.5987 42.3178,-76.6811 42.3178'



## assemble AOI polygon into WKT
wkt <- sprintf('POLYGON((%s))', bb)

## init sf polygon
x <- st_as_sfc(wkt)

# set CRS as GCS WGS84
st_crs(x) <- 4326

## get overlapping map unit keys
# could also use SDA_query() with more elaborate SQL
m <- SDA_spatialQuery(x, what = 'mukey')

## compose SQL to return component details for these map unit keys
sql <- sprintf("SELECT mukey, cokey, compname, comppct_r FROM component WHERE mukey IN %s AND majcompflag = 'Yes'", format_SQL_in_statement(as.integer(m$mukey)))

## send to SDA, result is a data.frame
s <- SDA_query(sql)

## get OSD morphology + extended summaries 
osd <- fetchOSD(unique(s$compname), extended = TRUE)


## check out results
str(osd, 1)





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
  cex.taxon.labels = 0.75, 
  cex.id = 0.66,
  cex.names = 0.66,
  width = 0.3, 
  name.style = 'center-center', 
  plot.depth.axis = TRUE,
  axis.line.offset = -3,
  hz.distinctness.offset = 'hzd'
)


## 3D geomorphic summary

# there may be records missing from SPC / geomorphic component
nm <- intersect(profile_id(osd$SPC), osd$geomcomp$series)

# keep only those series that exist in both
sub <- subset(osd$SPC, profile_id(osd$SPC) %in% nm)

## inverse problem: extra records in geomcomp summaries
# subset geomcopm
geomcomp.sub <- subset(osd$geomcomp, subset = series %in% profile_id(sub))

# viz geomorphic proportion summary, results contain clustering object
res <- vizGeomorphicComponent(geomcomp.sub)
print(res$fig)


# arrange according to clustering of geomorphic component
par(mar = c(0, 0, 0, 0))
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
# there may be records missing from SPC / hill slope position
nm <- intersect(profile_id(osd$SPC), osd$hillpos$series)

# keep only those series that exist in both
sub <- subset(osd$SPC, profile_id(osd$SPC) %in% nm)

## inverse problem: extra records in hill slope summaries
# subset hillpos
hillpos.sub <- subset(osd$hillpos, subset = series %in% profile_id(sub))

# viz hill slope proportion summary, results contain clustering object
res <- vizHillslopePosition(hillpos.sub)
print(res$fig)


# arrange according to clustering of hillslope position
par(mar = c(0, 0, 0, 0))
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


## borrowing ideas from this tutorial:
## https://ncss-tech.github.io/AQP/soilDB/exploring-geomorph-summary.html
##
hp.cols <- RColorBrewer::brewer.pal(n = 5, name = 'Set1')[c(2, 3, 4, 5, 1)]

# re-order hillslope proportions according to clustering
hp <- hillpos.sub[res$order, ]
nm <- names(hp[, 2:6])

par(mar = c(0.5, 0, 0, 2))
layout(matrix(c(1,2)), widths = c(1,1), heights = c(2,1))
plotProfileDendrogram(sub, res$clust, dend.y.scale = 3, scaling.factor = 0.012, y.offset = 0.2, width = 0.32, name.style = 'center-center', cex.names = 0.7, shrink = TRUE, cex.id = 0.55)

# symbol size is proportional to Shannon entropy
matplot(y = hp[, 2:6], type = 'b', lty = 1, pch = 16, axes = FALSE, col = hp.cols, xlab = '', ylab = '', xlim = c(0.5, length(sub) + 1), cex = hp$shannon_entropy + 0.2)
# grid(nx = 0, ny = NULL)
axis(side = 4, line = -1, las = 1, cex.axis = 0.7)
# axis(side = 2, line = -3, las = 1, cex.axis = 0.7)
legend('topleft', legend = rev(nm), col = rev(hp.cols), pch = 16, bty = 'n', cex = 0.8, pt.cex = 2, horiz = TRUE, inset = c(0.01, 0.01))
mtext('Probability', side = 2, line = -2, font = 2)


## TODO: encode Shannon entropy
par(mar = c(0.5, 0, 0, 2))
layout(matrix(c(1,2)), widths = c(1,1), heights = c(2,1))
plotProfileDendrogram(sub, res$clust, dend.y.scale = 3, scaling.factor = 0.012, y.offset = 0.2, width = 0.32, name.style = 'center-center', cex.names = 0.7, shrink = TRUE, cex.id = 0.55)

sp <- c(1.5, rep(1, times = length(sub) - 1))
barplot(height = t(as.matrix(hp[, 2:6])), beside = FALSE, width = 0.5, space = sp, col = hp.cols,  axes = FALSE, xlab = '', ylab = '', xlim = c(0.5, length(sub) + 1), ylim = c(0, 1.2))

legend(x = 0.75, y = 1.2, legend = rev(nm), col = rev(hp.cols), pch = 15, bty = 'n', cex = 0.8, pt.cex = 1.25, horiz = TRUE)
mtext('Probability', side = 2, line = -2, font = 2)



