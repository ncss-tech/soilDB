library(aqp)
library(sf)
library(SoilTaxonomy)

# need latest from GH
library(sharpshootR)
library(soilDB)


## copy / paste viewport bounding-box from SoilWeb
## click somewhere on the map
## press 'b', BBOX is copied to the clipboard


# # https://twitter.com/MoreorLoess/status/1471935030746304521
# # https://casoilresource.lawr.ucdavis.edu/gmap/?loc=41.83547,-90.12201,z16
bb <- '-90.1378 41.8273,-90.1378 41.8420,-90.1051 41.8420,-90.1051 41.8273,-90.1378 41.8273'

# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=38.54538,-121.74458,z14
bb <- '-121.8100 38.5145,-121.8100 38.5762,-121.6792 38.5762,-121.6792 38.5145,-121.8100 38.5145'
 
## OH
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=39.33287,-82.68023,z15
bb <- '-82.7149 39.3168,-82.7149 39.3487,-82.6417 39.3487,-82.6417 39.3168,-82.7149 39.3168'

 
# bb <- '-120.5453 37.5718,-120.5453 37.5796,-120.5289 37.5796,-120.5289 37.5718,-120.5453 37.5718'

# near Ithica, NY
bb <- '-76.6811 42.3178,-76.6811 42.3526,-76.5987 42.3526,-76.5987 42.3178,-76.6811 42.3178'


# WI, problematic misc. areas / series named MARSH
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=45.90325,-86.56538,z16
bb <- '-86.5810 45.8964,-86.5810 45.9107,-86.5481 45.9107,-86.5481 45.8964,-86.5810 45.8964'

# Asheville, NC
bb <- '-82.5218 35.5270,-82.5218 35.5354,-82.5047 35.5354,-82.5047 35.5270,-82.5218 35.5270'
bb <- '-82.3652 35.4593,-82.3652 35.4929,-82.2971 35.4929,-82.2971 35.4593,-82.3652 35.4593'

# Hendersonville, NC
bb <- '-82.4952 35.3743,-82.4952 35.4079,-82.4271 35.4079,-82.4271 35.3743,-82.4952 35.3743'


# KY155
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=37.63262,-85.36737,z15
bb <- '-85.4077 37.6151,-85.4077 37.6488,-85.3345 37.6488,-85.3345 37.6151,-85.4077 37.6151'


# TN015
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=35.69268,-86.04904,z16
bb <- '-86.0838 35.6759,-86.0838 35.7094,-86.0156 35.7094,-86.0156 35.6759,-86.0838 35.6759'




## assemble AOI polygon into WKT
wkt <- sprintf('POLYGON((%s))', bb)

## init sf polygon
# WGS84 GCS
x <- st_as_sfc(wkt, crs = 4326)


## get overlapping map unit keys
# could also use SDA_query() with more elaborate SQL
m <- SDA_spatialQuery(x, what = 'mukey')

## compose SQL to return component details for these map unit keys
# return only:
# * map units overlapping with BBOX
# * major components
# * no misc. areas that might share name with a poorly-named soil series
sql <- sprintf(
  "SELECT mukey, cokey, compname, compkind, comppct_r 
  FROM component 
  WHERE mukey IN %s 
  AND majcompflag = 'Yes'
  AND compkind != 'Miscellaneous area'
  ", format_SQL_in_statement(as.integer(m$mukey))
)

## send to SDA, result is a data.frame
s <- SDA_query(sql)


## get OSD morphology + extended summaries 
osd <- fetchOSD(unique(s$compname), extended = TRUE)


## check out results
str(osd, 1)



## classic arrangement, using normal (nominal) factors
SoilTaxonomyDendrogram(
  osd$SPC, 
  KST.order = FALSE,
  y.offset = 0.4, 
  scaling.factor = 0.0135, 
  cex.taxon.labels = 0.75, 
  cex.id = 0.66,
  cex.names = 0.66,
  width = 0.3, 
  name.style = 'center-center', 
  plot.depth.axis = TRUE,
  axis.line.offset = -3,
  hz.distinctness.offset = 'hzd',
  max.depth = 150
)

## arrange according to classification, accounting for order within KST
## using ordered factors
SoilTaxonomyDendrogram(
  spc = osd$SPC, 
  KST.order = TRUE, 
  y.offset = 0.4, 
  scaling.factor = 0.014, 
  cex.taxon.labels = 0.75,
  cex.id = 0.85,
  cex.names = 0.75,
  width = 0.3, 
  name.style = 'center-center', 
  plot.depth.axis = TRUE,
  axis.line.offset = -3.5,
  hz.distinctness.offset = 'hzd',
  max.depth = 200
)




## updated content, based on latest sharpshootR

# reconcileOSDGeomorph() will perform cross-check between SPC--geomorph summary
# vizHillslopePosition() makes the cross-section
options(.aqp.plotSPC.args = NULL)
options(.aqp.plotSPC.args = list(width = 0.35, scaling.factor = 0.01, max.depth = 200))
par(mar = c(0.5, 0, 0, 2), bg = 'black', fg = 'white')
plotGeomorphCrossSection(osd, type = 'line')
plotGeomorphCrossSection(osd, type = 'bar')

options(.aqp.plotSPC.args = list(width = 0.35, max.depth = 200))
plotGeomorphCrossSection(osd, type = 'line', clust = FALSE)
plotGeomorphCrossSection(osd, type = 'bar', clust = FALSE)



## also updated, better hydrologic sorting

# hillpos geomorphic summary
o <- reconcileOSDGeomorph(osd, 'hillpos')
res <- vizHillslopePosition(o$geom)
print(res$fig)
res$match.rate

# 3D geomorphic summary
o <- reconcileOSDGeomorph(osd, 'geomcomp')
res <- vizGeomorphicComponent(o$geom)
print(res$fig)
res$match.rate

# flats geomorphic summary
o <- reconcileOSDGeomorph(osd, 'flats')
res <- vizFlatsPosition(o$geom)
print(res$fig)
res$match.rate

# terrace geomorphic summary
o <- reconcileOSDGeomorph(osd, 'terrace')
res <- vizTerracePosition(o$geom)
print(res$fig)
res$match.rate

# mountain geomorphic summary
o <- reconcileOSDGeomorph(osd, 'mtnpos')
res <- vizMountainPosition(o$geom)
print(res$fig)
res$match.rate

# shape geomorphic summary
o <- reconcileOSDGeomorph(osd, 'shape_across')
res <- vizSurfaceShape(o$geom, title = 'Shape Across')
print(res$fig)
res$match.rate

# shape geomorphic summary
o <- reconcileOSDGeomorph(osd, 'shape_down')
res <- vizSurfaceShape(o$geom, title = 'Shape Down')
print(res$fig)
res$match.rate



## old content, manual reconciliation / cross-sections

# 
# 
# 
# 
# 
# ## 3D geomorphic summary
# 
# # there may be records missing from SPC / geomorphic component
# nm <- intersect(profile_id(osd$SPC), osd$geomcomp$series)
# 
# # keep only those series that exist in both
# sub <- subset(osd$SPC, profile_id(osd$SPC) %in% nm)
# 
# ## inverse problem: extra records in geomcomp summaries
# # subset geomcopm
# geomcomp.sub <- subset(osd$geomcomp, subset = series %in% profile_id(sub))
# 
# # viz geomorphic proportion summary, results contain clustering object
# res <- vizGeomorphicComponent(geomcomp.sub)
# print(res$fig)
# 
# 
# # arrange according to clustering of geomorphic component
# par(mar = c(0, 0, 0, 0))
# plotProfileDendrogram(
#   sub,
#   clust = res$clust,
#   y.offset = 0.2,
#   dend.y.scale = 3,
#   scaling.factor = 0.01,
#   width = 0.3,
#   name.style = 'center-center',
#   plot.depth.axis = FALSE,
#   hz.depths = TRUE,
#   hz.distinctness.offset = 'hzd',
#   cex.names = 0.6,
#   cex.id = 0.6
# )
# 
# 
# ## 2D geomorphic summary
# # there may be records missing from SPC / hill slope position
# nm <- intersect(profile_id(osd$SPC), osd$hillpos$series)
# 
# # keep only those series that exist in both
# sub <- subset(osd$SPC, profile_id(osd$SPC) %in% nm)
# 
# ## inverse problem: extra records in hill slope summaries
# # subset hillpos
# hillpos.sub <- subset(osd$hillpos, subset = series %in% profile_id(sub))
# 
# # viz hill slope proportion summary, results contain clustering object
# res <- vizHillslopePosition(hillpos.sub)
# print(res$fig)
# 
# 
# # arrange according to clustering of hillslope position
# par(mar = c(0, 0, 0, 0))
# plotProfileDendrogram(
#   sub, 
#   clust = res$clust, 
#   dend.y.scale = 3, 
#   y.offset = 0.2,
#   scaling.factor = 0.01, 
#   width = 0.3, 
#   name.style = 'center-center', 
#   plot.depth.axis = FALSE, 
#   hz.depths = TRUE, 
#   hz.distinctness.offset = 'hzd', 
#   cex.names = 0.6, 
#   cex.id = 0.6
# )
# 
# 
# ## borrowing ideas from this tutorial:
# ## https://ncss-tech.github.io/AQP/soilDB/exploring-geomorph-summary.html
# ##
# hp.cols <- RColorBrewer::brewer.pal(n = 5, name = 'Set1')[c(2, 3, 4, 5, 1)]
# 
# # re-order hillslope proportions according to clustering
# hp <- hillpos.sub[res$order, ]
# nm <- names(hp[, 2:6])
# 
# par(mar = c(0.5, 0, 0, 2))
# layout(matrix(c(1,2)), widths = c(1,1), heights = c(2,1))
# plotProfileDendrogram(sub, res$clust, dend.y.scale = 3, scaling.factor = 0.012, y.offset = 0.2, width = 0.32, name.style = 'center-center', cex.names = 0.7, shrink = TRUE, cex.id = 0.55)
# 
# ## TODO: encode Shannon entropy: values are computed row-wise, data plotted as columns
# matplot(y = hp[, 2:6], type = 'b', lty = 1, pch = 16, axes = FALSE, col = hp.cols, xlab = '', ylab = '', xlim = c(0.5, length(sub) + 1))
# # grid(nx = 0, ny = NULL)
# axis(side = 4, line = -1, las = 1, cex.axis = 0.7)
# # axis(side = 2, line = -3, las = 1, cex.axis = 0.7)
# legend('topleft', legend = rev(nm), col = rev(hp.cols), pch = 16, bty = 'n', cex = 0.8, pt.cex = 2, horiz = TRUE, inset = c(0.01, 0.01))
# mtext('Probability', side = 2, line = -2, font = 2)
# 
# 
# ## TODO: encode Shannon entropy
# par(mar = c(0.5, 0, 0, 2))
# layout(matrix(c(1,2)), widths = c(1,1), heights = c(2,1))
# plotProfileDendrogram(sub, res$clust, dend.y.scale = 3, scaling.factor = 0.012, y.offset = 0.2, width = 0.32, name.style = 'center-center', cex.names = 0.7, shrink = TRUE, cex.id = 0.55, hz.distinctness.offset = 'hzd')
# 
# sp <- c(1.5, rep(1, times = length(sub) - 1))
# barplot(height = t(as.matrix(hp[, 2:6])), beside = FALSE, width = 0.5, space = sp, col = hp.cols,  axes = FALSE, xlab = '', ylab = '', xlim = c(0.5, length(sub) + 1), ylim = c(0, 1.2))
# 
# idx <- match(hp$series, profile_id(sub))
# text(x = (1:nrow(hp)) + 0.4, y = 0.5, labels = sub$subgroup[idx], cex = 0.75, srt = 90, font = 2)
# 
# legend(x = 0.75, y = 1.2, legend = rev(nm), col = rev(hp.cols), pch = 15, bty = 'n', cex = 0.8, pt.cex = 1.25, horiz = TRUE)
# mtext('Probability', side = 2, line = -2, font = 2)



