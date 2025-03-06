library(aqp)
library(sf)
library(SoilTaxonomy)

# need latest from GH
library(sharpshootR)
library(soilDB)


## copy / paste viewport bounding-box from SoilWeb
## click somewhere on the map
## press 'b', BBOX is copied to the clipboard


## TX155: https://casoilresource.lawr.ucdavis.edu/gmap/?loc=34.05629,-99.57999,z14
bb <- '-99.6431 34.0224,-99.6431 34.0902,-99.5268 34.0902,-99.5268 34.0224,-99.6431 34.0224'


##
bb <- '-80.9121 35.9244,-80.9121 35.9410,-80.8829 35.9410,-80.8829 35.9244,-80.9121 35.9244'


## PA055
bb <- '-77.5878 39.9331,-77.5878 39.9645,-77.5191 39.9645,-77.5191 39.9331,-77.5878 39.9331'

## VA191
bb <- '-82.1785 36.6716,-82.1785 36.7044,-82.1098 36.7044,-82.1098 36.6716,-82.1785 36.6716'

## TN085
bb <- '-87.9174 35.8424,-87.9174 35.8756,-87.8487 35.8756,-87.8487 35.8424,-87.9174 35.8424'

## TN149
bb <- '-86.5358 35.6571,-86.5358 35.6737,-86.4971 35.6737,-86.4971 35.6571,-86.5358 35.6571'

## CA607
bb <- '-122.2687 40.4450,-122.2687 40.4757,-122.2063 40.4757,-122.2063 40.4450,-122.2687 40.4450'

## CA602
bb <- '-122.5402 41.6320,-122.5402 41.6923,-122.4154 41.6923,-122.4154 41.6320,-122.5402 41.6320'

## TX069
bb <- '-102.2001 34.4354,-102.2001 34.5009,-102.0756 34.5009,-102.0756 34.4354,-102.2001 34.4354'

## TX045
bb <- '-101.3237 34.5037,-101.3237 34.5711,-101.1752 34.5711,-101.1752 34.5037,-101.3237 34.5037'


# CA630
bb <- '-120.9782 38.1506,-120.9782 38.1828,-120.9039 38.1828,-120.9039 38.1506,-120.9782 38.1506'


## GCL's soil pit digging competition site
bb <- '-122.0050 39.6933,-122.0050 39.7543,-121.8931 39.7543,-121.8931 39.6933,-122.0050 39.6933'


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
  scaling.factor = 0.015, 
  cex.taxon.labels = 1, 
  cex.id = 1,
  cex.names = 0.85,
  width = 0.3, 
  name.style = 'center-center', 
  depth.axis = list(line = -4),
  # hz.distinctness.offset = 'hzd',
  max.depth = 200
)

## arrange according to classification, accounting for order within KST
## using ordered factors
SoilTaxonomyDendrogram(
  spc = osd$SPC, 
  KST.order = TRUE, 
  y.offset = 0.4, 
  scaling.factor = 0.015, 
  cex.taxon.labels = 1,
  cex.id = 1,
  cex.names = 0.9,
  width = 0.35, 
  name.style = 'center-center', 
  depth.axis = list(line = -4),
  hz.distinctness.offset = 'hzd',
  max.depth = 200
)




## updated content, based on latest sharpshootR

# reconcileOSDGeomorph() will perform cross-check between SPC--geomorph summary
# vizHillslopePosition() makes the cross-section

## notes:
# * the following functions will nearly always require tinkering with `scaling.factor`


options(.aqp.plotSPC.args = NULL)
options(.aqp.plotSPC.args = list(width = 0.35, scaling.factor = 0.013, max.depth = 150, cex.id = 0.7, cex.names = 0.85))

par(mar = c(0.5, 0, 0, 2), bg = 'black', fg = 'white')
plotGeomorphCrossSection(osd, type = 'line')
plotGeomorphCrossSection(osd, type = 'bar')

options(.aqp.plotSPC.args = list(width = 0.35, max.depth = 150, cex.id = 0.7, cex.names = 0.8))
plotGeomorphCrossSection(osd, type = 'line', clust = FALSE)
plotGeomorphCrossSection(osd, type = 'bar', clust = FALSE)



## TODO: combine 2D hillsope + flats
o1 <- reconcileOSDGeomorph(osd, selection = 'geomcomp')
o2 <- reconcileOSDGeomorph(osd, selection = 'flats')
# http://127.0.0.1:27837/graphics/8f1172c7-7e9e-49dd-9f4f-df3118a383b8.png
# safely combine SPCs, recognizing that there will be duplication
o1 <- o1$geom
o2 <- o2$geom

o <- subset(osd$SPC, profile_id(osd$SPC) %in% unique(c(o1$series, o2$series)))

o1$shannon_entropy <- NULL
o2$shannon_entropy <- NULL


o1
o2

o1.counts <- data.frame(
  round(
    sweep(o1[, -1], MARGIN = 1, STATS = o1$n, FUN = '*')
  )
)

o2.counts <- data.frame(
  round(
    sweep(o2[, -1], MARGIN = 1, STATS = o2$n, FUN = '*')
  )
)


o1.counts$n <- NULL
o2.counts$n <- NULL

o1 <- cbind(series = o1[, 1], o1.counts)
o2 <- cbind(series = o2[, 1], o2.counts)


g <- merge(o1, o2, by = 'series', all.x = TRUE, all.y = TRUE, sort = FALSE)

g2 <- lapply(g[, -1], function(i) {
  idx <- which(is.na(i)) 
  i[idx] <- rep(0, times = length(idx))
  return(i)
})

g2 <- as.data.frame(g2)

g2 <- sweep(g2, MARGIN = 1, STATS = rowSums(g2), FUN = '/')

g <- cbind(series = g[, 1], g2)

# check:
nrow(g) == length(o)

plotSPC(o)

knitr::kable(g, row.names = FALSE, digits = 2)


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



