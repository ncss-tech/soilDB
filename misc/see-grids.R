library(raster)
library(rasterVis)
library(viridis)
library(sp)
library(rgdal)
library(soilDB)
library(mapview)


fm <- raster('E:/temp/psamm.tif')

levelplot(
  fm,
  col.regions=viridis,
  main = 'psamm',
  margin = FALSE, 
  scales = list(draw = FALSE)
)

mapview(fm, col.regions = viridis, na.color = NA, use.layer.names = TRUE)



taxa <- 'vertisols'
x <- taxaExtent(taxa, level = 'order')
a <- aggregate(x, fact = 5)

taxa <- 'ustalfs'
x <- taxaExtent(taxa, level = 'suborder')
a <- aggregate(x, fact = 5)

taxa <- 'haplohumults'
x <- taxaExtent(taxa, level = 'greatgroup')
a <- aggregate(x, fact = 5)

taxa <- 'Xeric Haplohumults'
x <- taxaExtent(taxa, level = 'subgroup')
a <- aggregate(x, fact = 5)


levelplot(a, margin = FALSE, scales = list(draw = FALSE), col.regions = viridis, main = names(a))

mapview(a, col.regions = viridis, na.color = NA, use.layer.names = TRUE)


x <- seriesExtent('Bonneau', type = 'raster')
a <- aggregate(x, fact = 5)
mapview(a, col.regions = viridis, na.color = NA, use.layer.names = TRUE)


r.1 <- seriesExtent('holland', type = 'raster')
r.2 <- seriesExtent('chaix', type = 'raster')

x <- (r.1 / 100.0) * (r.2 / 100.0)

e <- rbind(
  as(extent(r.1), 'SpatialPolygons'),
  as(extent(r.2), 'SpatialPolygons'),
  makeUniqueIDs = TRUE
)


levelplot(
  r.1,
  col.regions=viridis,
  main = 'HOLLAND',
  margin = FALSE, 
  scales = list(draw = FALSE)
)

levelplot(
  r.2,
  col.regions=viridis,
  main = 'CHAIX',
  margin = FALSE, 
  scales = list(draw = FALSE)
)


levelplot(
  x,
  col.regions=viridis,
  main = 'Joint',
  margin = FALSE, 
  scales = list(draw = FALSE)
)


