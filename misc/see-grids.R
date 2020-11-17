library(raster)
library(rasterVis)
library(viridis)
library(sp)
library(rgdal)
library(soilDB)
library(mapview)

library(spData) 
library(sf) 

# CRS defs used by SEE grids
crs_lower48 <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

data("us_states")
us_states <- as(us_states, 'Spatial')
us_states <- spTransform(us_states, CRS(crs_lower48))

# get a bbox for CONUS and expand by 100km = 100,000m
b <- bbox(us_states)
x.lim <- c(b[1, 1] - 1e5, b[1, 2] + 1e5)
y.lim <- c(b[2, 1] - 1e5, b[2, 2] + 1e5)


fm <- raster('E:/temp/psamm.tif')

levelplot(
  fm,
  col.regions=viridis,
  main = 'psamm',
  margin = FALSE, 
  scales = list(draw = FALSE)
)

mapview(fm, col.regions = viridis, na.color = NA, use.layer.names = TRUE)


## 2020-11-17: not all soil order grids are available, must convert to tiled rasterization
## 2020-11-17: finish formative element grids


taxa <- 'haplustolls'
x <- taxaExtent(taxa, level = 'greatgroup')
a <- aggregate(x, fact = 5)

taxa <- 'vertisols'
x <- taxaExtent(taxa, level = 'order')
a <- aggregate(x, fact = 5)

taxa <- 'xeralfs'
x <- taxaExtent(taxa, level = 'suborder')
a <- aggregate(x, fact = 5)

taxa <- 'durixeralfs'
x <- taxaExtent(taxa, level = 'greatgroup')
a <- aggregate(x, fact = 5)

taxa <- 'cumulic haplustolls'
x <- taxaExtent(taxa, level = 'subgroup')
a <- aggregate(x, fact = 5)


taxa <- 'abruptic durixeralfs'
x <- taxaExtent(taxa, level = 'subgroup')
a <- aggregate(x, fact = 5)



## cropped to raster extent
levelplot(
  a,
  # maxpixels = 1e5,
  main = taxa,
  margin = FALSE, 
  # xlim = x.lim, ylim = y.lim,
  scales = list(draw = FALSE), 
  col.regions = viridis,
  panel = function(...) {
    panel.levelplot(...)
    sp.polygons(us_states, col = 'black', lwd = 2)
  }
)



## CONUS + states
## cropped to CONUS
levelplot(
  a,
  # maxpixels = 1e5,
  main = names(a),
  margin = FALSE, 
  xlim = x.lim, ylim = y.lim,
  scales = list(draw = FALSE), 
  col.regions = viridis,
  panel = function(...) {
    panel.levelplot(...)
    sp.polygons(us_states, col = 'black', lwd = 2)
  }
)




mapview(a, col.regions = viridis, na.color = NA, use.layer.names = TRUE)


x <- seriesExtent('Bonneau', type = 'raster')
a <- aggregate(x, fact = 5)
mapview(a, col.regions = viridis, na.color = NA, use.layer.names = TRUE)





r.1 <- seriesExtent('cecil', type = 'raster')
r.2 <- seriesExtent('appling', type = 'raster')

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

# ??
# joint distribution via bivariate color scheme
# https://nowosad.github.io/post/cbc-bp2/

library(pals)

bivcol = function(pal, labels = 1:9){
  tit = substitute(pal)
  pal = pal()
  ncol = length(pal)
  m <- matrix(seq_along(pal), nrow = sqrt(ncol))
  image(x = 1:3, y = 1:3, z = m,
        ylab = '', xlab = '',
        axes = FALSE, 
        col = pal, 
        asp = 1)
  mtext(tit)
  g <- expand.grid(1:ncol(m), 1:nrow(m))
  text(g$Var1, g$Var2, labels)
  }

bivcol(stevens.purplegold)



## can't readily use this with raster data
# https://cran.r-project.org/web/packages/biscale/vignettes/biscale.html

# library(biscale)
# library(sp)
# library(sf)

library(classInt)

## note: overlap classes are hard-coded, generalize to those cases where the full intersection of classes is not present


r.1 <- seriesExtent('cecil', type = 'raster')
r.2 <- seriesExtent('appling', type = 'raster')

c.1 <- classIntervals(r.1[], n = 3)
c.2 <- classIntervals(r.2[], n = 3)

r.1.class <- cut(r.1, breaks = c.1$brks)
r.2.class <- cut(r.2, breaks = c.2$brks)

r.joint <- r.1.class + (r.2.class * 10)

# must aggregate here, otherwise grid origin is modified
r.joint <- aggregate(r.joint, fact = 5, fun = modal)

table(r.joint[])

r.joint <- ratify(r.joint)

ll <- levels(r.joint)[[1]]
ll$attr <- 1:nrow(ll)
levels(r.joint) <- ll

## this fails when full intersection of classes is not present
bivcol(stevens.purplegold, labels = levels(r.joint)[[1]]$ID)

#    max
#    ^
# r2 min

# r1 min --> max



# 31 32 33
# 21 22 23
# 11 12 13

# 7 8 9
# 4 5 6
# 1 2 3

cols <- stevens.purplegold(9)

levelplot(
  r.joint,
  col.regions = cols,
  main = 'Joint',
  margin = FALSE, 
  scales = list(draw = FALSE)
)


mapview(r.joint, col.regions = cols, na.color = NA, use.layer.names = TRUE)

# legend <- bi_legend(pal = "DkBlue",
#                     dim = 3,
#                     xlab = "Higher % White ",
#                     ylab = "Higher Income ",
#                     size = 8)


