library(aqp)
library(soilDB)
library(sp)

## TODO: generalize this into a function that accepts group labels for comparisons

# comparisons
p.ut624 <- SpatialPoints(
  cbind(x = -109.66913, y = 38.68136), 
  proj4string = CRS('+proj=longlat +datum=WGS84')
)

p.ut687 <- SpatialPoints(
  cbind(x = -109.64742, y = 38.67399), 
  proj4string = CRS('+proj=longlat +datum=WGS84')
)

p <- rbind(p.ut624, p.ut687)

# query map unit records at this point
res <- SDA_spatialQuery(p, what = 'mukey')

# convert results into an SQL "IN" statement
# useful when there are multiple intersecting records
mu.is <- format_SQL_in_statement(res$mukey)

# composite SQL WHERE clause
sql <- sprintf("mukey IN %s", mu.is)

# get commonly used map unit / component / chorizon records
# as a SoilProfileCollection object
# confusing but essential: request that results contain `mukey`
# with `duplicates = TRUE`
x <- fetchSDA(sql, duplicates = TRUE)

# safely set texture class factor levels
# by making a copy of this column
# this will save in lieu of textures in the original
# `texture` column
horizons(x)$texture.class <- factor(x$texture, levels = SoilTextureLevels())

x$label <- sprintf("%s (%s%%)", x$compname, x$comppct_r)


par(mar = c(0, 0, 3, 0))

# graphical depiction of the result
plotSPC(x, color='texture.class', label='label', 
        name='hzname', cex.names = 1, width=0.25, 
        plot.depth.axis=FALSE, hz.depths=TRUE, 
        name.style='center-center'
)

plotSPC(x, color='claytotal_r', label='label', 
        name='hzname', cex.names = 1, width=0.25, 
        plot.depth.axis=FALSE, hz.depths=TRUE, 
        name.style='center-center'
)

groupedProfilePlot(
  x, groups = 'mukey', group.name.offset = -10, 
  color='texture.class', label='label', 
  name='hzname', cex.names = 1, width=0.25, 
  plot.depth.axis=FALSE, hz.depths=TRUE, 
  name.style='center-center',
  y.offset = 10
)

groupedProfilePlot(
  x, groups = 'mukey', group.name.offset = -10, 
  color='claytotal_r', label='label', 
  name='hzname', cex.names = 1, width=0.25, 
  plot.depth.axis=FALSE, hz.depths=TRUE, 
  name.style='center-center',
  y.offset = 10
)

groupedProfilePlot(
  x, groups = 'mukey', group.name.offset = -10, 
  color='ph1to1h2o_r', label='label', 
  name='hzname', cex.names = 1, width=0.25, 
  plot.depth.axis=FALSE, hz.depths=TRUE, 
  name.style='center-center',
  y.offset = 10
)


