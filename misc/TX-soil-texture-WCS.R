library(aqp)
library(terra)
library(soilDB)
library(elevatr)

# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=34.47387,-102.12719,z13
# TX069
bb <- '-102.2633 34.4032,-102.2633 34.5341,-102.0142 34.5341,-102.0142 34.4032,-102.2633 34.4032'
wkt <- sprintf("POLYGON((%s))", bb)
a <- vect(wkt, crs = 'epsg:4326')

texture_2550cm <- ISSR800.wcs(aoi = a, var = 'texture_2550cm')
plot(texture_2550cm, axes = FALSE)


m <- mukey.wcs(a, db = 'gSSURGO')

# extract RAT for thematic mapping
rat <- cats(m)[[1]]

# variables of interest
vars <- c("sandtotal_r","silttotal_r","claytotal_r")

p <-  get_SDA_property(property = vars,
                       method = "Dominant Component (Numeric)", 
                       mukeys = as.integer(rat$mukey),
                       top_depth = 25,
                       bottom_depth = 50)

# merge aggregate soil data into RAT
rat <- merge(rat, p, by.x = 'mukey', by.y = 'mukey', sort = FALSE, all.x = TRUE)

# requires that grid cell ID (mukey) be numeric
rat$mukey <- as.integer(rat$mukey)
levels(m) <- rat

ssc <- catalyze(m)[[vars]]

texture.class <- ssc[[1]]
names(texture.class) <- 'soil.texture'

# assign soil texture classes for the fine earth fraction
# using sand and clay percentages
values(texture.class) <- ssc_to_texcl(
  sand = values(ssc[['sandtotal_r']]), 
  clay = values(ssc[['claytotal_r']]), 
  droplevels = FALSE
)

plot(texture.class, col = hcl.colors(50), axes = FALSE, main = 'Soil Texture Class <2mm Fraction\nDominant Component, 25-50cm (RV)', mar = c(1, 1, 3, 2))


texture.rat <- read.csv('http://casoilresource.lawr.ucdavis.edu/800m_grids/RAT/texture_05.csv')

rat <- cats(texture.class)[[1]]

rat <- merge(rat, texture.rat[, c('class', 'hex', 'names')], by.x = 'label', by.y = 'class', all.x = TRUE, sort = FALSE)

rat <- rat[order(rat$value), ]
rat <- rat[, c('value', 'label', 'names', 'hex')]

levels(texture.rat) <- rat

plot(texture.class, col = hcl.colors(50), axes = FALSE, main = 'Soil Texture Class <2mm Fraction\nDominant Component, 25-50cm (RV)', mar = c(1, 1, 3, 2))

coltab(texture.class)
# coltab(texture.class) <- NULL

coltab(texture.class) <- rat[, c('value', 'hex')]

plot(texture.class, axes = FALSE, main = 'Soil Texture Class <2mm Fraction\nDominant Component, 25-50cm (RV)', mar = c(1, 1, 3, 2))

par(mfcol = c(1, 2))
plot(texture_2550cm, axes = FALSE, main = 'Soil Texture Class <2mm Fraction\nWeighted Mean, 25-50cm (ISSR-800)', mar = c(1, 1, 1, 4))
plot(texture.class, axes = FALSE, main = 'Soil Texture Class <2mm Fraction\nDominant Component, 25-50cm (SSURGO)', mar = c(1, 1, 1, 4))



## elevation

# query using GCS / WGS84 BBOX
m.ext <- as.polygons(ext(m))
crs(m.ext) <- 'epsg:5070'
m.ext <- project(m.ext, 'epsg:4326')

# ug, have to work through sf / raster objects
e <- get_elev_raster(sf::st_as_sf(m.ext), z = 14)

# convert to spatRaster and warp to 5070
e <- rast(e)
ee <- project(e, m, method = 'cubicspline', mask = TRUE)


par(mfcol = c(1, 2))

plot(ee, col = hcl.colors(50, palette = 'mako'), axes = FALSE, main = 'Elevation (m) ~4m', legend = FALSE, mar = c(1, 1, 1, 4))
plot(texture.class, axes = FALSE, main = 'Soil Texture Class <2mm Fraction\nDominant Component, 25-50cm (SSURGO)', mar = c(1, 1, 1, 4))





