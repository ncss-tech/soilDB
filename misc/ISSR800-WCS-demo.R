library(soilDB)
library(rasterVis)

spec <- .ISSR800.spec
all(names(spec) == sapply(spec, '[[', 'dsn'))

a <- list(
  aoi = c(-115.0, 47.5, -114.0, 48.0),
  crs = '+init=EPSG:4326'
)


v <- WCS_details(wcs = 'ISSR800')

z <- lapply(v$var, function(i) {
  try(ISSR800.wcs(aoi = a, var = i, quiet = TRUE))
})

names(z) <- v$var

which(sapply(z, function(i) class(i) == 'try-error'))



x <- ISSR800.wcs(aoi = a, var = 'series_name')
levelplot(x, margin = FALSE)


x <- ISSR800.wcs(aoi = a, var = 'hydgrp')
levelplot(x, margin = FALSE)

x <- ISSR800.wcs(aoi = a, var = 'clay_025cm')
levelplot(x, margin = FALSE)

# NODATA issues
x <- ISSR800.wcs(aoi = a, var = 'cec_025cm')
levelplot(x, margin = FALSE)

x <- ISSR800.wcs(aoi = a, var = 'ec_025cm')
levelplot(x, margin = FALSE)


x <- ISSR800.wcs(aoi = a, var = 'soilorder')
levelplot(x, margin = FALSE)

x <- ISSR800.wcs(aoi = a, var = 'suborder')
levelplot(x, margin = FALSE)

x <- ISSR800.wcs(aoi = a, var = 'greatgroup')
levelplot(x, margin = FALSE)

x <- ISSR800.wcs(aoi = a, var = 'texture_05cm')
levelplot(x, margin = FALSE)

x <- ISSR800.wcs(aoi = a, var = 'texture_2550cm')
levelplot(x, margin = FALSE)


## these should have a RAT
x <- ISSR800.wcs(aoi = a, var = 'lcc_irrigated')
levelplot(x, margin = FALSE)

x <- ISSR800.wcs(aoi = a, var = 'lcc_nonirrigated')
levelplot(x, margin = FALSE)




## these require more explanation: edge effects, selection in final grids, etc. 
x <- ISSR800.wcs(aoi = a, var = 'ssurgo_pct')
levelplot(x, margin = FALSE)

x <- ISSR800.wcs(aoi = a, var = 'statsgo_pct')
levelplot(x, margin = FALSE)




a <- list(
  aoi = c(-122.0, 37.0, -118.0, 38.0),
  crs = '+init=EPSG:4326'
)


x <- ISSR800.wcs(aoi = a, var = 'clay_025cm')
levelplot(x, margin = FALSE)


