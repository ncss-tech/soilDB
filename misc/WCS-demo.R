library(sp)
library(raster)
library(rasterVis)

a <- c(-121,37,-120,38)

# attempt in AEA
pH_05cm <- ISSR800.wcs(var = 'ph_05cm', aoi = a)
pH_3060cm <- ISSR800.wcs(var = 'ph_3060cm', aoi = a)

clay_05cm <- ISSR800.wcs(var = 'clay_05cm', aoi = a)
clay_3060cm <- ISSR800.wcs(var = 'clay_3060cm', aoi = a)

drainage_class <- ISSR800.wcs(var = 'drainage_class', aoi = a)
weg <- ISSR800.wcs(var = 'weg', aoi = a)

# attempt in GCS ~ 600m
pH_05cm.gcs <- ISSR800.wcs(var = 'ph_05cm', aoi = a, res = 0.004, crs = 'EPSG:4326')
drainage_class.gcs <- ISSR800.wcs(var = 'drainage_class', aoi = a, res = 0.004, crs = 'EPSG:4326')
weg.gcs <- ISSR800.wcs(var = 'weg', aoi = a, res = 0.004, crs = 'EPSG:4326')



# AEA
levelplot(stack(pH_05cm, pH_3060cm), margin = FALSE)

levelplot(stack(clay_05cm, clay_3060cm), margin = FALSE)

levelplot(drainage_class, margin = FALSE)
levelplot(weg, margin = FALSE)

# GCS
levelplot(pH_05cm.gcs, margin = FALSE)
levelplot(drainage_class.gcs, margin = FALSE)
levelplot(weg.gcs, margin = FALSE)


