library(soilDB)
library(terra)

## NB: using SpatRaster result from mukey.wcs
## get MUKEY raster from web coverage service for a rectangular extent
ras <- mukey.wcs(list(aoi = c(xmin = -120.5035, ymin = 38.0786, 
                              xmax = -120.4211, ymax = 38.1092), 
                      crs = "OGC:CRS84"))
if (inherits(ras, 'RasterLayer')) ras <- terra::rast(ras)

## pick an interpretation/rule name of interest
 
# mrulename <- "DHS - Potential for Radioactive Sequestration"
# mrulename <- "AGR - California Revised Storie Index (CA)"
mrulename <- "FOR - Road Suitability (Natural Surface)"
# mrulename <- "WMS - Embankments, Dikes, and Levees"
# mrulename <- "Hybrid Wine Grape Varieties Site Desirability (Long)"
# mrulename <- "ENG - Shallow Excavations"

## get dominant condition ratings and classes
rating <- get_SDA_interpretation(rulename = mrulename,
                                 method   = "Dominant Condition",
                                 mukeys   = unique(values(ras)))

## lookup names/colors etc for rating classes
explanation <- get_SDV_legend_elements(sprintf("nasisrulename = '%s'", mrulename))

## update raster attribute table
shortname <- paste0("class_", gsub("[^A-Za-z]", "", mrulename))
newrat <- rating[, c('mukey', shortname)]
newrat[[shortname]] <- as.numeric(factor(newrat[[shortname]], 
                                         levels = explanation$value))
levels(ras)[[1]] <- newrat

## set up categorical raster with full set of labels in order
r2 <- as.numeric(ras) - 1 # terra categorical rasters start at 0
levels(r2) <- explanation$label

## plot
plot(r2, col = explanation$hex, mar = c(2, 2, 2, 8), main = mrulename)

# write .tif file with auxiliary category info
writeRaster(r2, filename = paste0(mrulename, ".tif"), overwrite = TRUE)
