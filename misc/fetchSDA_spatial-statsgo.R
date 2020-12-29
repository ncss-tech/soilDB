library(sp)
library(soilDB)

symbs <- c('CA077','CA632','CA644','CA630', 'CA628', 'CA649')

# get bounding boxes of survey areas
by.areasym.bbox <- fetchSDA_spatial(x = symbs, 
                                   by.col = "areasymbol",
                                   method = "bbox",
                                   geom.src = "sapolygon",
                                   add.fields = "legend.areaname")

# get overlapping STATSGO geometry
statsgo.overlap <- SDA_spatialQuery(by.areasym.bbox, 
                                    what = "geom", 
                                    db = "STATSGO")

# get unique mukey from overlap, and query their full extent
full.extent <- fetchSDA_spatial(x = unique(statsgo.overlap$mukey[1]), 
                                db = 'STATSGO', chunk.size = 1,
                                add.fields = "legend.areaname")
# compare
par(mar=c(0,0,0,0))
plot(full.extent)

plot(statsgo.overlap, col="blue", add=T)
plot(by.areasym.bbox, lty=2, lwd=3, border="red", add=T)

bad <- fetchSDA_spatial(x = "657964", db = 'STATSGO', chunk.size = 1)
bad.bbox <- fetchSDA_spatial(x = "657964", db = 'STATSGO', chunk.size = 1, method = "bbox")

library(sp)
library(soilDB)
bad.bbox <- fetchSDA_spatial(x = "657964", 
                             db = 'STATSGO', 
                             chunk.size = 1, 
                             method = "bbox",
                             add.fields = "mapunit.muname")
par(mar=c(0,0,0,0))
plot(bad.bbox, col="RED")
bad.bbox$muname[1]
