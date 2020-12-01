library(soilDB)

symbols <- c('CA077','CA632','CA644','CA630', 'CA628', 'CA649')


by.areasym.bbox <- fetchSDA_spatial(x = symbols, 
                                   by.col = "areasymbol",
                                   method = "bbox",
                                   geom.src = "sapolygon",
                                   add.fields = "legend.areaname")

by.areasym <- fetchSDA_spatial(x = unique(by.areasym.bbox$lkey),
                                   by.col = "areasymbol",
                                   geom.src = "sapolygon")

by.areasym.pt <- fetchSDA_spatial(x = unique(by.areasym.bbox$lkey),
                                        by.col = "lkey",
                                        method = "point",
                                        geom.src = "sapolygon")


single.nmusym <- fetchSDA_spatial(x = "2x8l5", by = "nmusym")

library(sp)
plot(by.areasym.bbox, lty=2, border="red")
plot(single.nmusym, add=T)
plot(by.areasym, add=T, lty=2, border="blue")
plot(by.areasym.pt, add=T, pch=4)

