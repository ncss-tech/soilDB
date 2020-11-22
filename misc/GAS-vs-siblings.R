library(aqp)
library(soilDB)



## GAS vs. siblings

o <- OSDquery(geog_assoc_soils = 'none')


x <- fetchOSD('KIEV', extended = TRUE)
x$geog_assoc_soils

siblings('kiev')
