library(aqp)
library(soilDB)
library(raster)

# load latest SC-database
tf <- tempfile()
download.file(url = 'https://github.com/ncss-tech/SoilTaxonomy/raw/master/databases/SC-database.csv.gz', destfile = tf)
x <- read.csv(tf, stringsAsFactors=FALSE)

# keep only those records that are established or tentative
x <- subset(x, subset= tax_subgrp == 'typic calciargids')

# keep just the series names 
x <- x$soilseriesname

# iterate over series and download KSSL data
z <- fetchKSSL(x)

s <- filter(z, taxsubgrp == 'Typic Calciargids')

par(mar=c(0,0,3,0))
plotSPC(s, color='caco3', print.id = FALSE, divide.hz = FALSE, name = NA, width = 0.4)


plotSPC(s, color='estimated_ph_h2o', print.id = FALSE, divide.hz = FALSE, name = NA, width = 0.4)


## taxa extent
e <- taxaExtent('Typic Calciargids', level = 'subgroup')
mapview::mapview(e)

## now do this with SDA interface
