[![Travis-CI Build Status](https://travis-ci.org/ncss-tech/soilDB.svg?branch=master)](https://travis-ci.org/ncss-tech/soilDB)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/soilDB)](https://cran.r-project.org/package=soilDB)
[![Total_Downloads](http://cranlogs.r-pkg.org/badges/grand-total/soilDB)](https://cran.r-project.org/package=soilDB)

## Installation

Get the stable version from CRAN:

`install.packages('soilDB', dep=TRUE)`

Get the development version from Github:

`devtools::install_github("ncss-tech/soilDB", dependencies=FALSE, upgrade_dependencies=FALSE)`

## Website
http://ncss-tech.github.io/AQP/


## Important changes in soilDB-2.0

### NASIS

 * low-level functions return empty `data.frame` objects when local database (or selected set) is empty
 * fetchNASIS() is now a wrapper around pedon and component "fetch" functions
 * uncode() is now used in all queries to local NASIS database
 * many (coded) column names in pedon @site and @horizons have changed
 * lower cases is used for all un-coded data
 
 

## Examples
```r
library(soilDB)
library(plyr)
library(reshape2)

# search by series name
s <- fetchKSSL(series='auburn')

# search by bounding-box
# s <- fetchKSSL(bbox=c(-120, 37, -122, 38))

# how many pedons
length(s)

# plot 
par(mar=c(0,0,0,0))
plot(s, name='hzn_desgn', max.depth=150)


# get morphologic data too

# get lab and morphologic data
s <- fetchKSSL(series='auburn', returnMorphologicData = TRUE)

# extract SPC
pedons <- s$SPC

# simplify color data
s.colors <- simplifyColorData(s$morph$phcolor, id.var = 'labsampnum', wt='colorpct')

# merge color data into SPC
h <- horizons(pedons)
h <- join(h, s.colors, by='labsampnum', type='left', match='first')
horizons(pedons) <- h

# check
par(mar=c(0,0,0,0))
plot(pedons, color='moist_soil_color', print.id=FALSE, name='hzn_desgn')

# simplify fragment data
s.frags <- simplfyFragmentData(s$morph$phfrags, id.var='labsampnum')

# merge fragment data into SPC
h <- horizons(pedons)
h <- join(h, s.frags, by='labsampnum', type='left', match='first')
horizons(pedons) <- h

# check
par(mar=c(0,0,3,0))
plot(pedons, color='total_frags_pct', print.id=FALSE, name='hzn_desgn')
addVolumeFraction(pedons, 'total_frags_pct', pch=1, cex.min=0.25, cex.max = 0.5)
```


## Related Packages
 * [aqp](https://github.com/ncss-tech/aqp)
 * [sharpshootR](https://github.com/ncss-tech/sharpshootR)
 
