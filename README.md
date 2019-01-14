[![Travis-CI Build Status](https://travis-ci.org/ncss-tech/soilDB.svg?branch=master)](https://travis-ci.org/ncss-tech/soilDB)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/soilDB)](https://cran.r-project.org/package=soilDB)
[![Total_Downloads](http://cranlogs.r-pkg.org/badges/grand-total/soilDB)](https://cran.r-project.org/package=soilDB)

## Installation

Get the stable version from CRAN:

`install.packages('soilDB', dep=TRUE)`

Get the development version from Github:

`remotes::install_github("ncss-tech/soilDB", dependencies=FALSE, upgrade=FALSE, build=FALSE)`

## Website
http://ncss-tech.github.io/AQP/


## Important changes in soilDB-2.0

### NASIS

 * low-level functions return empty `data.frame` objects when local database (or selected set) is empty
 * fetchNASIS() is now a wrapper around pedon and component "fetch" functions
 * uncode() is now used in all queries to local NASIS database
 * many (coded) column names in pedon @site and @horizons have changed
 * lower cases is used for all un-coded data
 

## Database APIs
    
  * SDA
    + [`SDA_query`](http://ncss-tech.github.io/soilDB/docs/reference/SDA_query.html)
    + ['SDA_query_features`](http://ncss-tech.github.io/soilDB/docs/reference/SDA_query_features.html)

  * SSURGO/KSSL via SoilWeb
    + [`fetchKSSL`](http://ncss-tech.github.io/soilDB/docs/reference/fetchKSSL.html)
    + [`fetchOSD`](http://ncss-tech.github.io/soilDB/docs/reference/fetchOSD.html)
    + [`siblings`](http://ncss-tech.github.io/soilDB/docs/reference/siblings.html)
    + [`OSDquery`](http://ncss-tech.github.io/soilDB/docs/reference/OSDquery.html) 
    + [`seriesExtent`](http://ncss-tech.github.io/soilDB/docs/reference/seriesExtent.html)
    
  * SCAN/SNOTEL
    + [`fetchSCAN`](http://ncss-tech.github.io/soilDB/docs/reference/fetchSCAN.html)
    + [`SCAN_SNOTEL_metadata`](http://ncss-tech.github.io/soilDB/docs/reference/SCAN_SNOTEL_metadata.html)
    
  * Henry Mount Soil and Water Database
    + [`fetchHenry`](http://ncss-tech.github.io/soilDB/docs/reference/fetchHenry.html)
    
  * NASIS local database
    + [`fetchNASIS`](http://ncss-tech.github.io/soilDB/docs/reference/fetchNASIS.html)
    
  * NASIS WWW interface
    + [`parseWebReport`](http://ncss-tech.github.io/soilDB/docs/reference/parseWebReport.html)
    + [`fetchNASISWebReport`](http://ncss-tech.github.io/soilDB/docs/reference/fetchLIMS_component.html)


## Utility Functions

  * [`estimateSTR`](http://ncss-tech.github.io/soilDB/docs/reference/estimateSTR.html)
  * [`STRplot`](http://ncss-tech.github.io/soilDB/docs/reference/STRplot.html)
  * [`KSSL_VG_model`](http://ncss-tech.github.io/soilDB/docs/reference/KSSL_VG_model.html)
  * [`simplfyFragmentData`](http://ncss-tech.github.io/soilDB/docs/reference/simplfyFragmentData.html)
  * [`simplifyColorData`](http://ncss-tech.github.io/soilDB/docs/reference/simplifyColorData.html)
  * [`uncode`](http://ncss-tech.github.io/soilDB/docs/reference/uncode.html)
  * [`code`](http://ncss-tech.github.io/soilDB/docs/reference/uncode.html)


## Related Documentation
 * [fetchKSSL](http://ncss-tech.github.io/AQP/soilDB/KSSL-demo.html)
 * [SDA_query](http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.html)
 * [fetchOSD](http://ncss-tech.github.io/AQP/sharpshootR/OSD-dendrogram.html)
 * [SCAN/SNOTEL Data](http://ncss-tech.github.io/AQP/soilDB/fetchSCAN-demo.html)

## Related Packages
 * [aqp](https://github.com/ncss-tech/aqp)
 * [sharpshootR](https://github.com/ncss-tech/sharpshootR)
 

## Examples
```r
library(soilDB)
library(sharpshootR)
library(latticeExtra)

# get morphology + extended summaries
soils <- c('cecil', 'altavista', 'lloyd', 'wickham', 'wilkes',  'chewacla', 'congaree')
s <- fetchOSD(soils, extended = TRUE)

# experimental viz of hillslope position, from SSURGO component records
res <- vizHillslopePosition(s$hillpos)
print(res$fig)

# profile sketches
par(mar=c(0,1,0,4), xpd=NA)
plot(s$SPC, plot.order=res$order, cex.names=1, axis.line.offset = -0.1, width=0.2)


# siblings
s <- 'Amador'
amador <- siblings(s, only.major = FALSE, component.data = TRUE)

# limit to named soil series
sib.data <- subset(amador$sib.data, subset= ! compkind %in% c('Miscellaneous area', 'Family', 'Taxon above family'))

# get parsed OSD records
sibs <- fetchOSD(c(s, unique(amador$sib$sibling)), extended = TRUE)

# order by subgroup taxonomy
# invert colors
par(mar=c(0,0,0,0), fg='white', bg='black')
SoilTaxonomyDendrogram(sibs$SPC, dend.width = 1.5, y.offset = 0.4, scaling.factor = 0.02, width=0.2, cex.taxon.labels = 1, cex.names = 1)
```

