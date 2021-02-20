[![CRAN Version
(Stable)](http://www.r-pkg.org/badges/version/soilDB)](https://cran.r-project.org/package=soilDB)
[![GitHub Version
(Development)](https://img.shields.io/badge/GitHub-2.6.0-yellowgreen)](https://github.com/ncss-tech/soilDB)
[![R-CMD-check Build
Status](https://github.com/ncss-tech/soilDB/workflows/R-CMD-check/badge.svg)](https://github.com/ncss-tech/soilDB/actions)
[![Total CRAN
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/soilDB)](https://cran.r-project.org/package=soilDB)
![CRAN/METACRAN](https://img.shields.io/cran/l/soilDB) [![soilDB
Manual](https://img.shields.io/badge/docs-HTML-informational)](https://ncss-tech.github.io/soilDB/docs)

Installation
------------

Get the stable version from CRAN:

    install.packages('soilDB', dependencies = TRUE)

Get the development version from GitHub:

    remotes::install_github("ncss-tech/soilDB", dependencies = FALSE, upgrade = FALSE, build = FALSE)

Website
-------

-   <a href="http://ncss-tech.github.io/AQP/" class="uri">http://ncss-tech.github.io/AQP/</a>

soilDB 2.6.0
------------

### Notices on Database Interfaces

#### NASIS

-   low-level functions return empty `data.frame` objects when local
    database (or selected set) is empty
-   `fetchNASIS()` is now a wrapper around pedon and component “fetch”
    functions
-   `uncode()` is now used in all queries to local NASIS database

#### Soil Data Access (SDA)

-   `SDA_query` returns a `try-error` for queries with invalid syntax or
    on network error; empty results are an empty `data.frame()`
-   `fetchSDA_spatial` can return STATSGO `gsmmupolygon` or Soil Survey
    Area `sapolygon` data; and can join to the `legend` table

#### Other APIs

-   **NEW** functions
    [`ROSETTA`](http://ncss-tech.github.io/soilDB/docs/reference/ROSETTA.html),
    [`taxaExtent`](http://ncss-tech.github.io/soilDB/docs/reference/taxaExtent.html),
    [`mukey.wcs`](http://ncss-tech.github.io/soilDB/docs/reference/mukey.wcs.html),
    [`ISSR800.wcs`](http://ncss-tech.github.io/soilDB/docs/reference/ISSR800.wcs.html)

Functions by Data Source
------------------------

-   SDA
    -   [`fetchSDA`](http://ncss-tech.github.io/soilDB/docs/reference/fetchSDA_component.html)
    -   [`fetchSDA_spatial`](http://ncss-tech.github.io/soilDB/docs/reference/fetchSDA_spatial.html)
    -   [`SDA_query`](http://ncss-tech.github.io/soilDB/docs/reference/SDA_query.html)
    -   [`SDA_spatialQuery`](http://ncss-tech.github.io/soilDB/docs/reference/SDA_spatialQuery.html)
-   SSURGO Local Geodatabases
    -   [`fetchGDB`](http://ncss-tech.github.io/soilDB/docs/reference/fetchGDB.html)
-   NASIS local database
    -   [`fetchNASIS`](http://ncss-tech.github.io/soilDB/docs/reference/fetchNASIS.html)
-   ROSETTA
    -   <span style="color:red">**NEW:**</span>
        [`ROSETTA`](http://ncss-tech.github.io/soilDB/docs/reference/ROSETTA.html)
-   SSURGO/KSSL via SoilWeb
    -   [`fetchKSSL`](http://ncss-tech.github.io/soilDB/docs/reference/fetchKSSL.html)
    -   [`fetchOSD`](http://ncss-tech.github.io/soilDB/docs/reference/fetchOSD.html)
    -   [`siblings`](http://ncss-tech.github.io/soilDB/docs/reference/siblings.html)
    -   [`OSDquery`](http://ncss-tech.github.io/soilDB/docs/reference/OSDquery.html)
    -   [`seriesExtent`](http://ncss-tech.github.io/soilDB/docs/reference/seriesExtent.html)
    -   <span style="color:red">**NEW:**</span>
        [`taxaExtent`](http://ncss-tech.github.io/soilDB/docs/reference/taxaExtent.html)
    -   <span style="color:red">**NEW:**</span>
        [`mukey.wcs`](http://ncss-tech.github.io/soilDB/docs/reference/mukey.wcs.html)
    -   <span style="color:red">**NEW:**</span>
        [`ISSR800.wcs`](http://ncss-tech.github.io/soilDB/docs/reference/ISSR800.wcs.html)
-   NASIS WWW interface
    -   [`parseWebReport`](http://ncss-tech.github.io/soilDB/docs/reference/parseWebReport.html)
    -   [`fetchNASISWebReport`](http://ncss-tech.github.io/soilDB/docs/reference/fetchNASISWebReport.html)
-   SCAN/SNOTEL
    -   [`fetchSCAN`](http://ncss-tech.github.io/soilDB/docs/reference/fetchSCAN.html)
    -   [`SCAN_SNOTEL_metadata`](http://ncss-tech.github.io/soilDB/docs/reference/SCAN_SNOTEL_metadata.html)
-   Henry Mount Soil and Water Database
    -   [`fetchHenry`](http://ncss-tech.github.io/soilDB/docs/reference/fetchHenry.html)
-   SoilGrids
    -   [`fetchSoilGrids`](http://ncss-tech.github.io/soilDB/docs/reference/fetchSoilGrids.html)

Miscellaneous Functions
-----------------------

-   [`estimateSTR`](http://ncss-tech.github.io/soilDB/docs/reference/estimateSTR.html)
-   [`STRplot`](http://ncss-tech.github.io/soilDB/docs/reference/STRplot.html)
-   [`KSSL_VG_model`](http://ncss-tech.github.io/soilDB/docs/reference/KSSL_VG_model.html)
-   [`simplfyFragmentData`](http://ncss-tech.github.io/soilDB/docs/reference/simplfyFragmentData.html)
-   [`simplifyColorData`](http://ncss-tech.github.io/soilDB/docs/reference/simplifyColorData.html)
-   [`uncode`](http://ncss-tech.github.io/soilDB/docs/reference/uncode.html)
-   [`code`](http://ncss-tech.github.io/soilDB/docs/reference/code.html)
-   [`get_NOAA_GHCND`](http://ncss-tech.github.io/soilDB/docs/reference/get_NOAA_GHCND.html)

Tutorials and Demonstrations
----------------------------

-   [fetchKSSL](http://ncss-tech.github.io/AQP/soilDB/KSSL-demo.html)
-   [SDA\_query](http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.html)
-   [fetchOSD](http://ncss-tech.github.io/AQP/sharpshootR/OSD-dendrogram.html)
-   [SCAN/SNOTEL
    Data](http://ncss-tech.github.io/AQP/soilDB/fetchSCAN-demo.html)

Related Packages
----------------

-   [aqp](https://github.com/ncss-tech/aqp)
-   [sharpshootR](https://github.com/ncss-tech/sharpshootR)

Examples
--------

### Load Soil Packages

    library(aqp)
    library(soilDB)
    library(sharpshootR)

### Get Official Series Description Data

    # get morphology + extended summaries
    soils <- c('cecil', 'altavista', 'lloyd', 'wickham', 'woodbridge', 'chewacla', 'congaree')
    s <- fetchOSD(soils, extended = TRUE)

### Visualize Soil-Landscape Relationships

    # viz of hillslope position, from SSURGO component records
    res <- vizHillslopePosition(s$hillpos, annotation.cex = 0.9)
    print(res$fig)

<img src="https://i.imgur.com/SMGMV1a.png" width="1056" />

### Make Profile Sketches

    par(mar = c(0, 1, 0, 4), xpd = NA)

    plotSPC(
      s$SPC,
      plot.order = res$order,
      cex.names = 1,
      axis.line.offset = -0.1,
      width = 0.2
    )

<img src="https://i.imgur.com/emCrWkJ.png" width="1344" />

### Identify Tabular “Siblings”

    s <- 'Amador'
    amador <- siblings(s, only.major = FALSE, component.data = TRUE)

    # limit to named soil series
    sib.data <- subset(amador$sib.data, !compkind %in% c('Miscellaneous area', 'Family', 'Taxon above family'))

    # get parsed OSD records
    sibs <- fetchOSD(c(s, unique(amador$sib$sibling)), extended = TRUE)

### Plot Dendrograms with Taxonomic Relationships

    # invert colors
    par(mar = c(0, 0, 0, 0),
        fg = 'white',
        bg = 'black')

    SoilTaxonomyDendrogram(
      sibs$SPC,
      dend.width = 1.5,
      y.offset = 0.4,
      scaling.factor = 0.02,
      width = 0.2,
      cex.taxon.labels = 1,
      cex.names = 1
    )

<img src="https://i.imgur.com/yFN8Ubu.png" width="1344" />

Dependency Graph
----------------

![](https://cran.microsoft.com/packagedata/graphs/soilDB.png)
