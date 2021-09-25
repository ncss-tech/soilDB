[![CRAN Version
(Stable)](http://www.r-pkg.org/badges/version/soilDB)](https://cran.r-project.org/package=soilDB)
[![GitHub Version
(Development)](https://img.shields.io/badge/GitHub-2.6.5-yellowgreen)](https://github.com/ncss-tech/soilDB)
[![R-CMD-check Build
Status](https://github.com/ncss-tech/soilDB/workflows/R-CMD-check/badge.svg)](https://github.com/ncss-tech/soilDB/actions)
[![Total CRAN
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/soilDB)](https://cran.r-project.org/package=soilDB)
![CRAN/METACRAN](https://img.shields.io/cran/l/soilDB) [![soilDB
Manual](https://img.shields.io/badge/docs-HTML-informational)](https://ncss-tech.github.io/soilDB/docs)

Installation
------------

Get the stable version (2.6.5) from CRAN:

    install.packages('soilDB', dependencies = TRUE)

Get the development version (2.6.5) from GitHub:

    remotes::install_github("ncss-tech/soilDB", dependencies = FALSE, upgrade = FALSE, build = FALSE)

Website
-------

-   <a href="http://ncss-tech.github.io/AQP/" class="uri">http://ncss-tech.github.io/AQP/</a>

Citation
--------

Dylan Beaudette, Jay Skovlin, Stephen Roecker and Andrew Brown (2021).
soilDB: Soil Database Interface. R package version 2.6.5.
<https://CRAN.R-project.org/package=soilDB>

soilDB 2.6.5
------------

### Notices on Database Interfaces

#### NASIS

-   all NASIS queries now use {DBI}, tested to work with {odbc} or
    {RSQLite} drivers; replacing {RODBC}
    -   Install required R packages with
        `install.packages(c("DBI","odbc","RSQLite"), dependencies = TRUE)`
-   new methods for connecting to NASIS and querying NASIS data allow
    for `dsn` argument to specify a local “static” SQLite file
    containing NASIS tables.
    -   Default argument `dsn = NULL` uses `"nasis_local"` [ODBC
        connection](http://ncss-tech.github.io/AQP/soilDB/setup_local_nasis.html)
        to a local NASIS SQL Server instance

#### Soil Data Access (SDA)

-   `SDA_query()` returns a `try-error` for queries with invalid syntax
    or on network error; empty results are an empty `data.frame()`

-   `SDA_spatialQuery()` can return Soil Survey Area `sapolygon` data

-   `fetchSDA_spatial()` can return STATSGO `gsmmupolygon` or Soil
    Survey Area `sapolygon` data; and can join to the `legend` table

-   Added several new SDA query methods based on
    [ssurgoOnDemand](https://github.com/ncss-tech/ssurgoOnDemand) by
    Jason Nemecek and Chad Ferguson:

    -   `get_SDA_property()`, `get_SDA_interpretation()`,
        `get_SDA_muaggatt()`, `get_SDA_hydric()`,
        `get_SDA_pmgroupname()`, `get_SDA_coecoclass()`

#### MS Access

-   AKSite, Montana RangeDB, and PedonPC 6.x get and fetch methods now
    use {DBI}+{odbc}

#### SoilWeb API

-   `ISSR800.wcs()` and `mukey.wcs()` now return a result that inherits
    from `try-error` (and a message) if the Web Coverage Service fails

Functions by Data Source
------------------------

-   SDA
    -   [`fetchSDA`](http://ncss-tech.github.io/soilDB/docs/reference/fetchSDA_component.html)
    -   [`fetchSDA_spatial`](http://ncss-tech.github.io/soilDB/docs/reference/fetchSDA_spatial.html)
    -   [`SDA_query`](http://ncss-tech.github.io/soilDB/docs/reference/SDA_query.html)
    -   [`SDA_spatialQuery`](http://ncss-tech.github.io/soilDB/docs/reference/SDA_spatialQuery.html)
    -   “SSURGO On Demand” Queries
        -   [`get_SDA_hydric`](http://ncss-tech.github.io/soilDB/docs/reference/get_SDA_hydric.html)
        -   [`get_SDA_interpretation`](http://ncss-tech.github.io/soilDB/docs/reference/get_SDA_interpretation.html)
        -   [`get_SDA_muaggatt`](http://ncss-tech.github.io/soilDB/docs/reference/get_SDA_muaggatt.html)
        -   [`get_SDA_pmgroupname`](http://ncss-tech.github.io/soilDB/docs/reference/get_SDA_pmgroupname.html)
        -   [`get_SDA_property`](http://ncss-tech.github.io/soilDB/docs/reference/get_SDA_property.html)
-   SSURGO Local Geodatabases
    -   [`fetchGDB`](http://ncss-tech.github.io/soilDB/docs/reference/fetchGDB.html)
-   ROSETTA
    -   [`ROSETTA`](http://ncss-tech.github.io/soilDB/docs/reference/ROSETTA.html)
-   SSURGO/KSSL via SoilWeb
    -   [`fetchKSSL`](http://ncss-tech.github.io/soilDB/docs/reference/fetchKSSL.html)
    -   [`fetchOSD`](http://ncss-tech.github.io/soilDB/docs/reference/fetchOSD.html)
    -   [`siblings`](http://ncss-tech.github.io/soilDB/docs/reference/siblings.html)
    -   [`OSDquery`](http://ncss-tech.github.io/soilDB/docs/reference/OSDquery.html)
    -   [`seriesExtent`](http://ncss-tech.github.io/soilDB/docs/reference/seriesExtent.html)
    -   [`taxaExtent`](http://ncss-tech.github.io/soilDB/docs/reference/taxaExtent.html)
    -   [`mukey.wcs`](http://ncss-tech.github.io/soilDB/docs/reference/mukey.wcs.html)
    -   [`ISSR800.wcs`](http://ncss-tech.github.io/soilDB/docs/reference/ISSR800.wcs.html)
-   NASIS WWW interface
    -   [`parseWebReport`](http://ncss-tech.github.io/soilDB/docs/reference/parseWebReport.html)
    -   [`fetchNASISWebReport`](http://ncss-tech.github.io/soilDB/docs/reference/fetchNASISWebReport.html)
-   SCAN/SNOTEL
    -   [`fetchSCAN`](http://ncss-tech.github.io/soilDB/docs/reference/fetchSCAN.html)
    -   [`SCAN_SNOTEL_metadata`](http://ncss-tech.github.io/soilDB/docs/reference/SCAN_SNOTEL_metadata.html)
-   Henry Mount Soil and Water Database
    -   [`fetchHenry`](http://ncss-tech.github.io/soilDB/docs/reference/fetchHenry.html)
-   NASIS local database
    -   [`fetchNASIS`](http://ncss-tech.github.io/soilDB/docs/reference/fetchNASIS.html)
    -   <span style="color:red">**NEW:**</span> Argument `dsn` to
        specify path to connect to alternate (SQLite) data sources with
        NASIS schema
    -   <span
        style="color:red">**NEW:**</span>[`dbConnectNASIS`](http://ncss-tech.github.io/soilDB/docs/reference/dbConnectNASIS.html)
        (alias `NASIS`) - create a *DBIConnection* to local NASIS
        database
    -   <span
        style="color:red">**NEW:**</span>[`dbQueryNASIS`](http://ncss-tech.github.io/soilDB/docs/reference/dbQueryNASIS.html) -
        query NASIS local database (and close connection with
        `close=TRUE`)
    -   <span
        style="color:red">**NEW:**</span>[`createStaticNASIS`](http://ncss-tech.github.io/soilDB/docs/reference/createStaticNASIS.html) -
        create list of NASIS tables or write to SQLite
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

<img src="https://i.imgur.com/IVSWxvy.png" width="1056" />

### Make Profile Sketches

    par(mar = c(0, 1, 0, 4), xpd = NA)

    plotSPC(
      s$SPC,
      plot.order = res$order,
      cex.names = 1,
      axis.line.offset = -0.1,
      width = 0.2
    )

<img src="https://i.imgur.com/ZHKWuJK.png" width="1344" />

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

<img src="https://i.imgur.com/PojpxjJ.png" width="1344" />

Dependency Graph
----------------

![](https://cran.microsoft.com/packagedata/graphs/soilDB.png)
