[![CRAN
version](https://www.r-pkg.org/badges/version/soilDB)](https://CRAN.R-project.org/package=soilDB)
[![CRAN
status](https://badges.cranchecks.info/worst/soilDB.svg)](https://cran.r-project.org/web/checks/check_results_soilDB.html)
[![Development
Version](https://ncss-tech.r-universe.dev/badges/soilDB)](https://ncss-tech.r-universe.dev/)
[![R-CMD-check Build
Status](https://github.com/ncss-tech/soilDB/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/ncss-tech/soilDB/actions)
[![Monthly CRAN
Downloads](https://cranlogs.r-pkg.org/badges/soilDB)](https://cran.r-project.org/package=soilDB)
![CRAN/METACRAN](https://img.shields.io/cran/l/soilDB) [![soilDB
Manual](https://img.shields.io/badge/docs-HTML-informational)](https://ncss-tech.github.io/soilDB/)

## Installation

Get the stable version from CRAN:

    install.packages('soilDB', dependencies = TRUE)

Get the development version from GitHub:

    remotes::install_github("ncss-tech/soilDB", dependencies = FALSE)

## Website

-   CRAN Package: <https://cran.r-project.org/package=soilDB>
-   Package Manual: <http://ncss-tech.github.io/soilDB/>
-   Algorithms for Quantitative Pedology (AQP) Project:
    <http://ncss-tech.github.io/AQP/>

## Citation

    ## To cite soilDB in publications use:
    ## 
    ##   Beaudette, D., Skovlin, J., Roecker, S., Brown, A. (2025). soilDB:
    ##   Soil Database Interface. R package version 2.8.9.
    ##   <https://CRAN.R-project.org/package=soilDB>
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {soilDB: Soil Database Interface},
    ##     author = {Dylan Beaudette and Jay Skovlin and Stephen Roecker and Andrew Brown},
    ##     note = {R package version 2.8.9},
    ##     url = {https://CRAN.R-project.org/package=soilDB},
    ##     year = {2025},
    ##   }

## soilDB 2.8.11

<!-- ### Notices on Database Interfaces -->
<!-- #### NASIS -->
<!-- #### Soil Data Access (SDA) -->
<!-- #### SoilWeb API -->
<!-- #### MS Access -->

## Functions by Data Source

-   Soil Data Access (SDA)
    -   [`fetchSDA`](http://ncss-tech.github.io/soilDB/reference/fetchSDA.html)
    -   [`fetchSDA_spatial`](http://ncss-tech.github.io/soilDB/reference/fetchSDA_spatial.html)
    -   [`SDA_query`](http://ncss-tech.github.io/soilDB/reference/SDA_query.html)
    -   [`SDA_spatialQuery`](http://ncss-tech.github.io/soilDB/reference/SDA_spatialQuery.html)
    -   “SSURGO On Demand” Queries
        -   [`get_SDA_hydric`](http://ncss-tech.github.io/soilDB/reference/get_SDA_hydric.html)
        -   [`get_SDA_interpretation`](http://ncss-tech.github.io/soilDB/reference/get_SDA_interpretation.html)
        -   [`get_SDA_muaggatt`](http://ncss-tech.github.io/soilDB/reference/get_SDA_muaggatt.html)
        -   [`get_SDA_pmgroupname`](http://ncss-tech.github.io/soilDB/reference/get_SDA_pmgroupname.html)
        -   [`get_SDA_property`](http://ncss-tech.github.io/soilDB/reference/get_SDA_property.html)
        -   [`get_SDA_coecoclass`](http://ncss-tech.github.io/soilDB/reference/get_SDA_coecoclass.html)
        -   [`get_SDA_metrics`](http://ncss-tech.github.io/soilDB/reference/get_SDA_metrics.html)
        -   [`get_SDA_cosurfmorph`](http://ncss-tech.github.io/soilDB/reference/get_SDA_cosurfmorph.html)
-   SSURGO Web Soil Survey
    -   [`downloadSSURGO`](http://ncss-tech.github.io/soilDB/reference/downloadSSURGO.html)
    -   [`createSSURGO`](http://ncss-tech.github.io/soilDB/reference/createSSURGO.html)
-   SSURGO Local Geodatabases
    -   [`fetchGDB`](http://ncss-tech.github.io/soilDB/reference/fetchGDB.html)
-   ROSETTA
    -   [`ROSETTA`](http://ncss-tech.github.io/soilDB/reference/ROSETTA.html)
-   SSURGO/KSSL via SoilWeb
    -   [`fetchKSSL`](http://ncss-tech.github.io/soilDB/reference/fetchKSSL.html)
    -   [`fetchOSD`](http://ncss-tech.github.io/soilDB/reference/fetchOSD.html)
    -   [`siblings`](http://ncss-tech.github.io/soilDB/reference/siblings.html)
    -   [`OSDquery`](http://ncss-tech.github.io/soilDB/reference/OSDquery.html)
    -   [`seriesExtent`](http://ncss-tech.github.io/soilDB/reference/seriesExtent.html)
    -   [`taxaExtent`](http://ncss-tech.github.io/soilDB/reference/taxaExtent.html)
    -   [`mukey.wcs`](http://ncss-tech.github.io/soilDB/reference/mukey.wcs.html)
    -   [`ISSR800.wcs`](http://ncss-tech.github.io/soilDB/reference/ISSR800.wcs.html)
-   NASIS WWW interface
    -   [`parseWebReport`](http://ncss-tech.github.io/soilDB/reference/parseWebReport.html)
    -   [`fetchNASISWebReport`](http://ncss-tech.github.io/soilDB/reference/fetchNASISWebReport.html)
-   SCAN/SNOTEL
    -   [`fetchSCAN`](http://ncss-tech.github.io/soilDB/reference/fetchSCAN.html)
    -   [`SCAN_SNOTEL_metadata`](http://ncss-tech.github.io/soilDB/reference/SCAN_SNOTEL_metadata.html)
-   Henry Mount Soil and Water Database
    -   [`fetchHenry`](http://ncss-tech.github.io/soilDB/reference/fetchHenry.html)
-   NASIS local database
    -   [`fetchNASIS`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.html)
    -   [`dbConnectNASIS`](http://ncss-tech.github.io/soilDB/reference/dbConnectNASIS.html)
        (alias `NASIS()`)
    -   [`dbQueryNASIS`](http://ncss-tech.github.io/soilDB/reference/dbQueryNASIS.html)
    -   [`createStaticNASIS`](http://ncss-tech.github.io/soilDB/reference/createStaticNASIS.html)
-   SoilGrids
    -   [`fetchSoilGrids`](http://ncss-tech.github.io/soilDB/reference/fetchSoilGrids.html)

## Miscellaneous Functions

-   [`estimateSTR`](http://ncss-tech.github.io/soilDB/reference/estimateSTR.html)
-   [`STRplot`](http://ncss-tech.github.io/soilDB/reference/STRplot.html)
-   [`KSSL_VG_model`](http://ncss-tech.github.io/soilDB/reference/KSSL_VG_model.html)
-   [`simplifyFragmentData`](http://ncss-tech.github.io/soilDB/reference/simplifyFragmentData.html)
-   [`simplifyColorData`](http://ncss-tech.github.io/soilDB/reference/simplifyColorData.html)
-   [`uncode`](http://ncss-tech.github.io/soilDB/reference/uncode.html)
-   [`code`](http://ncss-tech.github.io/soilDB/reference/code.html)
-   [`get_NOAA_GHCND`](http://ncss-tech.github.io/soilDB/reference/get_NOAA_GHCND.html)

## Tutorials and Demonstrations

-   [fetchKSSL](http://ncss-tech.github.io/AQP/soilDB/KSSL-demo.html)
-   [SDA\_query](http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.html)
-   [fetchOSD](http://ncss-tech.github.io/AQP/sharpshootR/OSD-dendrogram.html)
-   [SCAN/SNOTEL
    Data](http://ncss-tech.github.io/AQP/soilDB/fetchSCAN-demo.html)

## Related Packages

-   [aqp](https://github.com/ncss-tech/aqp)
-   [sharpshootR](https://github.com/ncss-tech/sharpshootR)
-   [SoilTaxonomy](https://github.com/ncss-tech/SoilTaxonomy)
