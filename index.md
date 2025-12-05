## Installation

Get the stable version from CRAN:

``` R
install.packages('soilDB', dependencies = TRUE)
```

Get the development version from GitHub:

``` R
remotes::install_github("ncss-tech/soilDB", dependencies = FALSE)
```

## Website

- CRAN Package: <https://cran.r-project.org/package=soilDB>
- Package Manual: <http://ncss-tech.github.io/soilDB/>
- Algorithms for Quantitative Pedology (AQP) Project:
  <http://ncss-tech.github.io/AQP/>

## Citation

``` R
## To cite soilDB in publications use:
## 
##   Beaudette D, Skovlin J, Roecker S, Brown A (2025). _soilDB: Soil
##   Database Interface_. R package version 2.8.13,
##   <https://CRAN.R-project.org/package=soilDB>.
## 
## A BibTeX entry for LaTeX users is
## 
##   @Manual{,
##     title = {soilDB: Soil Database Interface},
##     author = {Dylan Beaudette and Jay Skovlin and Stephen Roecker and Andrew Brown},
##     note = {R package version 2.8.13},
##     url = {https://CRAN.R-project.org/package=soilDB},
##     year = {2025},
##   }
```

## soilDB 2.8.13

## Functions by Data Source

- Soil Data Access (SDA)
  - [`fetchSDA`](http://ncss-tech.github.io/soilDB/reference/fetchSDA.md)
  - [`fetchSDA_spatial`](http://ncss-tech.github.io/soilDB/reference/fetchSDA_spatial.md)
  - [`SDA_query`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md)
  - [`SDA_spatialQuery`](http://ncss-tech.github.io/soilDB/reference/SDA_spatialQuery.md)
  - “SSURGO On Demand” Queries
    - [`get_SDA_hydric`](http://ncss-tech.github.io/soilDB/reference/get_SDA_hydric.md)
    - [`get_SDA_interpretation`](http://ncss-tech.github.io/soilDB/reference/get_SDA_interpretation.md)
    - [`get_SDA_muaggatt`](http://ncss-tech.github.io/soilDB/reference/get_SDA_muaggatt.md)
    - [`get_SDA_pmgroupname`](http://ncss-tech.github.io/soilDB/reference/get_SDA_pmgroupname.md)
    - [`get_SDA_property`](http://ncss-tech.github.io/soilDB/reference/get_SDA_property.md)
    - [`get_SDA_coecoclass`](http://ncss-tech.github.io/soilDB/reference/get_SDA_coecoclass.md)
    - [`get_SDA_metrics`](http://ncss-tech.github.io/soilDB/reference/get_SDA_metrics.md)
    - [`get_SDA_cosurfmorph`](http://ncss-tech.github.io/soilDB/reference/get_SDA_cosurfmorph.md)
- SSURGO Web Soil Survey
  - [`downloadSSURGO`](http://ncss-tech.github.io/soilDB/reference/downloadSSURGO.md)
  - [`createSSURGO`](http://ncss-tech.github.io/soilDB/reference/createSSURGO.md)
- SSURGO Local Geodatabases
  - [`fetchGDB`](http://ncss-tech.github.io/soilDB/reference/fetchGDB.md)
- ROSETTA
  - [`ROSETTA`](http://ncss-tech.github.io/soilDB/reference/ROSETTA.md)
- SSURGO/KSSL via SoilWeb
  - [`fetchKSSL`](http://ncss-tech.github.io/soilDB/reference/fetchKSSL.md)
  - [`fetchOSD`](http://ncss-tech.github.io/soilDB/reference/fetchOSD.md)
  - [`siblings`](http://ncss-tech.github.io/soilDB/reference/siblings.md)
  - [`OSDquery`](http://ncss-tech.github.io/soilDB/reference/OSDquery.md)
  - [`seriesExtent`](http://ncss-tech.github.io/soilDB/reference/seriesExtent.md)
  - [`taxaExtent`](http://ncss-tech.github.io/soilDB/reference/taxaExtent.md)
  - [`mukey.wcs`](http://ncss-tech.github.io/soilDB/reference/mukey.wcs.md)
  - [`ISSR800.wcs`](http://ncss-tech.github.io/soilDB/reference/ISSR800.wcs.md)
- NASIS WWW interface
  - [`parseWebReport`](http://ncss-tech.github.io/soilDB/reference/parseWebReport.md)
  - [`fetchNASISWebReport`](http://ncss-tech.github.io/soilDB/reference/fetchNASISWebReport.md)
- SCAN/SNOTEL
  - [`fetchSCAN`](http://ncss-tech.github.io/soilDB/reference/fetchSCAN.md)
  - [`SCAN_SNOTEL_metadata`](http://ncss-tech.github.io/soilDB/reference/SCAN_SNOTEL_metadata.md)
- Henry Mount Soil and Water Database
  - [`fetchHenry`](http://ncss-tech.github.io/soilDB/reference/fetchHenry.md)
- NASIS local database
  - [`fetchNASIS`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md)
  - [`dbConnectNASIS`](http://ncss-tech.github.io/soilDB/reference/dbConnectNASIS.md)
    (alias
    [`NASIS()`](http://ncss-tech.github.io/soilDB/reference/dbConnectNASIS.md))
  - [`dbQueryNASIS`](http://ncss-tech.github.io/soilDB/reference/dbQueryNASIS.md)
  - [`createStaticNASIS`](http://ncss-tech.github.io/soilDB/reference/createStaticNASIS.md)
- SoilGrids
  - [`fetchSoilGrids`](http://ncss-tech.github.io/soilDB/reference/fetchSoilGrids.md)

## Miscellaneous Functions

- [`estimateSTR`](http://ncss-tech.github.io/soilDB/reference/estimateSTR.md)
- [`STRplot`](http://ncss-tech.github.io/soilDB/reference/STRplot.md)
- [`KSSL_VG_model`](http://ncss-tech.github.io/soilDB/reference/KSSL_VG_model.md)
- [`simplifyFragmentData`](http://ncss-tech.github.io/soilDB/reference/simplifyFragmentData.md)
- [`simplifyColorData`](http://ncss-tech.github.io/soilDB/reference/simplifyColorData.md)
- [`uncode`](http://ncss-tech.github.io/soilDB/reference/uncode.md)
- [`code`](http://ncss-tech.github.io/soilDB/reference/code.md)
- [`get_NOAA_GHCND`](http://ncss-tech.github.io/soilDB/reference/get_NOAA_GHCND.md)

## Tutorials and Demonstrations

- [fetchKSSL](http://ncss-tech.github.io/AQP/soilDB/KSSL-demo.md)
- [SDA_query](http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.md)
- [fetchOSD](http://ncss-tech.github.io/AQP/sharpshootR/OSD-dendrogram.md)
- [SCAN/SNOTEL
  Data](http://ncss-tech.github.io/AQP/soilDB/fetchSCAN-demo.md)

## Related Packages

- [aqp](https://github.com/ncss-tech/aqp)
- [sharpshootR](https://github.com/ncss-tech/sharpshootR)
- [SoilTaxonomy](https://github.com/ncss-tech/SoilTaxonomy)
