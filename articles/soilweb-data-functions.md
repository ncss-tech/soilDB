# Functions and Data Sources that Depend on SoilWeb

## Summary

The soilDB package relies on a number of data sources curated as part of
the [SoilWeb](https://casoilresource.lawr.ucdavis.edu/soilweb-apps)
ecosystem of web applications, APIs, web coverage services (WCS), and
internally-used lookup tables. These data are always updated shortly
after the beginning of each fiscal year (October 1st) as part of the
SSURGO refresh cycle. Within-cycle updates are typically performed
quarterly.

Many of these curated data sources and their evolution through time have
been documented in:

- O’Geen, A., Walkinshaw, M. and Beaudette, D.E. (2017), SoilWeb: A
  Multifaceted Interface to Soil Survey Information. Soil Sci. Soc. Am.
  J., 81: 853-862. <https://doi.org/10.2136/sssaj2016.11.0386n>

- Beaudette, D.E. and O’Geen, A.T. (2010), An iPhone Application for
  On-Demand Access to Digital Soil Survey Information. Soil Sci. Soc.
  Am. J., 74: 1682-1684. <https://doi.org/10.2136/sssaj2010.0144N>

- Beaudette, D.E. and O’Geen, A.T. (2009), Soil-Web: An online soil
  survey for California, Arizona, and Nevada, Comp. & Geo. Sci.,
  35:2119-2128, <https://doi.org/10.1016/j.cageo.2008.10.016>

Note that this vignette as published on CRAN or from a local R package
installation will not contain output from inline code sections. See the
Articles tab within the [soilDB package
website](https://ncss-tech.github.io/soilDB/) for a more comprehensive
version of this document.

### Primary Data Sources

- [SSURGO](https://www.nrcs.usda.gov/resources/data-and-reports/soil-survey-geographic-database-ssurgo),
  the detailed soil survey of the United States
- [STATSGO2](https://www.nrcs.usda.gov/resources/data-and-reports/description-of-statsgo2-database),
  the generalized soil survey of the United States
- Official Series Descriptions (OSD) via
  [SoilKnowledgeBase](https://github.com/ncss-tech/SoilKnowledgeBase/)
- Soil Classification (SC) database (no public version)
- [USDA Ag. Handbook 296 – Major Land Resource Area
  (MLRA)](https://www.nrcs.usda.gov/resources/data-and-reports/major-land-resource-area-mlra)
- Climate ([PRISM](https://prism.oregonstate.edu/)) data and derivatives
- Digital elevation model (DEM) derivatives
- [NCSS Lab Data Mart](https://ncsslabdatamart.sc.egov.usda.gov/) (LDM)
  snapshot
- NRCS [Rapid Carbon
  Assessment](https://www.nrcs.usda.gov/resources/data-and-reports/rapid-carbon-assessment-raca)
  (RaCA)

### `soilDB` Functions

The following functions interact with SoilWeb APIs or Web Coverage
Service (WCS) to request and download tabular or spatial data. Some
functions return `SoilProfileCollection` objects, as defined in the
[aqp](https://ncss-tech.github.io/aqp/) package.

Tabular data and/or `SoilProfileCollection`:

- [`fetchOSD()`](http://ncss-tech.github.io/soilDB/reference/fetchOSD.md):
  get soil morphology and many soil series level summaries for one or
  more soil series names
- [`OSDquery()`](http://ncss-tech.github.io/soilDB/reference/OSDquery.md):
  search the OSD records using the postgresql full text query syntax
- [`siblings()`](http://ncss-tech.github.io/soilDB/reference/siblings.md):
  get names and basic information about soil series that co-occurr
  within soil map units containing a given series

Spatial data:

- [`seriesExtent()`](http://ncss-tech.github.io/soilDB/reference/seriesExtent.md):
  get vector or raster representations of were soil series have been
  used in SSURGO
- [`taxaExtent()`](http://ncss-tech.github.io/soilDB/reference/taxaExtent.md):
  get raster representations of were taxa and formative elements (Soil
  Taxonomy) have been used in SSURGO

WCS:

- [`mukey.wcs()`](http://ncss-tech.github.io/soilDB/reference/mukey.wcs.md):
  get raster data describing map unit keys, from a variety of sources,
  by bounding-box
- [`ISSR800.wcs()`](http://ncss-tech.github.io/soilDB/reference/ISSR800.wcs.md):
  get raster data describing generalized patterns in soil property and
  condition, by bounding-box
- [`soilColor.wcs()`](http://ncss-tech.github.io/soilDB/reference/soilColor.wcs.md):
  get raster soil color maps by bounding-box

The
[`fetchOSD()`](http://ncss-tech.github.io/soilDB/reference/fetchOSD.md)
function, when specified with the `extended = TRUE` argument, will
include the last updated date for curated sources.

``` r
# typical invocation
library(soilDB)
library(aqp)

x <- fetchOSD(c('lucy'), extended = TRUE)

# access metadata
x$soilweb.metadata
```

An abbreviated example of these metadata will look like this:

| product                      | last_update |
|:-----------------------------|:------------|
| ISSR800                      | 2024-10-30  |
| KSSL snapshot                | 2025-01-21  |
| MLRA membership              | 2025-10-07  |
| OSD fulltext                 | 2026-01-02  |
| OSD morphology               | 2026-01-02  |
| SC database                  | 2026-01-02  |
| series climate summary       | 2025-10-08  |
| series-ESID cross-tabulation | 2025-10-07  |
| SSURGO geomorphology         | 2025-10-06  |
| SSURGO geomorphon            | 2026-01-11  |
| SSURGO NCCPI Stats           | 2025-10-19  |
| SSURGO parent material       | 2025-10-06  |
| series extent                | 2025-10-04  |
| taxa extent                  | 2025-10-21  |

Details about these data sources are provided below.

#### Functions with Questionable Future

Some functions represent temporary solutions to data delivery problems
that made sense in the past.
[`fetchKSSL()`](http://ncss-tech.github.io/soilDB/reference/fetchKSSL.md)
relies on an older snapshot of the NCSS LDM (2020),
[`fetchRaCA()`](http://ncss-tech.github.io/soilDB/reference/fetchRaCA.md)
relies on a pre-release of the current RaCA database, and
[`SoilWeb_spatial_query()`](http://ncss-tech.github.io/soilDB/reference/SoilWeb_spatial_query.md)
has been superseded by soilDB functions which interface with Soil Data
Access (see
[`SDA_query()`](https://ncss-tech.github.io/soilDB/reference/SDA_query.html)
and
[`SDA_spatialQuery`](https://ncss-tech.github.io/soilDB/reference/SDA_spatialQuery.html)).

- [`fetchKSSL()`](http://ncss-tech.github.io/soilDB/reference/fetchKSSL.md):
  get soil characterization and (limited) morphology data (soil color,
  structure, redoximorphic features)
- [`fetchRaCA()`](http://ncss-tech.github.io/soilDB/reference/fetchRaCA.md):
  get records associated with the RaCA collection
- [`SoilWeb_spatial_query()`](http://ncss-tech.github.io/soilDB/reference/SoilWeb_spatial_query.md):
  simple interface to spatial intersection between SSURGO polygons and
  user supplied bounding-box

There are currently no replacements for full functionality provided by
[`fetchKSSL()`](http://ncss-tech.github.io/soilDB/reference/fetchKSSL.md)
and
[`fetchRaCA()`](http://ncss-tech.github.io/soilDB/reference/fetchRaCA.md).
See
[`fetchLDM()`](https://ncss-tech.github.io/soilDB/reference/fetchLDM.html)
for a modern approach to downloading NCSS LDM snapshot data.

### Related Web Applications

The
[`OSDquery()`](https://ncss-tech.github.io/soilDB/reference/OSDquery.html)
function provides a convenient interface to the API behind the SoilWeb
OSD Search applications:

- <https://soilmap4-1.lawr.ucdavis.edu/osd-search/index.php>
- <https://casoilresource.lawr.ucdavis.edu/osd-search/search-entire-osd.php>

The web-based version of this search is limited to 100 records but
[`OSDquery()`](http://ncss-tech.github.io/soilDB/reference/OSDquery.md)
has no record limit.

The
[`seriesExtent()`](https://ncss-tech.github.io/soilDB/reference/seriesExtent.html)
function provides an interface to the data behind the [SoilWeb Series
Extent Explorer](https://casoilresource.lawr.ucdavis.edu/see/) (SEE)
application. The SEE website includes a short description of how the
source data were created.

The
[`taxaExtent()`](https://ncss-tech.github.io/soilDB/reference/taxaExtent.html)
function provides an interface to the data behind the [SoilWeb Soil
Taxonomy Explorer](https://casoilresource.lawr.ucdavis.edu/ste/) (STE)
application. the STE website includes a short description of how the
source data were created.

### Relevant Tutorials

- [Querying Soil Series
  Data](https://ncss-tech.github.io/AQP/soilDB/soil-series-query-functions.html)
- [Soil Series Co-Occurrence
  Data](https://ncss-tech.github.io/AQP/soilDB/siblings.html)
- [Competing Soil
  Series](https://ncss-tech.github.io/AQP/soilDB/competing-series.html)
- [Exploring Geomorphic
  Summaries](https://ncss-tech.github.io/AQP/soilDB/exploring-geomorph-summary.html)
- [What does a subgroup look
  like?](https://ncss-tech.github.io/AQP/soilDB/subgroup-series.html)
- [Map Unit Key Web Coverage
  Service](https://ncss-tech.github.io/AQP/soilDB/WCS-demonstration-01.html)
- [Investigating Soil Series
  Extent](https://ncss-tech.github.io/AQP/soilDB/series-extent.html)
- [Geographic Extent of Soil
  Taxa](https://ncss-tech.github.io/AQP/soilDB/taxa-extent.html)

## SoilWeb Curated Data Sources

### SC and OSD Derivatives

Competing (same family classification) soil series information are
derived from the Soil Classification database. Geographically associated
soils are derived from the OSD records. These data are available via
[`fetchOSD()`](http://ncss-tech.github.io/soilDB/reference/fetchOSD.md).

Competing soil series.

``` r
# series names listed in "competing" have the family classification as "series"
head(x$competing)
```

| series | competing    | family                                               |
|:-------|:-------------|:-----------------------------------------------------|
| MIAMI  | VAUGHNSVILLE | fine-loamy, mixed, active, mesic oxyaquic hapludalfs |
| MIAMI  | ADAMSTOWN    | fine-loamy, mixed, active, mesic oxyaquic hapludalfs |
| MIAMI  | BEECH        | fine-loamy, mixed, active, mesic oxyaquic hapludalfs |
| MIAMI  | BLAKESLEE    | fine-loamy, mixed, active, mesic oxyaquic hapludalfs |
| MIAMI  | CAZENOVIA    | fine-loamy, mixed, active, mesic oxyaquic hapludalfs |
| MIAMI  | EL DARA      | fine-loamy, mixed, active, mesic oxyaquic hapludalfs |

Geographically associated series. Series in the same region may have
several geographically associated series in common, and can be modeled
using [directed graphs](https://en.wikipedia.org/wiki/Directed_graph).

``` r
# series names listed in "gas" are geographically associated with "series"
head(x$geog_assoc_soils)
```

| series | gas        |
|:-------|:-----------|
| LUCY   | BAMA       |
| LUCY   | DOTHAN     |
| LUCY   | EUSTIS     |
| LUCY   | FUQUAY     |
| LUCY   | ORANGEBURG |
| LUCY   | RUSTON     |

### SSURGO Derivatives

SSURGO components are often named for soil series. Soil series summaries
derived from SSURGO are coordinated using a normalized form of component
name and soil series:

- names are converted to upper case
- component names are stripped of modifiers such as “variant” and
  “family”

See the [Soil Survey
Manual](https://www.nrcs.usda.gov/resources/guides-and-instructions/soil-survey-manual)
for more complete definitions of “map unit”, “component”, and “soil
series”.

The [Querying Soil Series
Data](https://ncss-tech.github.io/AQP/soilDB/soil-series-query-functions.html)
tutorial contains additional, relevant examples.

#### MLRA

Derived from the spatial intersection between MLRA and SSURGO polygons,
with area computed on the ellipsoid from geographic coordinates.
Membership values are area proportions by soil series.

MLRA “membership” for the
[LUCY](https://casoilresource.lawr.ucdavis.edu/sde/?series=lucy#osd)
soil series.

``` r
.mlra <- x$mlra[x$mlra$series == 'LUCY', ]
.mlra[order(.mlra$membership, decreasing = TRUE), ]
```

| series | mlra | area_ac | membership |
|:-------|:-----|--------:|-----------:|
| LUCY   | 133A |  431842 |       0.51 |
| LUCY   | 133C |  295749 |       0.35 |
| LUCY   | 137  |   71537 |       0.09 |
| LUCY   | 133B |   20082 |       0.02 |
| LUCY   | 135A |    7913 |       0.01 |
| LUCY   | 134  |    6952 |       0.01 |
| LUCY   | 153A |    3547 |       0.00 |
| LUCY   | 138  |    3663 |       0.00 |
| LUCY   | 136  |    2041 |       0.00 |
| LUCY   | 131B |     661 |       0.00 |
| LUCY   | 152A |    1185 |       0.00 |

Soil series can be found in multiple MLRA, therefore MLRA membership can
be modeled using [directed
graphs](https://en.wikipedia.org/wiki/Directed_graph).

#### Geomorphic Summaries

Geomorphic summaries are computed from SSURGO component “geomorphology”
tables. These represent a cross-tabulation of soil series name x
geomorphic position in several landform and surface shape description
systems. These systems are defined in the [Field Book for Describing and
Sampling
Soils](https://www.nrcs.usda.gov/resources/guides-and-instructions/field-book-for-describing-and-sampling-soils).

There are several associated functions in the
[sharpshootR](https://github.com/ncss-tech/sharpshootR/) package for
visualizing these summaries
(e.g. [`vizHillslopePosition()`](http://ncss-tech.github.io/sharpshootR/reference/vizHillslopePosition.md)).

- hillslope position (2D)
- geomorphic component: hills (3D)
- geomorphic component: mountains (3D)
- geomorphic component: terrace (3D)
- geomorphic component: flats (3D)
- surface curvature across-slope
- surface curvature down-slope

Note that the `n` column in each table is the number of component
geomorphic data records associated with each soil series. It is possible
for a single component to have multiple geomorphic positions defined.

``` r
# hillslope position
head(x$hillpos)
# geomorphic component: hills
head(x$geomcomp)
# geomorphic component: mountains
head(x$mtnpos)
# geomorphic component: terraces
head(x$terrace)
# geomorphic component: flats
head(x$flats)
# surface curvature across slope
head(x$shape_across)
# surface curvature down slope
head(x$shape_down)
```

| series  | Toeslope | Footslope | Backslope | Shoulder | Summit |    n | shannon_entropy |
|:--------|---------:|----------:|----------:|---------:|-------:|-----:|----------------:|
| LUCY    |     0.00 |      0.00 |      0.31 |     0.34 |   0.35 |  541 |            1.61 |
| MIAMI   |     0.03 |      0.06 |      0.41 |     0.37 |   0.13 | 1968 |            1.85 |
| MUSICK  |     0.00 |      0.00 |      0.75 |     0.19 |   0.05 |   77 |            0.99 |
| PIERRE  |     0.00 |      0.14 |      0.73 |     0.01 |   0.12 |  432 |            1.16 |
| TRISTAN |     0.00 |      0.00 |      1.00 |     0.00 |   0.00 |   14 |            0.00 |

hillslope position

| series  | Interfluve | Crest | Head Slope | Nose Slope | Side Slope | Base Slope |    n | shannon_entropy |
|:--------|-----------:|------:|-----------:|-----------:|-----------:|-----------:|-----:|----------------:|
| LUCY    |       0.68 |  0.04 |       0.00 |       0.00 |       0.27 |       0.00 |  427 |            1.13 |
| MIAMI   |       0.06 |  0.12 |       0.12 |       0.14 |       0.51 |       0.05 | 1419 |            2.08 |
| MUSICK  |       0.11 |  0.00 |       0.00 |       0.00 |       0.89 |       0.00 |   19 |            0.49 |
| PIERRE  |       0.05 |  0.00 |       0.03 |       0.08 |       0.84 |       0.00 |  114 |            0.86 |
| TRISTAN |       0.00 |  0.00 |       0.17 |       0.00 |       0.83 |       0.00 |   12 |            0.65 |

geomorphic component: hills

| series  | Mountaintop | Mountainflank | Upper third of mountainflank | Center third of mountainflank | Lower third of mountainflank | Mountainbase |   n | shannon_entropy |
|:--------|------------:|--------------:|-----------------------------:|------------------------------:|-----------------------------:|-------------:|----:|----------------:|
| MUSICK  |        0.26 |          0.72 |                         0.02 |                             0 |                            0 |            0 |  57 |            0.95 |
| TRISTAN |        0.00 |          1.00 |                         0.00 |                             0 |                            0 |            0 |   8 |            0.00 |

geomorphic component: mountains

| series | Tread | Riser |   n | shannon_entropy |
|:-------|------:|------:|----:|----------------:|
| LUCY   |   0.8 |   0.2 |  30 |            0.72 |
| MIAMI  |   1.0 |   0.0 |   2 |            0.00 |
| PIERRE |   0.5 |   0.5 |   4 |            1.00 |

geomorphic component: terraces

| series | Dip | Talf | Flat | Rise |   n | shannon_entropy |
|:-------|----:|-----:|-----:|-----:|----:|----------------:|
| LUCY   |   0 | 0.00 |    0 | 1.00 | 111 |            0.00 |
| MIAMI  |   0 | 0.02 |    0 | 0.98 | 126 |            0.16 |

geomorphic component: flats

| series  | Concave | Linear | Convex | Complex | Undulating |   n | shannon_entropy |
|:--------|--------:|-------:|-------:|--------:|-----------:|----:|----------------:|
| LUCY    |    0.03 |   0.40 |   0.57 |       0 |          0 | 373 |            1.13 |
| MIAMI   |    0.04 |   0.71 |   0.26 |       0 |          0 | 979 |            1.03 |
| MUSICK  |    0.23 |   0.45 |   0.32 |       0 |          0 |  60 |            1.53 |
| PIERRE  |    0.00 |   0.95 |   0.05 |       0 |          0 | 457 |            0.31 |
| TRISTAN |    0.44 |   0.31 |   0.25 |       0 |          0 |  32 |            1.55 |

surface curvature across-slope

| series  | Concave | Linear | Convex | Complex | Undulating |   n | shannon_entropy |
|:--------|--------:|-------:|-------:|--------:|-----------:|----:|----------------:|
| LUCY    |    0.03 |   0.09 |   0.87 |       0 |          0 | 373 |            0.65 |
| MIAMI   |    0.02 |   0.43 |   0.55 |       0 |          0 | 979 |            1.10 |
| MUSICK  |    0.45 |   0.50 |   0.05 |       0 |          0 |  60 |            1.23 |
| PIERRE  |    0.00 |   0.70 |   0.29 |       0 |          0 | 457 |            0.91 |
| TRISTAN |    0.34 |   0.62 |   0.03 |       0 |          0 |  32 |            1.11 |

surface curvature down-slope

#### Siblings

SoilWeb defines the term “siblings” as those components or soil series
that co-occur within map units. The
[`siblings()`](http://ncss-tech.github.io/soilDB/reference/siblings.md)
function returns siblings for a single soil series, with a tabulation of
how many times each sibling shares a common map unit.

Siblings of the PIERRE soil series, limited to just major components.
The `n` column describes how many map units are shared between a sibling
and the PIERRE series.

``` r
sib <- siblings('PIERRE', only.major = TRUE)
head(sib$sib)
```

| series | sibling  | majcompflag |   n |
|:-------|:---------|:------------|----:|
| PIERRE | Samsil   | TRUE        |  25 |
| PIERRE | Fairburn | TRUE        |  13 |
| PIERRE | Lismas   | TRUE        |  11 |
| PIERRE | Ucross   | TRUE        |   9 |
| PIERRE | Grummit  | TRUE        |   4 |
| PIERRE | Hisle    | TRUE        |   4 |
| PIERRE | Promise  | TRUE        |   2 |
| PIERRE | Kyle     | TRUE        |   2 |
| PIERRE | Marias   | TRUE        |   1 |
| PIERRE | Samday   | TRUE        |   1 |
| PIERRE | Winler   | TRUE        |   1 |
| PIERRE | Vananda  | TRUE        |   1 |

#### Other

Percentiles of the [National Commodity Crop Productivity
Index](https://www.nrcs.usda.gov/sites/default/files/2023-01/NCCPI-User-Guide.pdf)
are computed from SSURGO component records, by soil series name. These
include both irrigated and non-irrigated versions of the NCCPI.

``` r
head(x$NCCPI)
```

| series  |   n | nccpi_irrigated_q01 | nccpi_irrigated_q05 | nccpi_irrigated_q25 | nccpi_irrigated_q50 | nccpi_irrigated_q75 | nccpi_irrigated_q95 | nccpi_irrigated_q99 | nccpi_q01 | nccpi_q05 | nccpi_q25 | nccpi_q50 | nccpi_q75 | nccpi_q95 | nccpi_q99 |
|:--------|----:|--------------------:|--------------------:|--------------------:|--------------------:|--------------------:|--------------------:|--------------------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|
| LUCY    | 341 |                0.43 |                0.58 |                0.73 |                0.80 |                0.82 |                0.84 |                0.86 |      0.09 |      0.31 |      0.41 |      0.50 |      0.54 |      0.58 |      0.61 |
| MIAMI   | 731 |                  NA |                  NA |                  NA |                  NA |                  NA |                  NA |                  NA |      0.15 |      0.35 |      0.55 |      0.62 |      0.67 |      0.79 |      0.83 |
| MUSICK  | 103 |                0.29 |                0.37 |                0.77 |                0.82 |                0.90 |                0.99 |                0.99 |      0.11 |      0.14 |      0.43 |      0.64 |      0.75 |      0.84 |      0.84 |
| PIERRE  | 295 |                0.29 |                0.31 |                0.41 |                0.43 |                0.51 |                0.52 |                0.58 |      0.06 |      0.09 |      0.19 |      0.26 |      0.29 |      0.33 |      0.35 |
| TRISTAN |  32 |                0.32 |                0.32 |                0.32 |                0.32 |                0.32 |                0.32 |                0.32 |      0.00 |      0.00 |      0.02 |      0.02 |      0.02 |      0.08 |      0.09 |

Ecological classification membership are computed from map unit polygon
area and component percentages.

``` r
head(x$ecoclassid)
```

| series | ecoclassid  | n_components | area_ac | proportion |
|:-------|:------------|-------------:|--------:|-----------:|
| LUCY   | F153AY030NC |           15 |   25899 |       0.51 |
| LUCY   | F133BY006TX |           11 |   20631 |       0.41 |
| LUCY   | F137XY040SC |            1 |    2279 |       0.05 |
| LUCY   | F137XY050GA |            2 |    1587 |       0.03 |
| MIAMI  | F098XA015MI |           82 |   41416 |       0.04 |
| MIAMI  | F098XA022MI |           59 |   33897 |       0.03 |

#### Parent Material Summaries

Parent material kind and origin, tabulated by soil series name. The `n`
column is the number of component parent material records associated
with a specific parent material kind or origin. The `total` column is
the total number of component parent material records by soil series.
The final column, `P`, is the associated proportion.

``` r
head(x$pmkind)
head(x$pmorigin)
```

| series | pmkind                |   n | total |    P |
|:-------|:----------------------|----:|------:|-----:|
| LUCY   | Marine deposits       | 180 |   195 | 0.92 |
| LUCY   | Fluviomarine deposits |  13 |   195 | 0.07 |
| LUCY   | Alluvium              |   2 |   195 | 0.01 |
| MIAMI  | Till                  | 537 |   831 | 0.65 |
| MIAMI  | Loess                 | 267 |   831 | 0.32 |
| MIAMI  | Outwash               |  14 |   831 | 0.02 |

| series | pmorigin               |   n | total |    P |
|:-------|:-----------------------|----:|------:|-----:|
| LUCY   | Sedimentary rock       |  29 |    29 | 1.00 |
| MIAMI  | Limestone and dolomite |  54 |   102 | 0.53 |
| MIAMI  | Quartzite              |  48 |   102 | 0.47 |
| MUSICK | Diorite                |  40 |    89 | 0.45 |
| MUSICK | Granodiorite           |  24 |    89 | 0.27 |
| MUSICK | Granite                |  19 |    89 | 0.21 |

### Series and Taxa Extent

Soil series extent vector data are available for all areas where SSURGO
has been completed. Vector extents are described as polygons which
generalize the underlying SSURGO map unit polygons (EPSG:4326). Soil
series extent raster data are available as 800m grids (EPSG:5070) within
CONUS only. Taxonomic classes and formative elements are available as
800m grids (EPSG:5070) within CONUS only. Grids are

### Web Coverage Services

Web Coverage Services (WCS) provided by SoilWeb are still an
experimental interface to snapshots of authoritative soil survey and
derived data sources. The update cycle is slower than the typical SSURGO
refresh. All WCS requests return data as compressed GeoTIFF (EPSG:5070).

### Climate Data and Derivatives

These maps are derived from the daily, 800m resolution, PRISM data
spanning 1981–2010.

- Mean annual air temperature (deg. C), derived from daily minimum and
  maximum temperatures.
- Mean accumulated annual precipitation (mm), derived from daily totals.
- Mean monthly temperature (deg. C), derived from daily minimum and
  maximum temperatures.
- Mean accumulated monthly precipitation (mm), derived from daily
  totals.
- Estimated monthly potential evapotranspiration, [Thornthwaite,
  1948](https://en.wikipedia.org/wiki/Potential_evapotranspiration#Thornthwaite_equation_(1948))

Percentiles of each variable are computed by soil series, from a
sampling of one point per SSURGO map unit polygon.

An example of annual and monthly climate percentiles.

``` r
head(x$climate.annual)

head(x$climate.monthly)
```

|     | series | climate_var                             | minimum |  q01 |  q05 |  q25 |  q50 |  q75 |  q95 |  q99 | maximum |     n |
|:----|:-------|:----------------------------------------|--------:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|--------:|------:|
| 2   | LUCY   | Effective Precipitation (mm)            |     148 |  198 |  212 |  238 |  320 |  421 |  597 |  687 |     754 | 32459 |
| 3   | LUCY   | Frost-Free Days                         |     197 |  219 |  233 |  239 |  245 |  256 |  273 |  283 |     299 | 32459 |
| 4   | LUCY   | Mean Annual Air Temperature (degrees C) |      14 |   16 |   17 |   18 |   18 |   19 |   20 |   20 |      20 | 32459 |
| 5   | LUCY   | Mean Annual Precipitation (mm)          |    1071 | 1113 | 1124 | 1160 | 1268 | 1380 | 1577 | 1723 |    1786 | 32459 |
| 6   | LUCY   | Growing Degree Days (degrees C)         |    2465 | 2930 | 3135 | 3258 | 3351 | 3535 | 3710 | 3760 |    3923 | 32459 |
| 7   | LUCY   | Fraction of Annual PPT as Rain          |      96 |   98 |   99 |   99 |  100 |  100 |  100 |  100 |     100 | 32459 |

|     | series | climate_var | minimum | q01 | q05 | q25 | q50 | q75 | q95 | q99 | maximum |     n | month | variable           |
|:----|:-------|:------------|--------:|----:|----:|----:|----:|----:|----:|----:|--------:|------:|:------|:-------------------|
| 1   | LUCY   | ppt1        |      85 |  92 |  98 | 109 | 119 | 128 | 142 | 152 |     160 | 32459 | 1     | Precipitation (mm) |
| 10  | LUCY   | ppt2        |      72 |  82 |  88 | 102 | 113 | 123 | 134 | 143 |     156 | 32459 | 2     | Precipitation (mm) |
| 11  | LUCY   | ppt3        |      88 |  96 |  99 | 117 | 133 | 145 | 156 | 162 |     170 | 32459 | 3     | Precipitation (mm) |
| 12  | LUCY   | ppt4        |      63 |  69 |  72 |  78 |  87 |  99 | 117 | 124 |     134 | 32459 | 4     | Precipitation (mm) |
| 13  | LUCY   | ppt5        |      56 |  61 |  65 |  72 |  79 |  92 | 115 | 132 |     143 | 32459 | 5     | Precipitation (mm) |
| 14  | LUCY   | ppt6        |      82 |  92 |  99 | 109 | 122 | 135 | 162 | 179 |     197 | 32459 | 6     | Precipitation (mm) |

#### Frost-Free Period

Number of days in the 50%, 80%, and 90% probability frost-free period,
derived from daily minimum temperatures greater than 0 degrees C.

These maps are based on 50/80/90 percent probability estimates for the
last spring frost and first fall frost (day of year). See the related
[algorithm
documentation](http://ncss-tech.github.io/AQP/sharpshootR/FFD-estimates.md)
for details.

Values have been cross-checked with 300+ weather stations in CA.

**Linear Regression Model**

     ols(formula = ffd.50 ~ prism_ffd, data = z)
     

[TABLE]

Residuals

          Min       1Q   Median       3Q      Max 
     -278.344  -16.875    2.436   14.323  274.604 
     

|           |       β |   S.E. |   *t* | Pr(\>\|*t*\|) |
|:----------|--------:|-------:|------:|--------------:|
| Intercept | 15.1397 | 5.3455 |  2.83 |        0.0049 |
| prism_ffd |  0.9407 | 0.0261 | 35.98 |      \<0.0001 |

#### Frost-Free Days

Percentiles of frost-free days (FFD) at the 50% probability threshold.

#### Design Freeze Index

- number of degree days below 0 deg C, 90th percentile

From *NSSH Part 618.33 Frost Action, Potential*:

> Part 618, Subpart B, Exhibits, Section 618.85 is a map that shows the
> design freezing index values in the continental United States. The
> values **are the number of degree days below 0 deg C for the coldest
> year in a period of 10 years** . The values indicate duration and
> intensity of freezing temperatures. The 250 isoline is the approximate
> boundary below which frost action ceases to be a problem.

Methods:

- using units of degrees Celsius, and daily average air temperature
  ($Tavg$)
- freezing degree days for a single year:
  $FI = sum\left( abs\left( min(0,Tavg) \right) \right)$
- design freezing index, over 30 year record: $DFI = Q90(FI)$ where
  **FI** is the stack of annual FI

Notes:

- There is a fairly large difference in where the 250 DFI isoline falls,
  depending on the temperature units.
- The 90th percentile of **FI** seems to track the notion of “coldest
  year in 10 years”.
- The “average of 3 coldest years in 30” method gives different results,
  but spatial patterns are the same.
- Related
  [conversation](https://github.com/ncss-tech/soilReports/issues/84) on
  the calculation and interpretation.

#### Growing Degree Days (C)

Mean (Celsius) growing degree days, derived from the 800m PRISM daily
minimum/maximum temperature data over the interval of 1981–2010.

Calculation reference:
[http://agron-www.agron.iastate.edu/courses/Agron541/classes/541/lesson02b/2b.1.1.html](http://agron-www.agron.iastate.edu/courses/Agron541/classes/541/lesson02b/2b.1.1.md)

$$GDD_{i} = \left\lbrack min\left( T_{max},upper_{threshold} \right) + max\left( Tmin,lower_{threshold} \right)/2 \right\rbrack - T_{base}$$

$$GDD_{i} = max\left( GDD_{i},0 \right)$$

#### Effective Precipitation

Annual sum of monthly (total) precipitation - monthly (estimated)
evapotranspiration, averaged over the interval of 1981–2010. Potential
evapotranspiration (PET) estimated via [Thornthwaite’s method of
1948](https://en.wikipedia.org/wiki/Potential_evaporation). Input
sources included:

- 800m resolution, monthly, total precipitation (PRISM group)
- 800m resolution, monthly, mean air temperature (PRISM group)

Processing in GRASS GIS.

#### Fraction of Precipitation as Rain

This map contains estimates of the fraction of total (annual)
precipitation as rain, derived from 800m daily PRISM Tmax and PPT grids
(1981–2010). Calculations were performed using GRASS GIS, with methods
and estimated parameters of the conditional snow probability function
from Rajagopal and Harpold (2016).

Partition PPT into snow/rain:

$$rain = PPT - snow$$

$$snow = PPT*Pr(snow)$$

compute $Pr(snow)$ as a function of $Tmax$ using [exponential identity
for hyperbolic tangent
function](https://en.wikipedia.org/wiki/Hyperbolic_function#Standard_analytic_expressions):

Evaluate conditional probability (fraction) of snow on a daily basis:

$$Pr(snow) = a*\left( tanh\left( b*(Tmax - c) \right) - d \right)$$

a:-0.5, b:0.21, c:0.5, d:1

$$tanh(x) = \left( 1 - exp( - 2*x) \right)/\left( 1 + exp( - 2*x) \right)$$

$$Pr(snow) = - 0.5*\left( \left( 1 - exp\left( - 2*\left( 0.21*(Tmax - 0.5) \right) \right) \right)/\left( 1 + exp\left( - 2*\left( 0.21*(Tmax - 0.5) \right) \right) \right) - 1 \right)$$

$$rain = PPT - \left( PPT*Pr(snow) \right)$$

For each year($i$):

$$rainfraction_{i} = sum\left( rain_{i} \right)/sum\left( PPT_{i} \right)$$

Percentages have been converted to integers ranging from 0 to 100.

Rajagopal, S. and A.A. Harpold. 2016. Testing and Improving Temperature
Thresholds for Snow and Rain Prediction in the Western United States.
Journal of the American Water Resources Association, 52: 1142-1154.

### DEM Derivatives

#### Geomorphons

A cross-tabulation of geomorphon by soil series name was computed from
the current gSSURGO (30m) grid and a 30m grid of geomorphons. Currently,
these data are only available within CONUS.

These maps were generated using the [r.geomorphon GRASS GIS
module](https://grass.osgeo.org/grass75/manuals/r.geomorphon.html), with
the following parameters:

`r.geomorphon --o dem=elev30_int forms=forms30 search=75 skip=5 flat=1.718`

The source DEM was a 10m / 30m resolution compilation of USGS NED data,
rounded to integers. The “flat” threshold (1.718 deg) is based on a 3%
slope break.

[Jasiewicz, J., Stepinski, T., 2013, Geomorphons - a pattern recognition
approach to classification and mapping of landforms, Geomorphology,
vol. 182,
147-156.](http://www.sciencedirect.com/science/article/pii/S0169555X12005028)

Proportions are weighted by total soil series area (within CONUS) as
informed by the component name and associated component percentage.

``` r
head(x$geomorphons)
```

| series  | flat | summit | ridge | shoulder | spur | slope | hollow | footslope | valley | depression | shannon_entropy |
|:--------|-----:|-------:|------:|---------:|-----:|------:|-------:|----------:|-------:|-----------:|----------------:|
| LUCY    | 0.39 |   0.01 |  0.14 |     0.15 | 0.04 |  0.09 |   0.02 |      0.09 |   0.07 |       0.00 |            2.61 |
| MIAMI   | 0.74 |   0.00 |  0.05 |     0.11 | 0.01 |  0.04 |   0.01 |      0.04 |   0.01 |       0.00 |            1.46 |
| MUSICK  | 0.00 |   0.03 |  0.17 |     0.00 | 0.21 |  0.24 |   0.16 |      0.00 |   0.16 |       0.02 |            2.55 |
| PIERRE  | 0.24 |   0.01 |  0.12 |     0.14 | 0.05 |  0.16 |   0.04 |      0.14 |   0.10 |       0.00 |            2.90 |
| TRISTAN | 0.00 |   0.03 |  0.16 |     0.00 | 0.23 |  0.25 |   0.16 |      0.00 |   0.15 |       0.01 |            2.48 |

geomorphon summary
