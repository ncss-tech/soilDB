# Get 800m gridded soil properties from SoilWeb ISSR800 Web Coverage Service (WCS)

Intermediate-scale gridded (800m) soil property and interpretation maps
from aggregated SSURGO and STATSGO data. These maps were developed by
USDA-NRCS-SPSD staff in collaboration with UCD-LAWR. Originally for
educational use and [interactive thematic
maps](https://casoilresource.lawr.ucdavis.edu/soil-properties/), these
data are a suitable alternative to gridded STATSGO-derived thematic soil
maps. The full size grids can be [downloaded
here](https://casoilresource.lawr.ucdavis.edu/soil-properties/download.php).

## Usage

``` r
ISSR800.wcs(aoi, var, res = 800, quiet = FALSE)
```

## Arguments

- aoi:

  area of interest (AOI) defined using a terra pakcage objects
  (`SpatRaster`, `SpatVector`, `ext`), sf objects (`sf`, `sfc`), raster
  package object (`RasterLayer`, `bbox`), sp package `Spatial*`, or a
  `list`, see details

- var:

  ISSR-800 grid name (case insensitive), see details

- res:

  grid resolution, units of meters. The native resolution of ISSR800
  grids (this WCS) is 800m.

- quiet:

  logical, passed to
  [`curl::curl_download`](https://jeroen.r-universe.dev/curl/reference/curl_download.html)
  to enable / suppress URL and progress bar for download.

## Value

`SpatRaster` (or `RasterLayer`) object, or `try-error` if the WCS
request fails.

## Details

When specified as a `list`, `aoi` should contain:

- `aoi`: bounding-box specified as (xmin, ymin, xmax, ymax) e.g.
  c(-114.16, 47.65, -114.08, 47.68)

- `crs`: coordinate reference system of BBOX, e.g. 'OGC:CRS84'
  (EPSG:4326, WGS84 Longitude/Latitude)

The WCS query is parameterized using a rectangular extent derived from
the above AOI specification, after conversion to the native CRS
(EPSG:5070) of the ISSR800 grids.

Variables available from this WCS can be queried using
`WCS_details(wcs = 'ISSR800')`.

|  |  |  |
|----|----|----|
| var | crs | description |
| caco3_kg_sq_m | EPSG:5070 | Total CaCO3 (kg/m^2) |
| cec_025cm | EPSG:5070 | CEC at pH 7 0-25cm depth (cmol[+](https://rdrr.io/r/base/Arithmetic.html)/kg) |
| cec_050cm | EPSG:5070 | CEC at pH 7 0-50cm depth (cmol[+](https://rdrr.io/r/base/Arithmetic.html)/kg) |
| cec_05cm | EPSG:5070 | CEC at pH 7 0-5cm depth (cmol[+](https://rdrr.io/r/base/Arithmetic.html)/kg) |
| clay_025cm | EPSG:5070 | clay percent 0-25cm depth |
| clay_05cm | EPSG:5070 | clay percent 0-5cm depth |
| clay_2550cm | EPSG:5070 | clay percent 25-50cm depth |
| clay_3060cm | EPSG:5070 | clay percent 30-60cm depth |
| drainage_class | EPSG:5070 | Soil Drainage Class |
| ec_025cm | EPSG:5070 | EC 0-25cm depth (dS/m) |
| ec_05cm | EPSG:5070 | EC 0-5cm depth (dS/m) |
| greatgroup | EPSG:5070 | Soil Taxonomy: Greatgroup |
| hydgrp | EPSG:5070 | Hydrologic Soil Group |
| lcc_irrigated | EPSG:5070 | Land Capability Class, irrigated |
| lcc_nonirrigated | EPSG:5070 | Land Capability Class, non-irrigated |
| n_components | EPSG:5070 | Number of components per grid cell |
| n_polygons | EPSG:5070 | Number of polygons per grid cell |
| om_kg_sq_m | EPSG:5070 | Total Soil Organic Matter (kg/m^2) |
| paws | EPSG:5070 | total plant available water storage (cm water) |
| paws_025cm | EPSG:5070 | plant available water storage 0-25cm depth (cm water) |
| paws_050cm | EPSG:5070 | plant available water storage 0-50cm depth (cm water) |
| ph_025cm | EPSG:5070 | pH 1:1 H2O 0-25cm depth |
| ph_05cm | EPSG:5070 | pH 1:1 H2O 0-5cm depth |
| ph_2550cm | EPSG:5070 | pH 1:1 H2O 25-50cm depth |
| ph_3060cm | EPSG:5070 | pH 1:1 H2O 30-60cm depth |
| sand_025cm | EPSG:5070 | sand percent 0-25cm depth |
| sand_05cm | EPSG:5070 | sand percent 0-5cm depth |
| sand_2550cm | EPSG:5070 | sand percent 25-50cm depth |
| sand_3060cm | EPSG:5070 | sand percent 30-60cm depth |
| sar | EPSG:5070 | SAR, entire profile |
| series_name | EPSG:5070 | Soil Series Name |
| silt_025cm | EPSG:5070 | silt percent 0-25cm depth |
| silt_05cm | EPSG:5070 | silt percent 0-5cm depth |
| silt_2550cm | EPSG:5070 | silt percent 25-50cm depth |
| silt_3060cm | EPSG:5070 | silt percent 30-60cm depth |
| soilorder | EPSG:5070 | Soil Taxonomy: Soil Order |
| ssurgo_pct | EPSG:5070 | SSURGO data available, fraction of 800x800m grid cell |
| statsgo_pct | EPSG:5070 | STATSGO data available, fraction of 800x800m grid cell |
| str | EPSG:5070 | Soil Temperature Regime |
| suborder | EPSG:5070 | Soil Taxonomy: Suborder |
| survey_type | EPSG:5070 | Soil survey data source |
| texture_025cm | EPSG:5070 | Soil Texture Class, 0-25cm |
| texture_05cm | EPSG:5070 | Soil Texture Class, 0-5cm |
| texture_2550cm | EPSG:5070 | Soil Texture Class, 25-50cm |
| weg | EPSG:5070 | Wind Erodibility Group |
| wei | EPSG:5070 | Wind Erodibility Index |

## Note

There are still some issues to be resolved related to the encoding of NA
Variables with a natural zero (e.g. SAR) have 0 set to NA.

## Author

D.E. Beaudette and A.G. Brown

## Examples

``` r

if (FALSE) { # \dontrun{
library(terra)

# see WCS_details() for variable options
WCS_details(wcs = 'ISSR800')

# get wind erodibility group
res <- ISSR800.wcs(list(aoi = c(-116, 35, -115.5, 35.5), crs = "EPSG:4326"), 
                   var = 'weg', res = 800)
plot(res)
} # }
```
