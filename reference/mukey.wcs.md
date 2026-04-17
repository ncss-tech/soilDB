# Get Map Unit Key (`mukey`) grid from SoilWeb Web Coverage Service (WCS)

Download chunks of the gNATSGO, gSSURGO, OCONUS SSURGO, STATSGO2, and
RSS map unit key grid via bounding-box from the SoilWeb WCS.

## Usage

``` r
mukey.wcs(
  aoi,
  db = c("gNATSGO", "gSSURGO", "RSS", "STATSGO", "fSSURGO", "PR_SSURGO", "HI_SSURGO"),
  res = NULL,
  quiet = FALSE
)
```

## Arguments

- aoi:

  area of interest (AOI) defined using a terra pakcage objects
  (`SpatRaster`, `SpatVector`, `ext`), sf objects (`sf`, `sfc`), raster
  package object (`RasterLayer`, `bbox`), sp package `Spatial*`, or a
  `list`, see details

- db:

  character, mukey grid source, see details

- res:

  grid resolution, leave as NULL to use native grid resolution, see
  details

- quiet:

  logical, passed to
  [`curl::curl_download()`](https://jeroen.r-universe.dev/curl/reference/curl_download.html)
  to enable / suppress URL and progress bar for download.

## Value

A `SpatRaster` (or `RasterLayer`) object containing indexed map unit
keys and associated raster attribute table, or a `try-error` if the WCS
request fails. Basic metadata are encoded into the resulting
`SpatRaster`, accessible via
[`terra::metags()`](https://rspatial.github.io/terra/reference/metags.html).

## Details

When specified as a `list`, `aoi` should contain:

- `aoi`: bounding-box specified as (xmin, ymin, xmax, ymax) e.g.
  c(-114.16, 47.65, -114.08, 47.68)

- `crs`: coordinate reference system of BBOX, e.g. 'OGC:CRS84'
  (EPSG:4326, WGS84 Longitude/Latitude)

The WCS query is parameterized using a rectangular extent derived from
the above AOI specification, after conversion to the native CRS
(EPSG:5070) of the WCS grids.

Databases available from this WCS can be queried using
`WCS_details(wcs = 'mukey')`.

|           |            |                               |
|-----------|------------|-------------------------------|
| db        | crs        | description                   |
| ak_ssurgo | EPSG:3338  | AK map unit keys              |
| as_ssurgo | EPSG:4326  | AS map unit keys              |
| fssurgo   | EPSG:5070  | SSURGO/STATSGO2 map unit keys |
| gnatsgo   | EPSG:5070  | gNATSGO map unit keys         |
| gssurgo   | EPSG:5070  | gSSURGO map unit keys         |
| gu_ssurgo | EPSG:4326  | GU map unit keys              |
| hi_ssurgo | EPSG:6628  | HI map unit keys              |
| mp_ssurgo | EPSG:4326  | MP map unit keys              |
| pr_ssurgo | EPSG:32161 | PR map unit keys              |
| pw_ssurgo | EPSG:4326  | PW map unit keys              |
| rss       | EPSG:5070  | RSS map unit keys             |
| statsgo   | EPSG:5070  | STATSGO2 map unit keys        |

The `fSSURGO` database is an unofficial hybrid of gSSURGO, back-filled
with STATSGO2 data where SSURGO data are missing (e.b. denied access,
NOTCOM, large misc. areas other than water).

The RSS mukey grid is 10m resolution. CONUS, AK, HI, and PR mukey grids
are 30m resolution. AS, GU, MP, and PW use a geographic coordinate
system with a grid size of approximately 30m.

## Author

D.E. Beaudette and A.G. Brown

## Examples

``` r
if (FALSE) { # \dontrun{
library(terra)

aoi <- list(aoi = c(-116.7400, 35.2904, -116.7072, 35.3026), crs = "EPSG:4326")
res <- mukey.wcs(aoi, db = 'gNATSGO') 
  
m <- unique(values(res))

prp <- setNames(
  get_SDA_property(
    c("ph1to1h2o_r", "claytotal_r"),
    "weighted average",
    mukeys = m,
    top_depth = 0,
    bottom_depth = 25,
    include_minors = TRUE,
    miscellaneous_areas = FALSE
  )[, c("mukey", "ph1to1h2o_r", "claytotal_r")],
  c("ID",    "pH1to1_0to25", "clay_0to25")
)

levels(res) <- prp
res2 <- catalyze(res)
res2

terra::plot(res2[['pH1to1_0to25']])
} # }
```
