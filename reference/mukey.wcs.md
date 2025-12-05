# Get Map Unit Key (`mukey`) grid from SoilWeb Web Coverage Service (WCS)

Download chunks of the gNATSGO, gSSURGO, RSS, and STATSGO2 map unit key
grid via bounding-box from the SoilWeb WCS.

## Usage

``` r
mukey.wcs(
  aoi,
  db = c("gNATSGO", "gSSURGO", "RSS", "STATSGO", "PR_SSURGO", "HI_SSURGO"),
  res = 30,
  quiet = FALSE
)
```

## Arguments

- aoi:

  area of interest (AOI) defined using either a `Spatial*`,
  `RasterLayer`, `sf`, `sfc` or `bbox` object, or a `list`, see details

- db:

  name of the gridded map unit key grid to access, should be either
  'gNATSGO', 'gSSURGO', 'STATSGO', 'HI_SSURGO', or 'PR_SSURGO' (case
  insensitive)

- res:

  grid resolution, units of meters. The native resolution of gNATSGO and
  gSSURGO (this WCS) is 30m; STATSGO (this WCS) is 300m; and Raster Soil
  Surveys (RSS) are at 10m resolution. If `res` is not specified the
  native resolution of the source is used.

- quiet:

  logical, passed to
  [`curl::curl_download`](https://jeroen.r-universe.dev/curl/reference/curl_download.html)
  to enable / suppress URL and progress bar for download.

## Value

A SpatRaster (or RasterLayer) object containing indexed map unit keys
and associated raster attribute table or a try-error if request fails.
By default, spatial classes from the `terra` package are returned. If
the input object class is from the `raster` or `sp` packages a
RasterLayer is returned.

## Details

`aoi` should be specified as one of: `SpatRaster`, `Spatial*`,
`RasterLayer`, `sf`, `sfc`, `bbox` object, OR a `list` containing:

- `aoi`:

  bounding-box specified as (xmin, ymin, xmax, ymax) e.g. c(-114.16,
  47.65, -114.08, 47.68)

- `crs`:

  coordinate reference system of BBOX, e.g. 'OGC:CRS84' (EPSG:4326,
  WGS84 Longitude/Latitude)

The WCS query is parameterized using a rectangular extent derived from
the above AOI specification, after conversion to the native CRS
(EPSG:5070) of the WCS grids.

Databases available from this WCS can be queried using
`WCS_details(wcs = 'mukey')`.

## Note

The gNATSGO grid includes raster soil survey map unit keys which are not
in SDA.

## Author

D.E. Beaudette and A.G. Brown

## Examples

``` r
if (FALSE) { # \dontrun{
library(terra)

res <- mukey.wcs(list(aoi = c(-116.7400, 35.2904, -116.7072, 35.3026), crs = "EPSG:4326"),
                 db = 'gNATSGO', res = 30) 
  
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

plot(res2[['pH1to1_0to25']])
} # }
```
