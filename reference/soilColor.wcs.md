# Get 30m or 270m gridded soil soil color data from SoilWeb Web Coverage Service (WCS)

Moist soil colors, 2022.

## Usage

``` r
soilColor.wcs(aoi, var, res = 270, quiet = FALSE)
```

## Arguments

- aoi:

  area of interest (AOI) defined using a `Spatial*`, `RasterLayer`,
  `sf`, `sfc` or `bbox` object, OR a `list`, see details

- var:

  soil color grid name (case insensitive), see details

- res:

  grid resolution, units of meters, typically '270', or '30', depending
  on `var`. See details.

- quiet:

  logical, passed to
  [`curl::curl_download`](https://jeroen.r-universe.dev/curl/reference/curl_download.html)
  to enable / suppress URL and progress bar for download.

## Value

A `SpatRaster` (or `RasterLayer`) object containing indexed map unit
keys and associated raster attribute table or a try-error if request
fails. By default, spatial classes from the `terra` package are
returned. If the input object class is from the `raster` or `sp`
packages a `RasterLayer` is returned.

## Details

`aoi` should be specified as a `SpatRaster`, `Spatial*`, `RasterLayer`,
`SpatRaster`/`SpatVector`, `sf`, `sfc`, or `bbox` object or a `list`
containing:

- `aoi`:

  bounding-box specified as (xmin, ymin, xmax, ymax) e.g. c(-114.16,
  47.65, -114.08, 47.68)

- `crs`:

  coordinate reference system of BBOX, e.g. 'OGC:CRS84' (EPSG:4326,
  WGS84 Longitude/Latitude)

The WCS query is parameterized using a rectangular extent derived from
the above AOI specification, after conversion to the native CRS
(EPSG:5070) of the soil color grids.

Variables available from this WCS can be queried using
`WCS_details(wcs = 'soilColor')`. The full resolution version of the
soil color grids use a `hr` suffix, e.g. 'sc025cm_hr'.

## Author

D.E. Beaudette and A.G. Brown

## Examples

``` r
if (FALSE) { # \dontrun{
library(terra)

# see WCS_details() for variable options
WCS_details(wcs = 'soilColor')

# moist soil color at 25cm, 270m version
res <- soilColor.wcs(list(aoi = c(-116, 35, -115.5, 35.5), crs = "EPSG:4326"), 
                   var = 'sc025cm', res = 270)

# note colors and other metadata are stored
# in raster attribute table
plot(res, col = cats(res)[[1]]$col, axes = FALSE, legend = FALSE)
} # }
```
