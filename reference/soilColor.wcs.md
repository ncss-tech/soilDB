# Access gridded soil color data from the SoilWeb Web Coverage Service (WCS)

Moist soil colors, FY2026.

## Usage

``` r
soilColor.wcs(aoi, var, res = NULL, quiet = FALSE)
```

## Arguments

- aoi:

  area of interest (AOI) defined using a terra pakcage objects
  (`SpatRaster`, `SpatVector`, `ext`), sf objects (`sf`, `sfc`), raster
  package object (`RasterLayer`, `bbox`), sp package `Spatial*`, or a
  `list`, see details

- var:

  soil color grid name (case insensitive), see details

- res:

  grid resolution, leave as NULL to use native grid resolution, see
  details

- quiet:

  logical, passed to
  [`curl::curl_download`](https://jeroen.r-universe.dev/curl/reference/curl_download.html)
  to enable / suppress URL and progress bar for download.

## Value

A `SpatRaster` (or `RasterLayer`) object containing indexed soil color
IDs, associated raster attribute table, and color table. A `try-error`
is returned if the WCS request fails.

## Details

When specified as a `list`, `aoi` should contain:

- `aoi`: bounding-box specified as (xmin, ymin, xmax, ymax) e.g.
  c(-114.16, 47.65, -114.08, 47.68)

- `crs`: coordinate reference system of BBOX, e.g. 'OGC:CRS84'
  (EPSG:4326, WGS84 Longitude/Latitude)

The WCS query is parameterized using a rectangular extent derived from
the above AOI specification, after conversion to the native CRS of the
soil color grids.

Variables available from this WCS can be queried using
`WCS_details(wcs = 'soilColor')`. The full resolution version of the
CONUS soil color grids use a `hr` suffix, e.g. 'sc025cm_hr'.

|  |  |  |
|----|----|----|
| var | crs | description |
| ak_sc005cm | EPSG:3338 | Alaska, moist soil colors, 5cm (90m resolution) |
| ak_sc010cm | EPSG:3338 | Alaska, moist soil colors, 10cm (90m resolution) |
| ak_sc015cm | EPSG:3338 | Alaska, moist soil colors, 15cm (90m resolution) |
| ak_sc025cm | EPSG:3338 | Alaska, moist soil colors, 25cm (90m resolution) |
| ak_sc050cm | EPSG:3338 | Alaska, moist soil colors, 50cm (90m resolution) |
| ak_sc075cm | EPSG:3338 | Alaska, moist soil colors, 75cm (90m resolution) |
| ak_sc100cm | EPSG:3338 | Alaska, moist soil colors, 100cm (90m resolution) |
| ak_sc125cm | EPSG:3338 | Alaska, moist soil colors, 125cm (90m resolution) |
| as_sc005cm | EPSG:4326 | American Samoa, moist soil colors, 5cm (approx. 30m resolution) |
| as_sc010cm | EPSG:4326 | American Samoa, moist soil colors, 10cm (approx. 30m resolution) |
| as_sc015cm | EPSG:4326 | American Samoa, moist soil colors, 15cm (approx. 30m resolution) |
| as_sc025cm | EPSG:4326 | American Samoa, moist soil colors, 25cm (approx. 30m resolution) |
| as_sc050cm | EPSG:4326 | American Samoa, moist soil colors, 50cm (approx. 30m resolution) |
| as_sc075cm | EPSG:4326 | American Samoa, moist soil colors, 75cm (approx. 30m resolution) |
| as_sc100cm | EPSG:4326 | American Samoa, moist soil colors, 100cm (approx. 30m resolution) |
| as_sc125cm | EPSG:4326 | American Samoa, moist soil colors, 125cm (approx. 30m resolution) |
| gu_sc005cm | EPSG:4326 | Guam, moist soil colors, 5cm (approx. 30m resolution) |
| gu_sc010cm | EPSG:4326 | Guam, moist soil colors, 10cm (approx. 30m resolution) |
| gu_sc015cm | EPSG:4326 | Guam, moist soil colors, 15cm (approx. 30m resolution) |
| gu_sc025cm | EPSG:4326 | Guam, moist soil colors, 25cm (approx. 30m resolution) |
| gu_sc050cm | EPSG:4326 | Guam, moist soil colors, 50cm (approx. 30m resolution) |
| gu_sc075cm | EPSG:4326 | Guam, moist soil colors, 75cm (approx. 30m resolution) |
| gu_sc100cm | EPSG:4326 | Guam, moist soil colors, 100cm (approx. 30m resolution) |
| gu_sc125cm | EPSG:4326 | Guam, moist soil colors, 125cm (approx. 30m resolution) |
| hi_sc005cm | EPSG:6628 | Hawaii, moist soil colors, 5cm (30m resolution) |
| hi_sc010cm | EPSG:6628 | Hawaii, moist soil colors, 10cm (30m resolution) |
| hi_sc015cm | EPSG:6628 | Hawaii, moist soil colors, 15cm (30m resolution) |
| hi_sc025cm | EPSG:6628 | Hawaii, moist soil colors, 25cm (30m resolution) |
| hi_sc050cm | EPSG:6628 | Hawaii, moist soil colors, 50cm (30m resolution) |
| hi_sc075cm | EPSG:6628 | Hawaii, moist soil colors, 75cm (30m resolution) |
| hi_sc100cm | EPSG:6628 | Hawaii, moist soil colors, 100cm (30m resolution) |
| hi_sc125cm | EPSG:6628 | Hawaii, moist soil colors, 125cm (30m resolution) |
| mp_sc005cm | EPSG:4326 | Northern Mariana Islands, moist soil colors, 5cm (approx. 30m resolution) |
| mp_sc010cm | EPSG:4326 | Northern Mariana Islands, moist soil colors, 10cm (approx. 30m resolution) |
| mp_sc015cm | EPSG:4326 | Northern Mariana Islands, moist soil colors, 15cm (approx. 30m resolution) |
| mp_sc025cm | EPSG:4326 | Northern Mariana Islands, moist soil colors, 25cm (approx. 30m resolution) |
| mp_sc050cm | EPSG:4326 | Northern Mariana Islands, moist soil colors, 50cm (approx. 30m resolution) |
| mp_sc075cm | EPSG:4326 | Northern Mariana Islands, moist soil colors, 75cm (approx. 30m resolution) |
| mp_sc100cm | EPSG:4326 | Northern Mariana Islands, moist soil colors, 100cm (approx. 30m resolution) |
| mp_sc125cm | EPSG:4326 | Northern Mariana Islands, moist soil colors, 125cm (approx. 30m resolution) |
| pr_sc005cm | EPSG:32161 | Puerto Rico, moist soil colors, 5cm (30m resolution) |
| pr_sc010cm | EPSG:32161 | Puerto Rico, moist soil colors, 10cm (30m resolution) |
| pr_sc015cm | EPSG:32161 | Puerto Rico, moist soil colors, 15cm (30m resolution) |
| pr_sc025cm | EPSG:32161 | Puerto Rico, moist soil colors, 25cm (30m resolution) |
| pr_sc050cm | EPSG:32161 | Puerto Rico, moist soil colors, 50cm (30m resolution) |
| pr_sc075cm | EPSG:32161 | Puerto Rico, moist soil colors, 75cm (30m resolution) |
| pr_sc100cm | EPSG:32161 | Puerto Rico, moist soil colors, 100cm (30m resolution) |
| pr_sc125cm | EPSG:32161 | Puerto Rico, moist soil colors, 125cm (30m resolution) |
| pw_sc005cm | EPSG:4326 | Palau, moist soil colors, 5cm (approx. 30m resolution) |
| pw_sc010cm | EPSG:4326 | Palau, moist soil colors, 10cm (approx. 30m resolution) |
| pw_sc015cm | EPSG:4326 | Palau, moist soil colors, 15cm (approx. 30m resolution) |
| pw_sc025cm | EPSG:4326 | Palau, moist soil colors, 25cm (approx. 30m resolution) |
| pw_sc050cm | EPSG:4326 | Palau, moist soil colors, 50cm (approx. 30m resolution) |
| pw_sc075cm | EPSG:4326 | Palau, moist soil colors, 75cm (approx. 30m resolution) |
| pw_sc100cm | EPSG:4326 | Palau, moist soil colors, 100cm (approx. 30m resolution) |
| pw_sc125cm | EPSG:4326 | Palau, moist soil colors, 125cm (approx. 30m resolution) |
| sc005cm | EPSG:5070 | CONUS, moist soil color, 5cm (270m resolution) |
| sc005cm_hr | EPSG:5070 | CONUS, moist soil color, 5cm (30m resolution) |
| sc010cm | EPSG:5070 | CONUS, moist soil color, 10cm (270m resolution) |
| sc010cm_hr | EPSG:5070 | CONUS, moist soil color, 10cm (30m resolution) |
| sc015cm | EPSG:5070 | CONUS, moist soil color, 15cm (270m resolution) |
| sc015cm_hr | EPSG:5070 | CONUS, moist soil color, 15cm (30m resolution) |
| sc025cm | EPSG:5070 | CONUS, moist soil color, 25cm (270m resolution) |
| sc025cm_hr | EPSG:5070 | CONUS, moist soil color, 25cm (30m resolution) |
| sc050cm | EPSG:5070 | CONUS, moist soil color, 50cm (270m resolution) |
| sc050cm_hr | EPSG:5070 | CONUS, moist soil color, 50cm (30m resolution) |
| sc075cm | EPSG:5070 | CONUS, moist soil color, 75cm (270m resolution) |
| sc075cm_hr | EPSG:5070 | CONUS, moist soil color, 75cm (30m resolution) |
| sc100cm | EPSG:5070 | CONUS, moist soil color, 100cm (270m resolution) |
| sc100cm_hr | EPSG:5070 | CONUS, moist soil color, 100cm (30m resolution) |
| sc125cm | EPSG:5070 | CONUS, moist soil color, 125cm (270m resolution) |
| sc125cm_hr | EPSG:5070 | CONUS, moist soil color, 125cm (30m resolution) |

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
terra::plot(res, col = cats(res)[[1]]$col, axes = FALSE, legend = FALSE)
} # }
```
