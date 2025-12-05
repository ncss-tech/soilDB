# Get 800m gridded soil properties from SoilWeb ISSR-800 Web Coverage Service (WCS)

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

  area of interest (AOI) defined using a `Spatial*`, `RasterLayer`,
  `sf`, `sfc` or `bbox` object, OR a `list`, see details

- var:

  ISSR-800 grid name (case insensitive), see details

- res:

  grid resolution, units of meters. The native resolution of ISSR-800
  grids (this WCS) is 800m.

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
(EPSG:5070) of the ISSR-800 grids.

Variables available from this WCS can be queried using
`WCS_details(wcs = 'ISSR800')`.

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
