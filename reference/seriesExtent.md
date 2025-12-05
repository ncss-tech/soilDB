# Retrieve Soil Series Extent Maps from SoilWeb

This function downloads a generalized representations of a soil series
extent from SoilWeb, derived from the current SSURGO snapshot. Data can
be returned as vector outlines (`sf` object) or gridded representation
of area proportion falling within 800m cells (`SpatRaster` object).
Gridded series extent data are only available in CONUS. Vector
representations are returned with a GCS/WGS84 coordinate reference
system and raster representations are returned with an Albers Equal Area
/ NAD83 coordinate reference system (`EPSG:5070`).

## Usage

``` r
seriesExtent(
  s,
  type = c("vector", "raster"),
  timeout = 60,
  as_Spatial = getOption("soilDB.return_Spatial", default = FALSE)
)
```

## Arguments

- s:

  a soil series name, case-insensitive

- type:

  series extent representation, `'vector'`: results in an `sf` object
  and `'raster'` results in a `SpatRaster` object

- timeout:

  time that we are willing to wait for a response, in seconds

- as_Spatial:

  Return sp (`SpatialPolygonsDataFrame`) / raster (`RasterLayer`)
  classes? Default: `FALSE`.

## Value

An R spatial object, class depending on `type` and `as_Spatial`
arguments

## References

<https://casoilresource.lawr.ucdavis.edu/see/>

## Author

D.E. Beaudette

## Examples

``` r
if (FALSE) { # \dontrun{
  
  # specify a soil series name
  s <- 'magnor'
  
  # return an sf object
  x <- seriesExtent(s, type = 'vector')
  
  # return a terra SpatRasters
  y <- seriesExtent(s, type = 'raster')
  
  library(terra)
  if (!is.null(x) && !is.null(y)) {
    x <- terra::vect(x)
    # note that CRS are different
    terra::crs(x)
    terra::crs(y)
  
    # transform vector representation to CRS of raster
    x <- terra::project(x, terra::crs(y))
  
    # graphical comparison
    par(mar = c(1, 1 , 1, 3))
    plot(y, axes = FALSE)
    plot(x, add = TRUE)
  }
} # }
```
