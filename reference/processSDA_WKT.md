# Post-process Well-Known Text from Soil Data Access

This is a helper function commonly used with `SDA_query` to extract WKT
(well-known text) representation of geometry to an `sf` or `sp` object.

## Usage

``` r
processSDA_WKT(d, g = "geom", crs = 4326, p4s = NULL, as_sf = TRUE)
```

## Arguments

- d:

  `data.frame` returned by `SDA_query`, containing WKT representation of
  geometry

- g:

  name of column in `d` containing WKT geometry

- crs:

  CRS definition (e.g. an EPSG code). Default `4326` for WGS84
  Geographic Coordinate System

- p4s:

  Deprecated: PROJ4 CRS definition

- as_sf:

  Return an `sf` `data.frame`? If `FALSE` return a `Spatial*` object.

## Value

An `sf` object or if `as_sf` is `FALSE` a `Spatial*` object.

## Details

The SDA website can be found at <https://sdmdataaccess.nrcs.usda.gov>.
See the [SDA
Tutorial](http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.md) for
detailed examples.

The SDA website can be found at <https://sdmdataaccess.nrcs.usda.gov>.
See the [SDA
Tutorial](http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.md) for
detailed examples.

## Note

This function requires the `sf` package.

## Author

D.E. Beaudette, A.G. Brown
