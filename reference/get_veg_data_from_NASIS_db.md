# Get vegetation data from a local NASIS Database

Get vegetation data from a local NASIS Database. Result includes two
data.frames corresponding to the "Plot Plant Inventory" and "Vegetation
Transect" child tables of "Vegetation Plot".

## Usage

``` r
get_veg_data_from_NASIS_db(SS = TRUE, dsn = NULL)
```

## Arguments

- SS:

  get data from the currently loaded Selected Set in NASIS or from the
  entire local database (default: `TRUE`)

- dsn:

  Optional: path to local SQLite database containing NASIS table
  structure; default: `NULL`

## Value

A list of data.frame

## See also

[`fetchVegdata()`](http://ncss-tech.github.io/soilDB/reference/fetchVegdata.md)

## Author

Jay M. Skovlin and Dylan E. Beaudette
