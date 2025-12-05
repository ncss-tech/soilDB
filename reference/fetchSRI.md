# Fetch Soil Inventory Resource (SRI) for USFS Region 6

This is a higher level wrapper around the
[get_SRI](http://ncss-tech.github.io/soilDB/reference/get_SRI.md) and
[get_SRI_layers](http://ncss-tech.github.io/soilDB/reference/get_SRI_layers.md)
functions. This function can fetch multiple File Geodatabases (GDB) and
returns all the layers within the GDB.

## Usage

``` r
fetchSRI(gdb, ...)
```

## Arguments

- gdb:

  A `character` vector of the GDB(s), e.g. `'Deschutes'`.

- ...:

  Arguments to pass to
  [get_SRI](http://ncss-tech.github.io/soilDB/reference/get_SRI.md).

## Value

A list.

## See also

[`get_SRI()`](http://ncss-tech.github.io/soilDB/reference/get_SRI.md)
[`get_SRI_layers()`](http://ncss-tech.github.io/soilDB/reference/get_SRI_layers.md)

## Author

Josh Erickson

## Examples

``` r
if (FALSE) { # \dontrun{

# fetch Willamette and Winema SRI

sri <- fetchSRI(gdb = c('will', 'win'), quiet = TRUE)

} # }
```
