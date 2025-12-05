# Get SRI Layers

Get SRI Layers

## Usage

``` r
get_SRI_layers(gdb)
```

## Arguments

- gdb:

  A `character` of the GDB, e.g. `'Deschutes'`.

## Value

A list of metadata about the GDB

## Note

Refer to
[`get_SRI`](http://ncss-tech.github.io/soilDB/reference/get_SRI.md) for
information on File Geodatabase (GDB) availability.

## Author

Josh Erickson

## Examples

``` r
if (FALSE) { # \dontrun{
sri_layers <- get_SRI_layers('Willamette')
} # }
```
