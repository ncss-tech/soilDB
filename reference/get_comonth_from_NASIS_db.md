# Get component month data from a local NASIS Database

Get component month data from a local NASIS Database.

## Usage

``` r
get_comonth_from_NASIS_db(
  SS = TRUE,
  fill = FALSE,
  stringsAsFactors = NULL,
  dsn = NULL
)
```

## Arguments

- SS:

  get data from the currently loaded Selected Set in NASIS or from the
  entire local database (default: TRUE)

- fill:

  should missing "month" rows in the comonth table be filled with NA
  (FALSE)

- stringsAsFactors:

  deprecated

- dsn:

  Optional: path or *DBIConnection* to [local database containing NASIS
  table
  structure](http://ncss-tech.github.io/soilDB/reference/NASISLocalDatabase.md);
  default: `NULL`

## Value

A list with the results.

## See also

[`fetchNASIS`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md)

## Author

Stephen Roecker

## Examples

``` r
# \donttest{
if(local_NASIS_defined()) {
  # query text note data
  cm <- try(get_comonth_from_NASIS_db())

  # show structure of component month data
  str(cm)
}
# }
```
