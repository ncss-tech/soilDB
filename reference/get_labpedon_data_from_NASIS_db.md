# Get lab pedon data from a local NASIS Database

Get lab pedon-level data from a local NASIS database.

## Usage

``` r
get_labpedon_data_from_NASIS_db(SS = TRUE, dsn = NULL)
```

## Arguments

- SS:

  fetch data from the currently loaded selected set in NASIS or from the
  entire local database (default: TRUE)

- dsn:

  Optional: path to local SQLite database containing NASIS table
  structure; default: `NULL`

## Value

A data.frame.

## Note

This function queries KSSL laboratory site/horizon data from a local
NASIS database from the lab pedon data table.

## See also

[`get_lablayer_data_from_NASIS_db`](http://ncss-tech.github.io/soilDB/reference/get_lablayer_data_from_NASIS_db.md)

## Author

Jay M. Skovlin and Dylan E. Beaudette
