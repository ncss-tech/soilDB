# Get Horizon Data from a local NASIS Database

Get horizon-level data from a local NASIS database.

## Usage

``` r
get_hz_data_from_NASIS_db(
  SS = TRUE,
  fill = FALSE,
  stringsAsFactors = NULL,
  dsn = NULL
)
```

## Arguments

- SS:

  fetch data from Selected Set in NASIS or from the entire local
  database (default: `TRUE`)

- fill:

  include pedons without horizon data in result? default: `FALSE`

- stringsAsFactors:

  deprecated

- dsn:

  Optional: path to local SQLite database containing NASIS table
  structure; default: `NULL`

## Value

A data.frame.

## Note

`NULL` total rock fragment values are assumed to represent an *absence*
of rock fragments, and set to 0.

## See also

`get_hz_data_from_NASIS_db`,
[`get_site_data_from_NASIS_db`](http://ncss-tech.github.io/soilDB/reference/get_site_data_from_NASIS_db.md)

## Author

Jay M. Skovlin and Dylan E. Beaudette
