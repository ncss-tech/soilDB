# Get map unit aggregate attribute information from Soil Data Access

Get map unit aggregate attribute information from Soil Data Access

## Usage

``` r
get_SDA_muaggatt(
  areasymbols = NULL,
  mukeys = NULL,
  WHERE = NULL,
  query_string = FALSE,
  dsn = NULL
)
```

## Arguments

- areasymbols:

  vector of soil survey area symbols

- mukeys:

  vector of map unit keys

- WHERE:

  character containing SQL WHERE clause specified in terms of fields in
  `legend`, `mapunit`, or `muaggatt` tables, used in lieu of `mukeys` or
  `areasymbols`

- query_string:

  Default: `FALSE`; if `TRUE` return a character string containing query
  that would be sent to SDA via `SDA_query`

- dsn:

  Path to local SQLite database or a DBIConnection object. If `NULL`
  (default) use Soil Data Access API via
  [`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md).

## Value

a data.frame

## Author

Jason Nemecek, Chad Ferguson, Andrew Brown
