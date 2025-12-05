# Get Site Ecological Site History

Gets the Site Ecological Site History data from local NASIS database.
Used by
[`get_extended_data_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/get_extended_data_from_NASIS_db.md).

## Usage

``` r
get_ecosite_history_from_NASIS_db(
  best = TRUE,
  SS = TRUE,
  es_classifier = NULL,
  dsn = NULL
)
```

## Arguments

- best:

  Should the "best" ecological site correlation be chosen? Creates field
  called `es_selection_method` with `"most recent"` or
  `"least missing data"` for resolving many:1 relationships in site
  history.

- SS:

  Use selected set? Default: `TRUE`

- es_classifier:

  Optional: character. Vector of classifier names (and corresponding
  records) to retain in final result.

- dsn:

  Path to SQLite data source, or a `DBIConnection` to database with
  NASIS schema.

## Value

a `data.frame`, or `NULL` on error

## See also

[`get_extended_data_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/get_extended_data_from_NASIS_db.md)
