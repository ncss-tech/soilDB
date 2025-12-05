# Get map unit parent material group information from Soil Data Access

Get map unit parent material group information from Soil Data Access

## Usage

``` r
get_SDA_pmgroupname(
  areasymbols = NULL,
  mukeys = NULL,
  WHERE = NULL,
  method = "DOMINANT COMPONENT",
  simplify = TRUE,
  include_minors = TRUE,
  miscellaneous_areas = FALSE,
  query_string = FALSE,
  dsn = NULL
)
```

## Arguments

- areasymbols:

  *character*. Vector of soil survey area symbols

- mukeys:

  *integer*. Vector of map unit keys

- WHERE:

  *character*. SQL WHERE clause specified in terms of fields in
  `legend`, `mapunit`, `component`, or `copmgrp` tables, used in lieu of
  `mukeys` or `areasymbols`

- method:

  *character*. One of: `"Dominant Component"`, `"Dominant Condition"`,
  `"None"`

- simplify:

  *logical*. Group into generalized parent material groups? Default
  `TRUE`

- include_minors:

  logical. Include minor components? Default: `TRUE`.

- miscellaneous_areas:

  *logical*. Include miscellaneous areas (non-soil components) in
  results? Default: `FALSE`.

- query_string:

  Default: `FALSE`; if `TRUE` return a character string containing query
  that would be sent to SDA via `SDA_query`

- dsn:

  Path to local SQLite database or a DBIConnection object. If `NULL`
  (default) use Soil Data Access API via
  [`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md).

## Value

a data.frame

## Details

Default `method` is `"Dominant Component"` to get the dominant component
(highest percentage). Use `"Dominant Condition"` or dominant parent
material condition (similar conditions aggregated across components).
Use `"None"` for no aggregation (one record per component).

## Author

Jason Nemecek, Chad Ferguson, Andrew Brown
