# Get map unit hydric soils information from Soil Data Access

Assess the hydric soils composition of a map unit.

## Usage

``` r
get_SDA_hydric(
  areasymbols = NULL,
  mukeys = NULL,
  WHERE = NULL,
  method = "MAPUNIT",
  include_minors = TRUE,
  miscellaneous_areas = TRUE,
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
  `legend`, `mapunit`, or `component` tables, used in lieu of `mukeys`
  or `areasymbols`

- method:

  One of: `"Mapunit"`, `"Dominant Component"`, `"Dominant Condition"`,
  `"None"`

- include_minors:

  logical. Include minor components? Default: `TRUE`.

- miscellaneous_areas:

  *logical*. Include miscellaneous areas (non-soil components) in
  results? Default: `TRUE`.

- query_string:

  Default: `FALSE`; if `TRUE` return a character string containing query
  that would be sent to SDA via
  [`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md)

- dsn:

  Path to local SQLite database or a DBIConnection object. If `NULL`
  (default) use Soil Data Access API via
  [`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md).

## Value

a data.frame

## Details

The default classes for `method="MAPUNIT"` are as follows:

- `'Nonhydric'` - no hydric components

- `'Hydric'` - all hydric components

- `'Predominantly Hydric'` - hydric component percentage is 50% or more

- `'Partially Hydric'` - one or more of the major components is hydric

- `'Predominantly Nonhydric'` - hydric component percentage is less than
  50%

The default result will also include the following summaries of
component percentages: `total_comppct`, `hydric_majors` and
`hydric_inclusions`.

Default `method` `"Mapunit"` produces aggregate summaries of all
components in the mapunit. Use `"Dominant Component"` and
`"Dominant Condition"` to get the dominant component (highest
percentage) or dominant hydric condition (similar conditions aggregated
across components), respectively. Use `"None"` for no aggregation (one
record per component).

## Author

Jason Nemecek, Chad Ferguson, Andrew Brown
