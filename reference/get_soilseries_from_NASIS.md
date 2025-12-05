# Get records from the Series Classification (SC) database

These functions return records from the Series Classification (SC)
database, either from the local NASIS database (all series) or via web
report (named series only).

`get_competing_soilseries_from_NASIS():` Get Soil Series from NASIS
Matching Taxonomic Class Name

## Usage

``` r
get_soilseries_from_NASIS(
  stringsAsFactors = NULL,
  dsn = NULL,
  delimiter = " over ",
  SS = FALSE
)

get_soilseries_from_NASISWebReport(soils, stringsAsFactors = NULL)

get_competing_soilseries_from_NASIS(
  x,
  what = "taxclname",
  dsn = NULL,
  SS = FALSE
)
```

## Arguments

- stringsAsFactors:

  deprecated

- dsn:

  Optional: path or *DBIConnection* to [local database containing NASIS
  table
  structure](http://ncss-tech.github.io/soilDB/reference/NASISLocalDatabase.md);
  default: `NULL`

- delimiter:

  *character*. Used to collapse `taxminalogy` records where multiple
  values are used to describe strongly contrasting control sections.
  Default `" over "` creates combination mineralogy classes as they
  would be used in the family name.

- SS:

  *logical*. Fetch data from the currently loaded selected set in NASIS
  or from the entire local database (default: `FALSE`; this is to allow
  for queries against the full Series Classification database as
  default)

- soils:

  A vector of soil series names

- x:

  Taxonomic Class Name (or other field specified by `what`) to match,
  use `%` for wildcard

- what:

  Column name to match `x` against, default: `'taxclname'`

## Value

A `data.frame`

## Author

Stephen Roecker
