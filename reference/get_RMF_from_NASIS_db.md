# Get RMF data from local NASIS

Prepare a list of `data.frame` objects with data from the
"phrdxfeatures" and "phredoxfcolor" tables. These tables are related by
"phrdxfiid" column, and related to horizon data via "phiid".

## Usage

``` r
get_RMF_from_NASIS_db(SS = TRUE, dsn = NULL)
```

## Arguments

- SS:

  logical, limit query to the selected set

- dsn:

  optional path or *DBIConnection* to [local database containing NASIS
  table
  structure](http://ncss-tech.github.io/soilDB/reference/NASISLocalDatabase.md);
  default: `NULL`

## Value

a `list` with two `data.frame` objects:

- `RMF`: contents of "phrdxfeatures" table, often \>1 row per horizon

- `RMF_colors`: contents of "phredoxfcolor", usually \>1 row per record
  in "phrdxfeatures"
