# Get pedon horizon roots data from a local NASIS Database

This function returns records from the `phroots` table of a local NASIS
database. Pedon and pedon horizon record IDs are also included for
linking back to related records, typically queried via
[`fetchNASIS()`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md).

## Usage

``` r
get_phroots_from_NASIS_db(SS = TRUE, dsn = NULL)
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

A `data.frame`
