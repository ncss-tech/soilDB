# Get NASIS Project Information

Helper functions for accessing the NASIS Project object.

## Usage

``` r
get_projectmilestone_from_NASIS(SS = TRUE, stringsAsFactors = NULL, dsn = NULL)

get_projectmapunit_from_NASIS(SS = TRUE, stringsAsFactors = NULL, dsn = NULL)
```

## Arguments

- SS:

  *logical*. Use selected set? Default: `TRUE`. Set `FALSE` for local
  database.

- stringsAsFactors:

  Deprecated.

- dsn:

  Optional: path or *DBIConnection* to [local database containing NASIS
  table
  structure](http://ncss-tech.github.io/soilDB/reference/NASISLocalDatabase.md);
  default: `NULL`

## Value

`get_projectmilestone_from_NASIS()`: *data.frame* containing project and
project milestone information

`get_projectmapunit_from_NASIS()`: *data.frame* containing project and
project mapunit information
