# Create local NASIS database connection

Create a connection to a local NASIS database with `DBI`

## Usage

``` r
dbConnectNASIS(dsn = NULL)

NASIS(dsn = NULL)
```

## Arguments

- dsn:

  Optional: path to SQLite database containing NASIS table structure;
  Default: `NULL`

## Value

A `DBIConnection` object, as returned by
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html). If
`dsn` is a `DBIConnection`, the attribute `isUserDefined` of the result
is set to `TRUE`. If the `DBIConnection` is created by the internal
NASIS connection process, `isUserDefined` is set to `FALSE.`
