# Query a NASIS DBIConnection

Send queries to a NASIS DBIConnection

## Usage

``` r
dbQueryNASIS(conn, q, close = TRUE, ...)
```

## Arguments

- conn:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).

- q:

  A statement to execute using
  [`DBI::dbGetQuery`](https://dbi.r-dbi.org/reference/dbGetQuery.html);
  or a (named) vector containing multiple statements to evaluate
  separately

- close:

  Close connection after query? Default: `TRUE`

- ...:

  Additional arguments to
  [`DBI::dbGetQuery`](https://dbi.r-dbi.org/reference/dbGetQuery.html)

## Value

Result of
[`DBI::dbGetQuery`](https://dbi.r-dbi.org/reference/dbGetQuery.html)
