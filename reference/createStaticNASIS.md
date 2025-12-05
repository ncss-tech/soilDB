# Create a memory or file-based instance of NASIS database

Create a memory or file-based instance of NASIS database for selected
tables.

## Usage

``` r
createStaticNASIS(
  tables = NULL,
  new_names = NULL,
  SS = TRUE,
  dsn = NULL,
  output_path = NULL,
  verbose = FALSE
)
```

## Arguments

- tables:

  Character vector of target tables. Default: `NULL` is whatever tables
  are listed by
  [`DBI::dbListTables`](https://dbi.r-dbi.org/reference/dbListTables.html)
  for the connection typ being used.

- new_names:

  Optional: new table names (should match length of vector of matching
  `tables` in `dsn`)

- SS:

  Logical. Include "selected set" tables (ending with suffix
  `"_View_1"`). Default: `TRUE`

- dsn:

  Optional: path to SQLite database containing NASIS table structure; or
  a `DBIConnection`. Default: `NULL`

- output_path:

  Optional: path to new/existing SQLite database to write tables to.
  Default: `NULL` returns table results as named list.

- verbose:

  Show error messages from attempts to dump individual tables? Default
  `FALSE`

## Value

A named list of results from calling `dbQueryNASIS` for all columns in
each NASIS table.
