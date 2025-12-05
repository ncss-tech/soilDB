# Check for presence of `nasis_local` ODBC data source

Check for presence of a NASIS data source. This function *always*
returns `FALSE` when the `odbc` package is not available (regardless of
whether you have an ODBC data source properly set up).

## Usage

``` r
local_NASIS_defined(dsn = NULL)
```

## Arguments

- dsn:

  Optional: path to local SQLite database, or a DBIConnection,
  containing NASIS table structure; default: NULL

## Value

logical

## Details

If `dsn` is specified as a character vector it is assumed to refer to a
SQLite data source. The result will be `TRUE` or `FALSE` depending on
the result of `RSQLite::dbCanConnect()`.

If `dsn` is specified as a `DBIConnection` the function returns the
value of `DBI::dbExistsTable("MetadataDomainMaster")`

## Examples

``` r

if(local_NASIS_defined()) {
  # use fetchNASIS or some other lower-level fetch function
} else {
  message('could not find `nasis_local` ODBC data source')
}
#> could not find `nasis_local` ODBC data source
```
