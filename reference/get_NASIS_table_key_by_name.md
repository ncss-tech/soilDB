# Get a NASIS table key by type and table name

Get a NASIS table key by type and table name

## Usage

``` r
get_NASIS_table_key_by_name(
  tables,
  keycol = c("all", "fkey", "pkeyref", "pkey")
)
```

## Arguments

- tables:

  character vector of table names

- keycol:

  One of: "fkey" the foreign key; "pkeyref" the primary key referenced
  by the foreign key, or "pkey" the primary key.

## Value

The key column name for the specified table name

## Examples

``` r
if (FALSE) { # \dontrun{
get_NASIS_table_key_by_name(c("site","phorizon_View_1","not_a_table"))
} # }#' 
```
