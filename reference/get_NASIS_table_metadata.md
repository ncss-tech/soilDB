# Get NASIS Table Metadata (Table and Column Descriptions)

Retrieve a table containing table and column names with descriptions,
help text, units of measure, etc. from NASIS 7 metadata tables.

## Usage

``` r
get_NASIS_table_metadata(
  table = NULL,
  column = NULL,
  what.table = "TablePhysicalName",
  what.column = "ColumnPhysicalName",
  query_string = FALSE,
  dsn = NULL
)
```

## Arguments

- table:

  Character vector of table identifiers to match. Default `NULL` for
  "all tables" (no constraint)

- column:

  Character vector of column identifiers to match. Default `NULL` for
  "all columns" (in selected tables, if any, otherwise no constraint)

- what.table:

  Column to match `table` against. Default: `TablePhysicalName`.

- what.column:

  Column to match `column` against. Default: `ColumnPhysicalName`.

- query_string:

  Default: `FALSE`; if `TRUE` return a character containing query that
  would be sent to NASIS.

- dsn:

  Optional: path or *DBIConnection* to [local database containing NASIS
  table
  structure](http://ncss-tech.github.io/soilDB/reference/NASISLocalDatabase.md);
  default: `NULL`

## Value

a `data.frame`

## Details

These data are derived from the MetadataTable and MetadataTableColumn
tables and describe the expected contents of standard NASIS tables and
columns.

For NASIS choice lists based on domain and column names see
[`get_NASIS_metadata()`](http://ncss-tech.github.io/soilDB/reference/get_NASIS_metadata.md)
and
[`NASISChoiceList()`](http://ncss-tech.github.io/soilDB/reference/NASISChoiceList.md).
This function (`get_NASIS_table_metadata()`) is intended for
higher-level description of the expected contents of a NASIS database
instance, rather than the codes/specific values used within columns.

## See also

[`get_NASIS_metadata()`](http://ncss-tech.github.io/soilDB/reference/get_NASIS_metadata.md)
[`NASISChoiceList()`](http://ncss-tech.github.io/soilDB/reference/NASISChoiceList.md)
[`uncode()`](http://ncss-tech.github.io/soilDB/reference/uncode.md)
[`code()`](http://ncss-tech.github.io/soilDB/reference/uncode.md)

## Examples

``` r
if (local_NASIS_defined())
 str(get_NASIS_table_metadata())
```
