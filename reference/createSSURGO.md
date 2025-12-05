# Create a database from SSURGO Exports

The following database types are tested and fully supported:

- SQLite or Geopackage

- DuckDB

- Postgres or PostGIS

## Usage

``` r
createSSURGO(
  filename = NULL,
  exdir,
  conn = NULL,
  pattern = NULL,
  include_spatial = TRUE,
  include_tabular = TRUE,
  dissolve_field = NULL,
  maxruledepth = 0,
  overwrite = FALSE,
  append = FALSE,
  header = FALSE,
  quiet = TRUE,
  ...
)
```

## Arguments

- filename:

  *character*. Output file name (e.g. `'db.sqlite'` or `'db.gpkg'`).
  Only used when `con` is not specified by the user.

- exdir:

  *character*. Path containing containing input SSURGO spatial (.shp)
  and tabular (.txt) files, downloaded and extracted by
  [`downloadSSURGO()`](http://ncss-tech.github.io/soilDB/reference/downloadSSURGO.md)
  or similar.

- conn:

  A *DBIConnection* object. Default is a `SQLiteConnection` used for
  writing .sqlite or .gpkg files. Alternate options are any DBI
  connection types. When `include_spatial=TRUE`, the sf package is used
  to write spatial data to the database.

- pattern:

  *character*. Optional regular expression to use to filter
  subdirectories of `exdir`. Default: `NULL` will search all
  subdirectories for SSURGO export files.

- include_spatial:

  *logical* or *character*. Include spatial data layers in database?
  Default: `TRUE` inserts all spatial tables. If `include_spatial` is a
  *character* vector containing table names, only that set are written
  to file. e.g. `include_spatial=c("mupolygon", "featpoint")` writes
  only the mapunit polygons and special feature points.

- include_tabular:

  *logical* or *character*. Include tabular data layers in database?
  Default: `TRUE` inserts all tabular tables. If `include_tabular` is a
  *character* vector containing table names, only that set are written
  to file. e.g. `include_tabular=c("mapunit", "muaggatt")` writes only
  the `mapunit` and `muaggatt` tables. Note that special feature
  descriptions are stored in table `"featdesc"` and metadata for each
  soil survey area are stored in `"soil_metadata"` tables.

- dissolve_field:

  *character*. Dissolve geometries to create MULTIPOLYGON features?
  Column name specified is the grouping variable. Default: `NULL` does
  no aggregation, giving 1 `POLYGON` feature per delineation. `"mukey"`
  aggregates all related delineations within a soil survey area.

- maxruledepth:

  *integer*. Maximum rule depth for `"cointerp"` table. Default `0`
  includes only shallowest ratings for smaller database size.

- overwrite:

  *logical*. Overwrite existing layers? Default: `FALSE`

- append:

  *logical*. Append to existing layers? Default: `FALSE`

- header:

  *logical*. Passed to
  [`read.delim()`](https://rdrr.io/r/utils/read.table.html) for reading
  pipe-delimited (`|`) text files containing tabular data.

- quiet:

  *logical*. Suppress messages and other output from database read/write
  operations?

- ...:

  Additional arguments passed to
  [`write_sf()`](https://r-spatial.github.io/sf/reference/st_write.html)
  for writing spatial layers.

## Value

Character. Vector of layer/table names in `filename`.

## Details

In theory any other DBI-compatible data source can be used for output.
See `conn` argument. If you encounter issues using specific DBI
connection types, please report in the soilDB issue tracker.

## See also

[`downloadSSURGO()`](http://ncss-tech.github.io/soilDB/reference/downloadSSURGO.md)

## Examples

``` r
if (FALSE) { # \dontrun{
 downloadSSURGO("areasymbol IN ('CA067', 'CA077', 'CA632')", destdir = "SSURGO_test")
 createSSURGO("test.gpkg", "SSURGO_test")
} # }
```
