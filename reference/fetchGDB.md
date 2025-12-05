# Get a SoilProfileCollection from a SSURGO file geodatabase

Functions to load and flatten commonly used tables and from SSURGO file
geodatabases, and create soil profile collection objects (SPC).

## Usage

``` r
get_component_from_GDB(
  dsn = "gNATSGO_CONUS.gdb",
  WHERE = NULL,
  childs = FALSE,
  droplevels = TRUE,
  stringsAsFactors = NULL
)

get_legend_from_GDB(
  dsn = "gNATSGO_CONUS.gdb",
  WHERE = NULL,
  droplevels = TRUE,
  stringsAsFactors = NULL,
  stats = FALSE
)

get_mapunit_from_GDB(
  dsn = "gNATSGO_CONUS.gdb",
  WHERE = NULL,
  droplevels = TRUE,
  stringsAsFactors = NULL,
  stats = FALSE
)

fetchGDB(
  dsn = "gNATSGO_CONUS.gdb",
  WHERE = NULL,
  childs = FALSE,
  droplevels = TRUE,
  stringsAsFactors = NULL
)
```

## Arguments

- dsn:

  data source name (interpretation varies by driver - for some drivers,
  dsn is a file name, but may also be a folder, or contain the name and
  access credentials of a database); in case of GeoJSON, dsn may be the
  character string holding the geojson data. It can also be an open
  database connection.

- WHERE:

  text string formatted as an SQL WHERE clause (default: FALSE)

- childs:

  logical; if FALSE parent material and geomorphic child tables are not
  flattened and appended

- droplevels:

  logical: indicating whether to drop unused levels in classifying
  factors. This is useful when a class has large number of unused
  classes, which can waste space in tables and figures.

- stringsAsFactors:

  deprecated

- stats:

  Return statistics (number of mapunit keys per legend; number of
  components, major components per mapunit, total and hydric component
  percentage)? Default: `FALSE`

## Value

A `data.frame` or `SoilProfileCollection` object.

## Details

These functions return data from SSURGO file geodatabases with the use
of a simple text string that formatted as an SQL WHERE clause (e.g.
`WHERE = "areasymbol = 'IN001'"`. Any columns within the target table
can be specified (except for fetchGDB() which currently can only target
one table (e.g. legend, mapunit or component) at a time with the WHERE
clause).

## Author

Stephen Roecker

## Examples

``` r
# \donttest{

## replace `dsn` with path to your own geodatabase (SSURGO OR gNATSGO)
##
##  download CONUS gNATSGO from here:
##    https://nrcs.app.box.com/v/soils/folder/191790828371
##
# dsn <- "D:/geodata/soils/gNATSGO_CONUS.gdb"
# le <- get_legend_from_GDB(dsn = dsn, WHERE = "areasymbol LIKE '%'")
# mu <- get_mapunit_from_GDB(dsn = dsn, WHERE = "muname LIKE 'Miami%'")
# co <- get_component_from_GDB(dsn, WHERE = "compname = 'Miami'
#                              AND majcompflag = 'Yes'", childs = FALSE)
# f_in_GDB <- fetchGDB(WHERE = "areasymbol LIKE 'IN%'")

# }
```
