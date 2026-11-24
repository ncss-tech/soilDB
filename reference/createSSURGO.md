# Create a database from SSURGO Exports

The following database types are tested and fully supported:

- SQLite or Geopackage

- DuckDB

- Postgres or PostGIS

## Usage

``` r
createSSURGO(
  filename = NULL,
  exdir = NULL,
  conn = NULL,
  pattern = NULL,
  include_spatial = TRUE,
  include_tabular = TRUE,
  include_raster = FALSE,
  LAPPLY.FUN = lapply,
  LAPPLY.FUN.ARGS = NULL,
  dissolve_field = NULL,
  maxruledepth = 0,
  overwrite = FALSE,
  append = FALSE,
  header = FALSE,
  sep = "|",
  na.strings = c("", "NA"),
  quote = "",
  fiscal_year = NULL,
  template = NULL,
  touches = FALSE,
  grid_alignment = c("gssurgo", "target_aligned", "nlcd"),
  resolution_m = 10,
  quiet = TRUE,
  areasymbols = NULL,
  ...
)
```

## Arguments

- filename:

  *character*. Output file name (e.g. `'db.sqlite'` or `'db.gpkg'`).
  Only used when `con` is not specified by the user.

- exdir:

  *character*. Path containing input SSURGO spatial (.shp) and tabular
  (.txt) files, downloaded and extracted by
  [`downloadSSURGO()`](http://ncss-tech.github.io/soilDB/reference/downloadSSURGO.md)
  or similar. Default: `NULL` selects the most recent fiscal-year cache
  directory.

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

- include_raster:

  *logical*. Generate MUKEY GeoTIFF rasters for each SSA? Default:
  `FALSE` does not create raster outputs. Set to `TRUE` to create
  georeferenced rasters cached in `soilDB_user_dir("cache", "WSS", ...)`
  and output a combined/copied raster matching `filename`. Requires
  `terra` package.

- LAPPLY.FUN:

  *function*. [`lapply()`](https://rdrr.io/r/base/lapply.html)-like
  function to use for iteration during rasterization phase. Only used if
  `include_raster=TRUE`. This allows for the rasterization operations to
  be run in parallel instead of sequential, custom progress reporting,
  or similar.

- LAPPLY.FUN.ARGS:

  *list*. Optional list of additional arguments to pass to `LAPPLY.FUN`.

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
  [`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html)
  for reading delimited tabular text files. Default: `FALSE`

- sep:

  *character*. Passed to
  [`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html).
  Default: `"|"`

- na.strings:

  *character*. Passed to
  [`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html).
  Default: `c("", "NA")`

- quote:

  *character*. Passed to
  [`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html).
  Default: `""`

- fiscal_year:

  *character*. Optional fiscal year label for cache organization (e.g.,
  `'FY2024'`, `'FY26'`). If `NULL`, auto-detect from sacatalog saverest
  date. Allows organizing rasters by official release year or custom
  labels. Only used if `include_raster=TRUE`.

- template:

  Optional `SpatRaster` template defining a common target CRS and grid,
  including cell size, for all selected SSAs. Use a template when the
  selection spans regional CRSs or when a geographic CRS is needed. If
  `NULL` (default), creates a standard grid at `resolution_m` in the
  regional standard CRS (EPSG:5070 for CONUS, EPSG:3338 for AK,
  EPSG:6628 for HI, EPSG:32161 for PR/VI, EPSG:4326 for Pacific Islands)
  with bounds snapped according to `grid_alignment`.

- touches:

  *logical*. If `TRUE`, all cells touched by polygon boundaries are
  rasterized. Default `FALSE` rasterizes only cells whose centroids fall
  within polygons.

- grid_alignment:

  *character*. Grid alignment mode when `template = NULL`. Use
  `"gssurgo"` (default), `"target_aligned"`, or `"nlcd"`. The GSSURGO
  mode uses a half-cell origin matching official USDA-NRCS products;
  target-aligned uses a zero origin; and NLCD uses the canonical WGS84
  Albers origin anchor `(-2415585, 3314805)`. A supplied `template`
  takes precedence.

- resolution_m:

  *numeric*. Finite positive raster cell size in meters when
  `template = NULL` and the regional CRS is projected (default: `10`).
  Meter-based rasterization with a geographic regional CRS requires an
  explicit `template`.

- quiet:

  *logical*. Suppress messages and other output from database read/write
  operations?

- areasymbols:

  *character*. Optional character vector of soil survey area symbols
  (e.g. `c("CA067", "CA077")`) used to subset the folders/files within
  `exdir` to process. Default `NULL` processes all SSURGO exports found
  in `exdir`.

- ...:

  Additional arguments passed to
  [`sf::write_sf()`](https://r-spatial.github.io/sf/reference/st_write.html)
  for writing spatial layers.

## Value

*character*. Vector of layer/table names in `filename`.

## Details

In theory any other DBI-compatible data source can be used for output.
See `conn` argument. If you encounter issues using specific DBI
connection types, please report in the soilDB issue tracker.

When `include_raster=TRUE`, common resolutions include 10m, 30m, 300m,
and 900m; any positive resolution can be requested. Custom `SpatRaster`
templates can be supplied for arbitrary grids. The alignment modes are
intentional: `gssurgo` matches official USDA-NRCS products,
`target_aligned` follows zero-origin GDAL-style alignment, and `nlcd`
uses the canonical WGS84 Albers origin. These choices support
interoperability with federal raster products including NLCD, CDL, USGS
3DEP, and gNATSGO. A supplied template defines a common CRS and grid for
all selected SSAs. Without a template, selected SSAs must share a
regional CRS. The Pacific Island default CRS (EPSG:4326) is geographic,
so meter-based rasterization there requires an explicit template. Raster
output uses `NAflag = 0`; MUKEY zero is reserved as NoData.

A single SSURGO MUKEY per cell may be misleading at resolutions greater
than 90 meters. A warning is issued for recognized SSURGO area symbols
when the output grid exceeds this resolution.

## See also

[`downloadSSURGO()`](http://ncss-tech.github.io/soilDB/reference/downloadSSURGO.md)

## Examples

``` r
if (FALSE) { # \dontrun{
 downloadSSURGO("areasymbol IN ('CA067', 'CA077', 'CA632')", destdir = "SSURGO_test")
 createSSURGO("test.gpkg", "SSURGO_test")
} # }
```
