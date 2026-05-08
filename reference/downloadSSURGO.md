# Get SSURGO ZIP files from Web Soil Survey 'Download Soils Data'

Download ZIP files containing spatial (ESRI shapefile) and tabular (TXT)
files in standard SSURGO format. To specify the Soil Survey Areas you
would like to download, use a `WHERE` clause for query of `sacatalog`
table, for example: `areasymbol = 'CA067'`,
`"areasymbol IN ('CA628', 'CA067')"`, or `areasymbol LIKE 'CT%'`.

## Usage

``` r
downloadSSURGO(
  WHERE = NULL,
  areasymbols = NULL,
  destdir = tempdir(),
  exdir = destdir,
  include_template = FALSE,
  include_spatial = TRUE,
  include_tabular = TRUE,
  db = c("SSURGO", "STATSGO"),
  extract = TRUE,
  LAPPLY.FUN = lapply,
  LAPPLY.FUN.ARGS = NULL,
  remove_zip = FALSE,
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- WHERE:

  *character*. A SQL `WHERE` clause expression used to filter records in
  `sacatalog` table. Alternately `WHERE` can be any spatial object
  supported by
  [`SDA_spatialQuery()`](http://ncss-tech.github.io/soilDB/reference/SDA_spatialQuery.md)
  for defining the target extent.

- areasymbols:

  *character*. Character vector of soil survey area symbols e.g.
  `c("CA067", "CA077")`. Used in lieu of `WHERE` argument.

- destdir:

  *character*. Directory to download ZIP files into. Default
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- exdir:

  *character*. Directory to extract ZIP archives into. May be a
  directory that does not yet exist. Each ZIP file will extract to a
  folder labeled with `areasymbol` in this directory. Default: `destdir`

- include_template:

  *logical*. Include the (possibly state-specific) MS Access template
  database? Default: `FALSE`

- include_spatial:

  *logical* or *character*. Extract spatial data layers from ZIP file?
  Default: `TRUE` inserts all spatial tables. If `include_spatial` is a
  *character* vector containing table names, only that set is extracted
  from the downloaded ZIP files. e.g.
  `include_spatial=c("mupolygon", "featpoint")` extracts only the
  shapefiles (with side car files) for mapunit polygons and special
  feature points.

- include_tabular:

  *logical* or *character*. Extract tabular data from ZIP file? Default:
  `TRUE` inserts all tabular tables. If `include_tabular` is a
  *character* vector containing table names, only that set is extracted
  from the downloaded ZIP files. e.g.
  `include_tabular=c("mapunit", "muaggatt")` writes only the `mapunit`
  and `muaggatt` tables. Note that special feature descriptions are
  stored in table `"featdesc"` and metadata for each soil survey area
  are stored in `"soil_metadata"` tables.

- db:

  *character*. Either `"SSURGO"` (default; detailed soil map) or
  `"STATSGO"` (general soil map).

- extract:

  *logical*. Extract ZIP files to `exdir`? Default: `TRUE`

- LAPPLY.FUN:

  *function*. [`lapply()`](https://rdrr.io/r/base/lapply.html)-like
  function to use for iteration during `extract` phase. Only used if
  `extract=TRUE`. This allows for the
  [`utils::unzip()`](https://rdrr.io/r/utils/unzip.html) operations to
  be run in parallel instead of sequential, custom progress reporting,
  or similar.

- LAPPLY.FUN.ARGS:

  *list*. Optional list of additional arguments to pass to `LAPPLY.FUN`.

- remove_zip:

  *logical*. Remove ZIP files after extracting? Default: `FALSE`

- overwrite:

  *logical*. Overwrite by re-extracting if directory already exists?
  Default: `FALSE`

- quiet:

  *logical*. Passed to
  [`curl::curl_download()`](https://jeroen.r-universe.dev/curl/reference/curl_download.html).

## Value

*character*. Paths to downloaded ZIP files (invisibly). May not exist if
`remove_zip = TRUE`.

## Details

Pipe-delimited TXT files are found in */tabular/* folder extracted from
a SSURGO ZIP. The files are named for tables in the SSURGO schema. There
is no header and the files do not have column names. See the *Soil Data
Access Tables and Columns Report*:
<https://sdmdataaccess.nrcs.usda.gov/documents/TablesAndColumnsReport.pdf>
for details on tables, column names and metadata including the default
sequence of columns used in TXT files. The function returns a
`try-error` if the `WHERE`/`areasymbols` arguments result in

Several ESRI shapefiles are found in the */spatial/* folder extracted
from a SSURGO ZIP. These have prefix `soilmu_` (mapunit), `soilsa_`
(survey area), `soilsf_` (special features). There will also be a TXT
file with prefix `soilsf_` describing any special features. Shapefile
names then have an `a_` (polygon), `l_` (line), `p_` (point) followed by
the soil survey area symbol. When `db="STATSGO"` the `WHERE` argument is
not supported. Allowed `areasymbols` include `"US"` and two-letter state
codes e.g. `"WY"` for the Wyoming general soils map.

As in
[`createSSURGO()`](http://ncss-tech.github.io/soilDB/reference/createSSURGO.md),
the `include_spatial` and `include_tabular` arguments either take a
logical value (default `TRUE`) or a character vector of the specific
table names to include. Note that when used in `downloadSSURGO()` the
required metadata files are *always* extracted to facilitate mapping to
user-facing table names. These arguments allow for customizing the files
that get extracted from ZIP files, not just filtering on file names (as
is implemented with pre-existing `pattern` argument). This can
dramatically improve efficiency of extraction and the overall size of
the data in `exdir`. These arguments can be used in conjunction with the
`pattern` argument to fine-tune the files included in the generated
snapshot database.

## See also

[`createSSURGO()`](http://ncss-tech.github.io/soilDB/reference/createSSURGO.md)
