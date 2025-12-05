# Get SSURGO ZIP files from Web Soil Survey 'Download Soils Data'

Download ZIP files containing spatial (ESRI shapefile) and tabular (TXT)
files with standard SSURGO format; optionally including the
corresponding SSURGO Template Database with `include_template=TRUE`.

## Usage

``` r
downloadSSURGO(
  WHERE = NULL,
  areasymbols = NULL,
  destdir = tempdir(),
  exdir = destdir,
  include_template = FALSE,
  db = c("SSURGO", "STATSGO"),
  extract = TRUE,
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

- db:

  *character*. Either `"SSURGO"` (default; detailed soil map) or
  `"STATSGO"` (general soil map).

- extract:

  *logical*. Extract ZIP files to `exdir`? Default: `TRUE`

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

To specify the Soil Survey Areas you would like to obtain data you use a
`WHERE` clause for query of `sacatalog` table such as
`areasymbol = 'CA067'`, `"areasymbol IN ('CA628', 'CA067')"` or
`areasymbol LIKE 'CT%'`.

When `db="STATSGO"` the `WHERE` argument is not supported. Allowed
`areasymbols` include `"US"` and two-letter state codes e.g. `"WY"` for
the Wyoming general soils map.

Pipe-delimited TXT files are found in */tabular/* folder extracted from
a SSURGO ZIP. The files are named for tables in the SSURGO schema. There
is no header / the files do not have column names. See the *Soil Data
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
the soil survey area symbol.

## See also

[`createSSURGO()`](http://ncss-tech.github.io/soilDB/reference/createSSURGO.md)
