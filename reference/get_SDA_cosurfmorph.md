# Get Geomorphic/Surface Morphometry Data from Soil Data Access

Get Geomorphic/Surface Morphometry Data from Soil Data Access or a local
SSURGO data source and summarize by counts and proportions
("probabilities").

## Usage

``` r
get_SDA_cosurfmorph(
  table = c("cosurfmorphgc", "cosurfmorphhpp", "cosurfmorphss", "cosurfmorphmr"),
  by = "mapunit.mukey",
  areasymbols = NULL,
  mukeys = NULL,
  WHERE = NULL,
  method = c("bygroup", "none"),
  include_minors = TRUE,
  miscellaneous_areas = FALSE,
  representative_only = TRUE,
  db = c("SSURGO", "STATSGO"),
  dsn = NULL,
  query_string = FALSE
)
```

## Arguments

- table:

  Target table to summarize. Default: `"cosurfmorphgc"` (3D Geomorphic
  Component). Alternate choices include `cosurfmorphhpp` (2D Hillslope
  Position), `cosurfmorphss` (Surface Shape), and `cosurfmorphmr`
  (Microrelief).

- by:

  Grouping variable. Default: `"mapunit.mukey"`

- areasymbols:

  A vector of soil survey area symbols (e.g. `'CA067'`)

- mukeys:

  A vector of map unit keys (e.g. `466627`)

- WHERE:

  WHERE clause added to SQL query. For example: `areasymbol = 'CA067'`

- method:

  *character*. One of: `"ByGroup"`, `"None"`

- include_minors:

  logical. Include minor components? Default: `TRUE`.

- miscellaneous_areas:

  logical. Include miscellaneous areas (non-soil components) in results?
  Default: `FALSE`.

- representative_only:

  logical. Include only representative Component Parent Material Groups?
  Default: `TRUE`.

- db:

  Either `'SSURGO'` (default) or `'STATSGO'`. If `'SSURGO'` is specified
  `areasymbol = 'US'` records are excluded. If `'STATSGO'` only
  `areasymbol = 'US'` records are included.

- dsn:

  Path to local SSURGO database SQLite database. Default `NULL` uses
  Soil Data Access.

- query_string:

  Return query instead of sending to Soil Data Access / local database.
  Default: `FALSE`.

## Value

a `data.frame` containing the grouping variable (`by`) and tabular
summaries of counts and proportions of geomorphic records.

## Details

Default `table="cosurfmorphgc"` summarizes columns `geomposmntn`,
`geomposhill`, `geomposflats`, and `geompostrce`.
`table="cosurfmorphhpp"` summarizes `"hillslopeprof"`,
`table="cosurfmorphss"` summarizes `shapeacross` and `shapedown`, and
`table="cosurfmorphmr"` summarizes `geomicrorelief`.

Queries are a generalization of now-deprecated functions from
sharpshootR package by Dylan Beaudette: `geomPosMountainProbability()`,
`geomPosHillProbability()`, `surfaceShapeProbability()`,
`hillslopeProbability()`

Similar summaries of SSURGO component surface morphometry data by series
name can be found in `fetchOSD(, extended=TRUE)` or downloaded from
<https://github.com/ncss-tech/SoilWeb-data> Full component data
including surface morphometry summaries at the "site" level can be
obtained with
[`fetchSDA()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA.md).

## See also

[`fetchSDA()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA.md)
[`get_SDA_pmgroupname()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_pmgroupname.md)

## Author

Dylan E. Beaudette, Andrew G. Brown

## Examples

``` r
if (FALSE) { # \dontrun{
 # Summarize by 3D geomorphic components by component name (default `by='compname'`)
 get_SDA_cosurfmorph(WHERE = "areasymbol = 'CA630'")

 # Whole Soil Survey Area summary (using `by = 'areasymbol'`)
 get_SDA_cosurfmorph(by = 'areasymbol', WHERE = "areasymbol = 'CA630'")

 # 2D Hillslope Position summary(using `table = 'cosurfmorphhpp'`)
 get_SDA_cosurfmorph('cosurfmorphhpp', WHERE = "areasymbol = 'CA630'")

 # Surface Shape summary (using `table = 'cosurfmorphss'`)
 get_SDA_cosurfmorph('cosurfmorphss', WHERE = "areasymbol = 'CA630'")

 # Microrelief summary (using `table = 'cosurfmorphmr'`)
 get_SDA_cosurfmorph('cosurfmorphmr', WHERE = "areasymbol = 'CA630'")
} # }
```
