# Get Legend, Mapunit and Legend Mapunit Area Overlap Tables

Get Legend, Mapunit and Legend Mapunit Area Overlap Tables

## Usage

``` r
get_mapunit_from_NASIS(
  SS = TRUE,
  repdmu = TRUE,
  droplevels = TRUE,
  stringsAsFactors = NULL,
  areatypename = c("Non-MLRA Soil Survey Area", "MLRA Soil Survey Area"),
  dsn = NULL
)

get_legend_from_NASIS(
  SS = TRUE,
  droplevels = TRUE,
  stringsAsFactors = NULL,
  areatypename = c("Non-MLRA Soil Survey Area", "MLRA Soil Survey Area"),
  dsn = NULL
)

get_lmuaoverlap_from_NASIS(
  SS = TRUE,
  droplevels = TRUE,
  stringsAsFactors = NULL,
  areatypename = c("Non-MLRA Soil Survey Area", "MLRA Soil Survey Area"),
  dsn = NULL
)
```

## Arguments

- SS:

  Fetch data from the currently loaded selected set in NASIS or from the
  entire local database (default: `TRUE`)

- repdmu:

  Return only "representative" data mapunits? Default: `TRUE`

- droplevels:

  Drop unused levels from `farmlndcl` and other factor levels from NASIS
  domains?

- stringsAsFactors:

  deprecated

- areatypename:

  Used for `get_legend_from_NASIS()`. Default:
  `c('Non-MLRA Soil Survey Area', 'MLRA Soil Survey Area')`

- dsn:

  Optional: path to local SQLite database containing NASIS table
  structure; default: `NULL`
