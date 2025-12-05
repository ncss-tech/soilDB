# Get vegetation plot data from local NASIS database

Convenience function for loading most commonly used Vegetation Plot
information from local NASIS database.

## Usage

``` r
fetchVegdata(
  SS = TRUE,
  include_pedon = TRUE,
  nullFragsAreZero = TRUE,
  stringsAsFactors = NULL,
  dsn = NULL
)

get_vegplot_from_NASIS_db(SS = TRUE, stringsAsFactors = NULL, dsn = NULL)

get_vegplot_location_from_NASIS_db(
  SS = TRUE,
  stringsAsFactors = NULL,
  dsn = NULL
)

get_vegplot_trhi_from_NASIS_db(SS = TRUE, stringsAsFactors = NULL, dsn = NULL)

get_vegplot_species_from_NASIS_db(
  SS = TRUE,
  stringsAsFactors = NULL,
  dsn = NULL
)

get_vegplot_transect_from_NASIS_db(
  SS = TRUE,
  stringsAsFactors = NULL,
  dsn = NULL
)

get_vegplot_transpecies_from_NASIS_db(
  SS = TRUE,
  stringsAsFactors = NULL,
  dsn = NULL
)

get_vegplot_transpoints_from_NASIS_db(SS = TRUE, dsn = NULL)

get_vegplot_prodquadrats_from_NASIS_db(SS = TRUE, dsn = NULL)

get_vegplot_groundsurface_from_NASIS_db(SS = TRUE, dsn = NULL)

get_vegplot_tree_si_summary_from_NASIS_db(
  SS = TRUE,
  stringsAsFactors = NULL,
  dsn = NULL
)

get_vegplot_speciesbasalarea_from_NASIS(SS = TRUE, dsn = NULL)

get_vegplot_tree_si_details_from_NASIS_db(
  SS = TRUE,
  stringsAsFactors = NULL,
  dsn = NULL
)

get_vegplot_textnote_from_NASIS_db(
  SS = TRUE,
  fixLineEndings = TRUE,
  stringsAsFactors = NULL,
  dsn = NULL
)
```

## Arguments

- SS:

  fetch data from the currently loaded selected set in NASIS or from the
  entire local database (default: `TRUE`)

- include_pedon:

  Include pedon and transect data joined to site? (default: `TRUE`). If
  `include_pedon` is set to `"assocuserpedonid"` only pedon records that
  are linked through the Associated User Pedon ID column will have their
  peiid reported in the `vegplot` table.

- nullFragsAreZero:

  Should fragment volumes of `NULL` be interpreted as `0`? (default:
  `TRUE`), see details

- stringsAsFactors:

  deprecated

- dsn:

  Optional: path to local SQLite database containing NASIS table
  structure; default: `NULL`

- fixLineEndings:

  Replace `'\r\n'` with `'\n'`; Default: `TRUE`

## Value

`fetchVegdata()`: A named list containing: "vegplot", "vegplotlocation",
"vegplotrhi", "vegplotspecies", "vegtransect", "vegtransplantsum",
'vegsiteindexsum', "vegsiteindexdet", "vegbasalarea", and "vegplottext"
tables

`get_vegplot_location_from_NASIS_db()`: a data.frame containing location
data from the corresponding record in the site table

`get_vegplot_trhi_from_NASIS_db()`: a data.frame containing Rangeland
Health Indicator (RHI) data from the `vegplot` table

`get_vegplot_species_from_NASIS_db()`: a data.frame containing Plot
Plant Inventory data

`get_vegplot_transect_from_NASIS_db()`: a data.frame containing
Vegetation Transect data

`get_vegplot_transect_from_NASIS_db()`: a data.frame containing
Vegetation Transect Plant Summary data

`get_vegplot_transpoints_from_NASIS_db()`: a data.frame containing
Vegetation Transect Point Plant Cover Details

`get_vegplot_prodquadrats_from_NASIS_db()`: a data.frame containing
Vegetation Transect Production Quadrat data

`get_vegplot_groundsurface_from_NASIS_db()`: a data.frame containing
summary data for line point intercept ground surface cover hits by cover
type.

`get_vegplot_tree_si_summary_from_NASIS_db()`: a data.frame containing
Vegetation Plot Tree Site Index Summary data

`get_vegplot_speciesbasalarea_from_NASIS()`: a data.frame containing
Vegetation Plot Species Basal Area and Trees Counted data

`get_vegplot_tree_si_details_from_NASIS_db()`: a data.frame containing
Vegetation Plot Tree Site Index Details data

`get_vegplot_textnote_from_NASIS_db()`: a data.frame containing
Vegetation Plot text notes

## Examples

``` r
if (FALSE) { # local_NASIS_defined()
# \donttest{
vsurf <- get_vegplot_groundsurface_from_NASIS_db()
# }
}
```
