# Get component data from a local NASIS Database

Functions for getting component and related child table data from local
NASIS database.

## Usage

``` r
get_component_data_from_NASIS_db(
  SS = TRUE,
  nullFragsAreZero = TRUE,
  stringsAsFactors = NULL,
  dsn = NULL
)

get_component_diaghz_from_NASIS_db(SS = TRUE, dsn = NULL)

get_component_restrictions_from_NASIS_db(SS = TRUE, dsn = NULL)

get_component_correlation_data_from_NASIS_db(
  SS = TRUE,
  dropAdditional = TRUE,
  dropNotRepresentative = TRUE,
  stringsAsFactors = NULL,
  dsn = NULL
)

get_component_cogeomorph_data_from_NASIS_db(SS = TRUE, dsn = NULL)

get_component_cogeomorph_data_from_NASIS_db2(SS = TRUE, dsn = NULL)

get_component_copm_data_from_NASIS_db(
  SS = TRUE,
  stringsAsFactors = NULL,
  dsn = NULL
)

get_component_esd_data_from_NASIS_db(
  SS = TRUE,
  stringsAsFactors = NULL,
  dsn = NULL
)

get_component_otherveg_data_from_NASIS_db(SS = TRUE, dsn = NULL)

get_copedon_from_NASIS_db(SS = TRUE, dsn = NULL)

get_component_horizon_data_from_NASIS_db(
  SS = TRUE,
  fill = FALSE,
  dsn = NULL,
  nullFragsAreZero = TRUE
)
```

## Arguments

- SS:

  fetch data from the currently loaded selected set in NASIS or from the
  entire local database (default: `TRUE`)

- nullFragsAreZero:

  should surface fragment cover percentages of NULL be interpreted as 0?
  (default: TRUE)

- stringsAsFactors:

  deprecated

- dsn:

  Optional: path or *DBIConnection* to [local database containing NASIS
  table
  structure](http://ncss-tech.github.io/soilDB/reference/NASISLocalDatabase.md);
  default: `NULL`

- dropAdditional:

  Remove map units with "additional" status? Default: `TRUE`

- dropNotRepresentative:

  Remove non-representative data map units? Default: `TRUE`

- fill:

  Return a single minimal (NA-filled) horizon for components with no
  horizon records? Default `FALSE`

## Value

a `data.frame`

## See also

[`fetchNASIS`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md)

## Author

Dylan E. Beaudette, Stephen Roecker, and Jay M. Skovlin

## Examples

``` r
# \donttest{
if(local_NASIS_defined()) {
 fc <- try(get_component_data_from_NASIS_db())

 # show structure of component data returned
 str(fc)
}
# }
```
