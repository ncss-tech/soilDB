# Get a pedon or component data `SoilProfileCollection` from NASIS

Fetch commonly used site/pedon/horizon or mapunit component data from
NASIS, returned as a `SoilProfileCollection` object.

This function imports data from NASIS into R as a
`SoilProfileCollection` object. It "flattens" NASIS pedon and component
tables, including their child tables, into several more manageable data
frames. Primarily these functions access the local NASIS database using
an ODBC connection. The `dsn` argument allows you to specify a path or
`DBIConnection` to an SQLite database. The argument
`from = "pedon_report"`, data can be read from the NASIS Report
'fetchNASIS', from either text file or URL (specified as `url`). The
primary purpose of `fetchNASIS(from = "pedon_report")` is importing
datasets larger than 8000+ pedons/components.

Tutorials:

- [fetchNASIS
  Columns](http://ncss-tech.github.io/soilDB/articles/fetchNASIS.md)

- [fetchNASIS Pedons
  Tutorial](http://ncss-tech.github.io/AQP/soilDB/fetchNASIS-mini-tutorial.md)

- [fetchNASIS Components
  Tutorial](http://ncss-tech.github.io/AQP/soilDB/NASIS-component-data.md)

## Usage

``` r
fetchNASIS(
  from = "pedons",
  url = NULL,
  SS = TRUE,
  rmHzErrors = FALSE,
  nullFragsAreZero = TRUE,
  soilColorState = "moist",
  mixColors = FALSE,
  lab = FALSE,
  fill = FALSE,
  dropAdditional = TRUE,
  dropNonRepresentative = TRUE,
  duplicates = FALSE,
  stringsAsFactors = NULL,
  dsn = NULL
)

get_concentrations_from_NASIS_db(
  SS = TRUE,
  stringsAsFactors = NULL,
  dsn = NULL
)

get_phfmp_from_NASIS_db(SS = TRUE, stringsAsFactors = NULL, dsn = NULL)
```

## Arguments

- from:

  Determines what objects should fetched? Default: `'pedons'`.
  Alternately, `'components'`, or `'pedon_report'`.

- url:

  String specifying the url for the NASIS pedon_report (default: `NULL`)

- SS:

  Fetch data from the currently loaded selected set in NASIS or from the
  entire Local database (default: `TRUE`)

- rmHzErrors:

  Should pedons with horizon depth errors be removed from the results?
  (default: `FALSE`)

- nullFragsAreZero:

  Should fragment volumes of `NULL` be interpreted as `0`? (default:
  `TRUE`), see details

- soilColorState:

  Used only for `from = 'pedons'`; which colors should be used to
  generate the convenience field `soil_color`? (`'moist'` or `'dry'`)

- mixColors:

  Should mixed colors be calculated where multiple colors are populated
  for the same moisture state in a horizon? Default `FALSE` takes the
  dominant color for each horizon moist/dry state.

- lab:

  Should the `phlabresults` child table be fetched with
  site/pedon/horizon data (default: `FALSE`)

- fill:

  Include pedon or component records without horizon data in result?
  (default: `FALSE`)

- dropAdditional:

  Used only for `from='components'` with `duplicates = TRUE`. Prevent
  "duplication" of `mustatus == "additional"` mapunits? Default: `TRUE`

- dropNonRepresentative:

  Used only for `from='components'` with `duplicates = TRUE`. Prevent
  "duplication" of non-representative data mapunits? Default: `TRUE`

- duplicates:

  Used only for `from='components'`. Duplicate components for all
  instances of use (i.e. one for each legend data mapunit is used on;
  optionally for additional mapunits, and/or non-representative data
  mapunits?). This will include columns from
  [`get_component_correlation_data_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/get_component_data_from_NASIS_db.md)
  that identify which legend(s) a component is used on.

- stringsAsFactors:

  deprecated

- dsn:

  Optional: path or *DBIConnection* to [local database containing NASIS
  table
  structure](http://ncss-tech.github.io/soilDB/reference/NASISLocalDatabase.md);
  default: `NULL`

## Value

A `SoilProfileCollection` object

## Details

The value of `nullFragsAreZero` will have a significant impact on the
rock fragment fractions returned by fetchNASIS. Set
`nullFragsAreZero = FALSE` in those cases where there are many data-gaps
and NULL rock fragment values should be interpreted as NULL. Set
`nullFragsAreZero = TRUE` in those cases where NULL rock fragment values
should be interpreted as 0.

This function attempts to do most of the boilerplate work when
extracting site/pedon/horizon or component data from a local NASIS
database. Pedon IDs that are missing horizon data, or have errors in
their horizonation are printed on the console. Pedons with combination
horizons (e.g. B/C) are erroneously marked as errors due to the way in
which they are stored in NASIS as two overlapping horizon records.

## See also

`get_component_data_from_NASIS()`

## Author

D. E. Beaudette, J. M. Skovlin, S.M. Roecker, A.G. Brown
