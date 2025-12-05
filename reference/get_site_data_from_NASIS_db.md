# Get Site Data from a local NASIS Database

Get site-level data from a local NASIS database.

`get_site_association_from_NASIS()`: Get Associated User Site IDs for
each Site.

## Usage

``` r
get_site_data_from_NASIS_db(
  SS = TRUE,
  include_pedon = TRUE,
  nullFragsAreZero = TRUE,
  stringsAsFactors = NULL,
  dsn = NULL
)

get_site_association_from_NASIS(SS = TRUE, dsn = NULL)
```

## Arguments

- SS:

  fetch data from Selected Set in NASIS or from the entire local
  database (default: `TRUE`)

- include_pedon:

  Include pedon and transect data joined to site? (default: `TRUE`)

- nullFragsAreZero:

  should surface fragment cover percentages of `NULL` be interpreted as
  `0`? (default: `TRUE`)

- stringsAsFactors:

  deprecated

- dsn:

  Optional: path to local SQLite database containing NASIS table
  structure; default: `NULL`

## Value

A data.frame

## Details

It is possible to have multiple pedon records per site observation and
multiple site observations per site, which will result in multiple
records per site. See argument `include_pedon=FALSE` to omit joins to
pedon and transect tables.

The following aggregations of child table information are performed by
this function:

- Site Area Overlap for State, County and MLRA are returned for related
  area records, as specified in the site table, as the following column
  names: `site_state`, `site_county`, and `site_mlra`.

- Site Observation Surface Fragment data are simplified (converted to
  wide format) using
  [`simplifyFragmentData()`](http://ncss-tech.github.io/soilDB/reference/simplifyFragmentData.md).

- The best Ecological Site History record is selected using
  `get_ecosite_history_from_NASIS_db(best = TRUE)`.

- Site Other Vegetation Class information is aggregated by class name,
  using `" & "` as the separator when multiple classes are assigned.

- When multiple Site Bedrock entries are present, only the shallowest is
  returned by this function. In lieu of bedrock depth the first record
  in the table is returned.

## See also

[`get_hz_data_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/get_hz_data_from_NASIS_db.md),
[`fetchNASIS()`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md),
[`fetchVegdata()`](http://ncss-tech.github.io/soilDB/reference/fetchVegdata.md)

## Author

Jay M. Skovlin, Dylan E. Beaudette, Andrew G. Brown, Greg Schmidt
