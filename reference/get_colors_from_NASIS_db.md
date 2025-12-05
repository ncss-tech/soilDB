# Get Soil Color Data from a local NASIS Database

Get, format, mix, and return color data from a NASIS database.

## Usage

``` r
get_colors_from_NASIS_db(
  SS = TRUE,
  method = "dominant",
  mixColors = FALSE,
  dsn = NULL
)
```

## Arguments

- SS:

  fetch data from Selected Set in NASIS or from the entire local
  database (default: `TRUE`)

- method:

  Aggregation method to handle multiple colors per horizon and moisture
  state. Default `"dominant"` for dominant condition (or first record)
  within moisture state. Other options include `"mixed"` to calculate
  mixture using
  [`simplifyColorData()`](http://ncss-tech.github.io/soilDB/reference/simplifyColorData.md)
  and `"none"` to do no aggregation (returns a long format
  representation that may have multiple values per horizon and moisture
  state)

- mixColors:

  Deprecated. See `method`. Should mixed colors be calculated where
  multiple colors are populated for the same moisture state in a
  horizon? Default `FALSE` takes the dominant color based on `colorpct`
  or first record based on horizon ID (`phiid`) sorting for "moist" and
  "dry" state. Pedon Horizon Color records without a moisture state
  populated are ignored.

- dsn:

  Optional: path to local SQLite database containing NASIS table
  structure; default: `NULL`

## Value

A data.frame with the results.

## See also

[`simplifyColorData`](http://ncss-tech.github.io/soilDB/reference/simplifyColorData.md),
[`get_hz_data_from_NASIS_db`](http://ncss-tech.github.io/soilDB/reference/get_hz_data_from_NASIS_db.md),
[`get_site_data_from_NASIS_db`](http://ncss-tech.github.io/soilDB/reference/get_site_data_from_NASIS_db.md)

## Author

Jay M. Skovlin and Dylan E. Beaudette
