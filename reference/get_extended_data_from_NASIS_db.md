# Get accessory tables and summaries from a local NASIS Database

Get accessory tables and summaries from a local NASIS Database

## Usage

``` r
get_extended_data_from_NASIS_db(
  SS = TRUE,
  nullFragsAreZero = TRUE,
  stringsAsFactors = NULL,
  dsn = NULL
)
```

## Arguments

- SS:

  get data from the currently loaded Selected Set in NASIS or from the
  entire local database (default: `TRUE`)

- nullFragsAreZero:

  should fragment volumes of NULL be interpreted as 0? (default: TRUE),
  see details

- stringsAsFactors:

  deprecated

- dsn:

  Optional: path to local SQLite database containing NASIS table
  structure; default: `NULL`

## Value

A list with the results.

## See also

[`get_hz_data_from_NASIS_db`](http://ncss-tech.github.io/soilDB/reference/get_hz_data_from_NASIS_db.md),
[`get_site_data_from_NASIS_db`](http://ncss-tech.github.io/soilDB/reference/get_site_data_from_NASIS_db.md)

## Author

Jay M. Skovlin and Dylan E. Beaudette

## Examples

``` r
# \donttest{

if(local_NASIS_defined()) {
 # query extended data
 e <- try(get_extended_data_from_NASIS_db())

 # show contents of extended data
 str(e)
}

# }
```
