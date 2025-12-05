# Get text note data from a local NASIS Database

Get text note data from a local NASIS Database

## Usage

``` r
get_text_notes_from_NASIS_db(SS = TRUE, fixLineEndings = TRUE, dsn = NULL)

get_mutext_from_NASIS_db(SS = TRUE, fixLineEndings = TRUE, dsn = NULL)

get_cotext_from_NASIS_db(SS = TRUE, fixLineEndings = TRUE, dsn = NULL)
```

## Arguments

- SS:

  get data from the currently loaded Selected Set in NASIS or from the
  entire local database (default: `TRUE`)

- fixLineEndings:

  convert line endings from `\r\n` to `\n`

- dsn:

  Optional: path to local SQLite database containing NASIS table
  structure; default: `NULL`

## Value

A `list` with the results.

## See also

[`get_hz_data_from_pedon_db`](http://ncss-tech.github.io/soilDB/reference/get_hz_data_from_pedon_db.md),
[`get_site_data_from_pedon_db`](http://ncss-tech.github.io/soilDB/reference/get_site_data_from_pedon_db.md)

## Author

Dylan E. Beaudette and Jay M. Skovlin

## Examples

``` r
# \donttest{
if(local_NASIS_defined()) {
 # query text note data
 t <- try(get_text_notes_from_NASIS_db())

 # show contents text note data, includes: siteobs, site, pedon, horizon level text notes data.
 str(t)

 # view text categories for site text notes
 if(!inherits(t, 'try-error')) {
  table(t$site_text$textcat)
 }
}
# }
```
