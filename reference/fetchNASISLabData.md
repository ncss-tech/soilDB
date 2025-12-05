# Get NCSS Pedon laboratory data from NASIS

Fetch KSSL laboratory pedon/horizon layer data from a local NASIS
database, return as a SoilProfileCollection object.

## Usage

``` r
fetchNASISLabData(SS = TRUE, dsn = NULL)
```

## Arguments

- SS:

  fetch data from the currently loaded selected set in NASIS or from the
  entire local database (default: `TRUE`)#'

- dsn:

  Optional: path to local SQLite database containing NASIS table
  structure; default: `NULL`

## Value

a SoilProfileCollection object

## See also

[`get_labpedon_data_from_NASIS_db`](http://ncss-tech.github.io/soilDB/reference/get_labpedon_data_from_NASIS_db.md)

## Author

J.M. Skovlin and D.E. Beaudette
