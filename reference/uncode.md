# Convert coded values returned from NASIS and SDA queries into human-readable values

These functions convert the coded values returned from NASIS or SDA to
factors (e.g. 1 = Alfisols) using the metadata tables from NASIS. For
SDA the metadata is pulled from a static snapshot in the soilDB package
(/data/metadata.rda).

## Usage

``` r
uncode(
  df,
  invert = FALSE,
  db = "NASIS",
  droplevels = FALSE,
  stringsAsFactors = NULL,
  dsn = NULL
)

code(df, db = NULL, droplevels = FALSE, stringsAsFactors = NULL, dsn = NULL)
```

## Arguments

- df:

  data.frame

- invert:

  converts the code labels back to their coded values (`FALSE`)

- db:

  label specifying the soil database the data is coming from, which
  indicates whether or not to query metadata from local NASIS database
  ("NASIS") or use soilDB-local snapshot ("LIMS" or "SDA")

- droplevels:

  logical: indicating whether to drop unused levels in classifying
  factors. This is useful when a class has large number of unused
  classes, which can waste space in tables and figures.

- stringsAsFactors:

  deprecated

- dsn:

  Optional: path to local SQLite database containing NASIS table
  structure; default: `NULL`

## Value

A `data.frame` with the results.

## Details

These functions convert the coded values returned from NASIS into their
plain text representation. It duplicates the functionality of the
CODELABEL function found in NASIS. This function is primarily intended
to be used internally by other soilDB R functions, in order to minimize
the need to manually convert values.

The function works by iterating through the column names in a data frame
and looking up whether they match any of the ColumnPhysicalNames found
in the metadata domain tables. If matches are found then the columns
coded values are converted to their corresponding factor levels.
Therefore it is not advisable to reuse column names from NASIS unless
the contents match the range of values and format found in NASIS.
Otherwise uncode() will convert their values to NA.

When data is being imported from NASIS, the metadata tables are sourced
directly from NASIS. When data is being imported from SDA or the NASIS
Web Reports, the metadata is pulled from a static snapshot in the soilDB
package.

Set `options(soilDB.NASIS.skip_uncode = TRUE)` to bypass decoding logic;
for instance when using soilDB NASIS functions with custom NASIS
snapshots that have already been decoded.

## Author

Stephen Roecker

## Examples

``` r
# convert column name `fraghard` (fragment hardness) codes to labels
uncode(data.frame(fraghard = 1:10))
#>               fraghard
#> 1          noncemented
#> 2            indurated
#> 3  moderately cemented
#> 4    strongly cemented
#> 5      weakly cemented
#> 6     extremely weakly
#> 7          very weakly
#> 8        very strongly
#> 9               weakly
#> 10          moderately

# convert column name `fragshp` (fragment shape) labels to codes
code(data.frame(fragshp = c("flat", "nonflat")))
#>   fragshp
#> 1       1
#> 2       2
```
