# List cached Web Soil Survey ZIP archives

List cached Web Soil Survey ZIP archives stored by
[`downloadSSURGO()`](http://ncss-tech.github.io/soilDB/reference/downloadSSURGO.md).
The same cache policy module is used by
[`downloadSSURGO()`](http://ncss-tech.github.io/soilDB/reference/downloadSSURGO.md)
to place and pick ZIPs.

## Usage

``` r
list_WSS_cache(
  areasymbols = NULL,
  fiscal_year = NULL,
  db = c("SSURGO", "STATSGO"),
  pattern = NULL,
  cache_dir = NULL
)
```

## Arguments

- areasymbols:

  *character*. Optional areasymbol filter.

- fiscal_year:

  *character* or *integer*. Optional fiscal year filter such as
  `"FY26"`.

- db:

  *character*. One or more of `"SSURGO"` or `"STATSGO"`. Defaults to
  both.

- pattern:

  *character*. Optional regular expression used to filter cached ZIP
  filenames.

- cache_dir:

  *character*. Optional cache root. Defaults to the soilDB WSS cache.

## Value

A `data.frame` of cached ZIP metadata.
