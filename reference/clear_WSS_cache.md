# Clean cached Web Soil Survey ZIP archives

Clean cached Web Soil Survey ZIP archives stored by
[`downloadSSURGO()`](http://ncss-tech.github.io/soilDB/reference/downloadSSURGO.md).
By default, this will clear the entire cache unless filtered by
arguments.

## Usage

``` r
clear_WSS_cache(
  areasymbols = NULL,
  fiscal_year = NULL,
  db = c("SSURGO", "STATSGO"),
  pattern = NULL,
  latest_only = FALSE,
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

- latest_only:

  *logical*. If `TRUE`, preserves the most recent ZIP for each
  area/fiscal year and deletes only outdated/orphaned ones. Default:
  `FALSE`.

- cache_dir:

  *character*. Optional cache root. Defaults to the soilDB WSS cache.

## Value

The removed file paths are returned invisibly.
