# Get Logic Errors in NASIS/PedonPC Pedon Horizon

Get Logic Errors in NASIS/PedonPC Pedon Horizon

## Usage

``` r
getHzErrorsNASIS(strict = TRUE, SS = TRUE, dsn = NULL)
```

## Arguments

- strict:

  how strict should horizon boundaries be checked for consistency:
  TRUE=more \| FALSE=less

- SS:

  fetch data from the currently loaded selected set in NASIS or from the
  entire local database (default: TRUE)

- dsn:

  Optional: path to local SQLite database containing NASIS table
  structure; default: NULL

## Value

A data.frame containing problematic records with columns:
'peiid','upedonid','hzdept','hzdepb','hzname'
