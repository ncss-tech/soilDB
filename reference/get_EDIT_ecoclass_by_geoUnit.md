# Get Ecological Dynamics Information Tool (EDIT) ecological sites by catalog (ESD/ESG) and MLRA

Data are accessed via Ecological Dynamics Interpretive Tool (EDIT) web
services: https://edit.jornada.nmsu.edu/resources/esd. `geoUnit` refers
to MLRA codes, possibly with a leading zero and trailing "X" for two
digit MLRA symbols.

## Usage

``` r
get_EDIT_ecoclass_by_geoUnit(geoUnit, catalog = c("esd", "esg"))
```

## Arguments

- geoUnit:

  A character vector of `geoUnit` codes e.g. `c("018X","022A")` for
  MLRAs 18 and 22A.

- catalog:

  Catalog ID. One of: `"esd"` or `"esg"`

## Value

A `data.frame` containing: `geoUnit`, `id`, `legacyId`, `name`. `NULL`
if no result.

## Examples

``` r
if (FALSE) { # \dontrun{
   get_EDIT_ecoclass_by_geoUnit(c("018X","022A"))
} # }
```
