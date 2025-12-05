# Get the Component Soil Moisture Tables

Read and flatten the component soil moisture month tables from a local
NASIS Database.

## Usage

``` r
get_cosoilmoist_from_NASIS(
  SS = TRUE,
  impute = TRUE,
  stringsAsFactors = NULL,
  dsn = NULL
)
```

## Arguments

- SS:

  fetch data from the currently loaded selected set in NASIS or from the
  entire local database (default: `TRUE`)

- impute:

  replace missing (i.e. `NULL`) values with `"Not_Populated"` for
  categorical data, or the "RV" for numeric data or `201` cm if the "RV"
  is also `NULL` (default: `TRUE`)

- stringsAsFactors:

  deprecated

- dsn:

  Optional: path to local SQLite database containing NASIS table
  structure; default: `NULL`

## Value

A data.frame.

## Details

The component soil moisture tables within NASIS house monthly data on
flooding, ponding, and soil moisture status. The soil moisture status is
used to specify the water table depth for components (e.g.
`status == "Moist"`).

## See also

[fetchNASIS](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md),
[get_cosoilmoist_from_NASISWebReport](http://ncss-tech.github.io/soilDB/reference/fetchNASISWebReport.md),
[get_cosoilmoist_from_SDA](http://ncss-tech.github.io/soilDB/reference/fetchSDA.md),
`get_comonth_from_SDA`

## Author

S.M. Roecker

## Examples

``` r
# \donttest{
if(local_NASIS_defined()) {
 # load cosoilmoist (e.g. water table data)
 test <- try(get_cosoilmoist_from_NASIS())

 # inspect
 if(!inherits(test, 'try-error')) {
   head(test)
 }
}
# }
```
