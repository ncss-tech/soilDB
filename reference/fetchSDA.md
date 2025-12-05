# Get SSURGO/STATSGO2 Mapunit Data from Soil Data Access

Functions to download and flatten commonly used tables and from Soil
Data Access, and create soil profile collection objects (SPC).

## Usage

``` r
get_component_from_SDA(
  WHERE = NULL,
  duplicates = FALSE,
  childs = TRUE,
  droplevels = TRUE,
  nullFragsAreZero = TRUE,
  stringsAsFactors = NULL
)

get_cointerp_from_SDA(
  WHERE = NULL,
  mrulename = NULL,
  duplicates = FALSE,
  droplevels = TRUE,
  stringsAsFactors = NULL
)

get_legend_from_SDA(WHERE = NULL, droplevels = TRUE, stringsAsFactors = NULL)

get_lmuaoverlap_from_SDA(
  WHERE = NULL,
  droplevels = TRUE,
  stringsAsFactors = NULL
)

get_mapunit_from_SDA(WHERE = NULL, droplevels = TRUE, stringsAsFactors = NULL)

get_chorizon_from_SDA(
  WHERE = NULL,
  duplicates = FALSE,
  childs = TRUE,
  nullFragsAreZero = TRUE,
  droplevels = TRUE,
  stringsAsFactors = NULL
)

fetchSDA(
  WHERE = NULL,
  duplicates = FALSE,
  childs = TRUE,
  nullFragsAreZero = TRUE,
  rmHzErrors = FALSE,
  droplevels = TRUE,
  stringsAsFactors = NULL
)

get_cosoilmoist_from_SDA(
  WHERE = NULL,
  duplicates = FALSE,
  impute = TRUE,
  stringsAsFactors = NULL
)
```

## Arguments

- WHERE:

  text string formatted as an SQL WHERE clause (default: FALSE)

- duplicates:

  logical; if TRUE a record is returned for each unique mukey (may be
  many per nationalmusym)

- childs:

  logical; if FALSE parent material and geomorphic child tables are not
  flattened and appended

- droplevels:

  logical: indicating whether to drop unused levels in classifying
  factors. This is useful when a class has large number of unused
  classes, which can waste space in tables and figures.

- nullFragsAreZero:

  should fragment volumes of NULL be interpreted as 0? (default: TRUE),
  see details

- stringsAsFactors:

  deprecated

- mrulename:

  character. Interpretation rule names

- rmHzErrors:

  should pedons with horizonation errors be removed from the results?
  (default: FALSE)

- impute:

  replace missing (i.e. `NULL`) values with `"Not_Populated"` for
  categorical data, or the "RV" for numeric data or `201` cm if the "RV"
  is also`NULL` (default: `TRUE`)

## Value

A data.frame or SoilProfileCollection object.

## Details

These functions return data from Soil Data Access with the use of a
simple text string that formatted as an SQL WHERE clause (e.g.
`WHERE = "areasymbol = 'IN001'"`. All functions are SQL queries that
wrap around `SDAquery()` and format the data for analysis.

Beware SDA includes the data for both SSURGO and STATSGO2. The
`areasymbol` for STATSGO2 is `US`. For just SSURGO, include
`WHERE = "areareasymbol != 'US'"`.

If the duplicates argument is set to TRUE, duplicate components are
returned. This is not necessary with data returned from NASIS, which has
one unique national map unit. SDA has duplicate map national map units,
one for each legend it exists in.

The value of `nullFragsAreZero` will have a significant impact on the
rock fragment fractions returned by `fetchSDA`. Set
`nullFragsAreZero = FALSE` in those cases where there are many data-gaps
and NULL rock fragment values should be interpreted as NULLs. Set
`nullFragsAreZero = TRUE` in those cases where NULL rock fragment values
should be interpreted as 0.

Additional examples can be found in the [Soil Data Access (SDA)
Tutorial](http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.md)

## See also

[SDA_query](http://ncss-tech.github.io/soilDB/reference/SDA_query.md)

## Author

Stephen Roecker
