# Get component tables from NASIS Web Reports

Get component tables from NASIS Web Reports

## Usage

``` r
fetchNASISWebReport(
  projectname,
  rmHzErrors = FALSE,
  fill = FALSE,
  stringsAsFactors = NULL
)

get_component_from_NASISWebReport(projectname, stringsAsFactors = NULL)

get_chorizon_from_NASISWebReport(
  projectname,
  fill = FALSE,
  stringsAsFactors = NULL
)

get_legend_from_NASISWebReport(
  mlraoffice,
  areasymbol,
  droplevels = TRUE,
  stringsAsFactors = NULL
)

get_lmuaoverlap_from_NASISWebReport(
  areasymbol,
  droplevels = TRUE,
  stringsAsFactors = NULL
)

get_mapunit_from_NASISWebReport(
  areasymbol,
  droplevels = TRUE,
  stringsAsFactors = NULL
)

get_projectmapunit_from_NASISWebReport(projectname, stringsAsFactors = NULL)

get_projectmapunit2_from_NASISWebReport(
  mlrassoarea,
  fiscalyear,
  projectname,
  stringsAsFactors = NULL
)

get_project_from_NASISWebReport(mlrassoarea, fiscalyear)

get_progress_from_NASISWebReport(mlrassoarea, fiscalyear, projecttypename)

get_project_correlation_from_NASISWebReport(
  mlrassoarea,
  fiscalyear,
  projectname
)

get_cosoilmoist_from_NASISWebReport(
  projectname,
  impute = TRUE,
  stringsAsFactors = NULL
)

get_sitesoilmoist_from_NASISWebReport(usiteid)
```

## Arguments

- projectname:

  text string vector of project names to be inserted into a SQL WHERE
  clause (default: `NA`)

- rmHzErrors:

  should pedons with horizonation errors be removed from the results?
  (default: `FALSE`)

- fill:

  should rows with missing component ids be removed (default: `FALSE`)

- stringsAsFactors:

  deprecated

- mlraoffice:

  text string value identifying the MLRA Regional Soil Survey Office
  group name inserted into a SQL WHERE clause (default: `NA`)

- areasymbol:

  text string value identifying the area symbol (e.g. `IN001` or `IN%`)
  inserted into a SQL WHERE clause (default: `NA`) `NULL` (default:
  `TRUE`)

- droplevels:

  logical: indicating whether to drop unused levels in classifying
  factors. This is useful when a class has large number of unused
  classes, which can waste space in tables and figures.

- mlrassoarea:

  text string value identifying the MLRA Soil Survey Office areasymbol
  symbol inserted into a SQL WHERE clause (default: `NA`)

- fiscalyear:

  text string value identifying the fiscal year inserted into a SQL
  WHERE clause (default: `NA`)

- projecttypename:

  text string value identifying the project type name inserted into a
  SQL WHERE clause (default: `NA`)

- impute:

  replace missing (i.e. `NULL`) values with `"Not_Populated"` for
  categorical data, or the "RV" for numeric data or `201` cm if the "RV"
  is also `NULL` (default: `TRUE`)

- usiteid:

  character: User Site IDs

## Value

A data.frame or list with the results.

## Author

Stephen Roecker
