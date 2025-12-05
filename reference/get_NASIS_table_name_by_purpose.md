# Get NASIS 7 Physical Table Names

Method generalizing concepts of NASIS 7 data model to group tables by
"purpose." Most of our more complex queries rely on tables from one or
more purposes, so individual higher-level functions might call a
function like this to identify the relevant tables from a data source.

## Usage

``` r
get_NASIS_table_name_by_purpose(
  purpose = c("metadata", "lookup", "nasis", "site", "pedon", "transect", "component",
    "vegetation", "project", "techsoilservice", "area", "soilseries", "legend",
    "mapunit", "datamapunit"),
  SS = FALSE
)
```

## Arguments

- purpose:

  character. One or more of: "metadata", "lookup", "nasis", "site",
  "pedon", "transect", "component", "vegetation", "project",
  "techsoilservice", "area", "soilseries", "legend", "mapunit",
  "datamapunit"

- SS:

  append "\_View_1" on appropriate tables? Default: FALSE

## Value

character vector of table names

## See also

createStaticNASIS

## Examples

``` r
if (FALSE) { # \dontrun{
# get the "site" table names
get_NASIS_table_name_by_purpose("site")

# get the pedon table names
get_NASIS_table_name_by_purpose("pedon", SS = TRUE)

# metadata and lookup not affected by SS argument, but site and pedon are
get_NASIS_table_name_by_purpose(c("metadata", "lookup",
                                   "site", "pedon"), SS = TRUE)
} # }
```
