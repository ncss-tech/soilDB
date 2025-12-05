# Get Soil Data Access, Lab Data Mart and Web Soil Survey Usage Metrics

Obtain pre-calculated tabular reports of usage, activities, areas of
interest (AOI), exports, ecological sites, ratings and reports for
specific areas, times and intervals.

## Usage

``` r
get_SDA_metrics(query_name, query_frequency, query_year, state = NULL)
```

## Arguments

- query_name:

  One or more of: `'LDM_Usage'`, `'SDA_Usage'`, `'wss_ActivityCounts'`,
  `'wss_AOIDefinition'`, `'wss_AOISizeRange'`, `'wss_ExportCounts'`,
  `'wss_PrintableOutput'`, `'wss_top100AOIs'`,
  `'wss_top100Ecologicalsites'`, `'wss_top100ratings'`,
  `'wss_top100reports'`

- query_frequency:

  One or more of: `'M'`, `'CY'`, `'FY'`

- query_year:

  Integer. One or more years e.g. `2020:2021`

- state:

  Optional: State abbreviation; Default: `NULL` uses `"xnational"` for
  all states.

## Value

A `data.frame` containing query results

## Author

Jason Nemecek

## Examples

``` r
if (FALSE) { # \dontrun{
get_SDA_metrics('SDA_Usage', 'CY', 2019:2021)
} # }
```
