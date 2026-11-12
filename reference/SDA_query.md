# Query Soil Data Access

Submit a query to the Soil Data Access (SDA) REST/JSON web-service and
return the results as a data.frame. There is a 100,000 record and 32Mb
JSON serialization limit per query. Queries should contain a WHERE
clause or JOIN condition to limit the number of rows affected /
returned. Consider wrapping calls to `SDA_query()` in a function that
can iterate over logical chunks (e.g. areasymbol, mukey, cokey, etc.).
The function
[`makeChunks()`](http://ncss-tech.github.io/soilDB/reference/makeChunks.md)
can help with such iteration. All usages of `SDA_query()` should handle
the possibility of a `try-error` result in case the web service
connection is down or if an invalid query is passed to the endpoint.

## Usage

``` r
SDA_query(q, dsn = NULL)
```

## Arguments

- q:

  character. A valid T-SQL query surrounded by double quotes.

- dsn:

  character. Default: `NULL` uses Soil Data Access remote data source
  via REST API. Alternately, `dsn` may be a file path to an SQLite
  database using the SSURGO schema, or a `DBIConnection` that has
  already been created.

## Value

A data.frame result for queries that return a single table. A list of
data.frame for queries that return multiple tables. `NULL` if result is
empty, and `try-error` on error.

## Details

The SDA website can be found at <https://sdmdataaccess.nrcs.usda.gov>
and query examples can be found at
<https://sdmdataaccess.nrcs.usda.gov/QueryHelp.aspx>. A library of query
examples can be found at
<https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=SDA-SQL_Library_Home>.

SSURGO (detailed soil survey) and STATSGO (generalized soil survey) data
are stored together within SDA. This means that queries that don't
specify an area symbol may result in a mixture of SSURGO and STATSGO
records. See the examples below and the [SDA
Tutorial](http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.md) for
details.

## See also

[`SDA_spatialQuery()`](http://ncss-tech.github.io/soilDB/reference/SDA_spatialQuery.md)

## Author

D.E. Beaudette, A.G Brown

## Examples

``` r
# \donttest{
  ## get SSURGO export date for all soil survey areas in California
  # there is no need to filter STATSGO
  # because we are filtering on SSURGO area symbols
  q <- "SELECT areasymbol, saverest FROM sacatalog WHERE areasymbol LIKE 'CA%';"
  x <- SDA_query(q)
#> single result set, returning a data.frame
  head(x)
#>   areasymbol              saverest
#> 1      CA011  9/3/2026 10:05:10 PM
#> 2      CA013 9/15/2026 11:00:47 PM
#> 3      CA021  9/3/2026 10:06:40 PM
#> 4      CA031   9/1/2026 3:03:42 PM
#> 5      CA033  9/3/2026 10:06:40 PM
#> 6      CA041  9/16/2026 8:30:43 PM


  ## get SSURGO component data associated with the
  ## Amador series / major component only
  # this query must explicitly filter out STATSGO data
  q <- "SELECT cokey, compname, comppct_r FROM legend
    INNER JOIN mapunit mu ON mu.lkey = legend.lkey
    INNER JOIN component co ON mu.mukey = co.mukey
    WHERE legend.areasymbol != 'US' AND compname = 'Amador';"

  res <- SDA_query(q)
#> single result set, returning a data.frame
  str(res)
#> 'data.frame':    64 obs. of  3 variables:
#>  $ cokey    : int  28035009 28105005 28035013 28104419 29287311 28105043 28033184 27932844 28104409 29284002 ...
#>  $ compname : chr  "Amador" "Amador" "Amador" "Amador" ...
#>  $ comppct_r: int  40 5 76 5 85 5 45 3 5 30 ...
#>  - attr(*, "SDA_id")= chr "Table"

  ## get component-level data for a specific soil survey area (Yolo county, CA)
  # there is no need to filter STATSGO because the query contains
  # an implicit selection of SSURGO data by areasymbol
  q <- "SELECT
    component.mukey, cokey, comppct_r, compname, taxclname,
    taxorder, taxsuborder, taxgrtgroup, taxsubgrp
    FROM legend
    INNER JOIN mapunit ON mapunit.lkey = legend.lkey
    LEFT OUTER JOIN component ON component.mukey = mapunit.mukey
    WHERE legend.areasymbol = 'CA113' ;"

  res <- SDA_query(q)
#> single result set, returning a data.frame
  str(res)
#> 'data.frame':    609 obs. of  9 variables:
#>  $ mukey      : int  459227 459227 459227 459226 459226 459226 459226 459226 459226 459225 ...
#>  $ cokey      : int  28579807 28579808 28579809 28579801 28579802 28579803 28579804 28579805 28579806 28579796 ...
#>  $ comppct_r  : int  10 30 60 5 2 3 5 40 45 10 ...
#>  $ compname   : chr  "Rock land" "Millsholm" "Dibble" "Balcom" ...
#>  $ taxclname  : chr  NA "Loamy, mixed, thermic Lithic Xerochrepts" "Fine, montmorillonitic, thermic Typic Haploxeralfs" NA ...
#>  $ taxorder   : chr  NA "Inceptisols" "Alfisols" NA ...
#>  $ taxsuborder: chr  NA "Ochrepts" "Xeralfs" NA ...
#>  $ taxgrtgroup: chr  NA "Xerochrepts" "Haploxeralfs" NA ...
#>  $ taxsubgrp  : chr  NA "Lithic Xerochrepts" "Typic Haploxeralfs" NA ...
#>  - attr(*, "SDA_id")= chr "Table"

  ## get tabular data based on result from spatial query
  # there is no need to filter STATSGO because
  # SDA_Get_Mukey_from_intersection_with_WktWgs84() implies SSURGO
  p <- wk::as_wkt(wk::rct(-120.9, 37.7, -120.8, 37.8))
  q <- paste0("SELECT mukey, cokey, compname, comppct_r FROM component
      WHERE mukey IN (SELECT DISTINCT mukey FROM
      SDA_Get_Mukey_from_intersection_with_WktWgs84('", p,
       "')) ORDER BY mukey, cokey, comppct_r DESC")

   x <- SDA_query(q)
#> single result set, returning a data.frame
   str(x)
#> 'data.frame':    341 obs. of  4 variables:
#>  $ mukey    : int  462527 462527 462527 462554 462554 462554 462555 462555 462555 462558 ...
#>  $ cokey    : int  29287327 29287328 29287329 29287237 29287238 29287239 29287234 29287235 29287236 29287228 ...
#>  $ compname : chr  "Alamo" "Madera" "San Joaquin" "Corning" ...
#>  $ comppct_r: int  85 10 5 85 5 10 85 5 10 85 ...
#>  - attr(*, "SDA_id")= chr "Table"
# }
```
