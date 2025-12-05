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

## Note

This function requires the `httr`, `jsonlite`, and `xml2` packages

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
#>   areasymbol             saverest
#> 1      CA011  9/3/2025 3:58:54 PM
#> 2      CA013  9/2/2025 5:27:45 PM
#> 3      CA021  9/3/2025 3:59:54 PM
#> 4      CA031  9/2/2025 3:35:31 PM
#> 5      CA033  9/3/2025 4:01:24 PM
#> 6      CA041 8/29/2025 3:33:01 PM


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
#>  $ cokey    : int  26328128 26328136 26328145 26328146 26327297 26327641 26327786 26328051 26328055 26397063 ...
#>  $ compname : chr  "Amador" "Amador" "Amador" "Amador" ...
#>  $ comppct_r: int  76 30 15 40 45 3 3 7 25 3 ...
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
#>  $ mukey      : int  757748 757748 757748 757748 757749 757749 757749 757749 757749 757750 ...
#>  $ cokey      : int  26806713 26806714 26806715 26806716 26806717 26806718 26806719 26806720 26806721 26806722 ...
#>  $ comppct_r  : int  80 2 8 10 80 1 5 10 4 80 ...
#>  $ compname   : chr  "Scribner" "Unnamed" "Corbiere" "Vina" ...
#>  $ taxclname  : chr  "Fine-loamy, mixed, superactive, thermic Cumulic Endoaquolls" NA "Fine, mixed, superactive, thermic Cumulic Vertic Endoaquolls" "Coarse-loamy, mixed, superactive, thermic Pachic Haploxerolls" ...
#>  $ taxorder   : chr  "Mollisols" NA "Mollisols" "Mollisols" ...
#>  $ taxsuborder: chr  "Aquolls" NA "Aquolls" "Xerolls" ...
#>  $ taxgrtgroup: chr  "Endoaquolls" NA "Endoaquolls" "Haploxerolls" ...
#>  $ taxsubgrp  : chr  "Cumulic Endoaquolls" NA "Cumulic Vertic Endoaquolls" "Pachic Haploxerolls" ...
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
#>  $ cokey    : int  26733839 26733840 26733841 26733929 26733930 26733931 26733932 26733933 26733934 26733938 ...
#>  $ compname : chr  "Alamo" "Madera" "San Joaquin" "Corning" ...
#>  $ comppct_r: int  85 10 5 85 5 10 85 5 10 85 ...
#>  - attr(*, "SDA_id")= chr "Table"
# }
```
