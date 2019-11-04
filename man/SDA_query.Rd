\name{SDA_query}
\alias{SDA_query}
\alias{makeChunks}
\alias{format_SQL_in_statement}

\title{Soil Data Access Query}
\description{Submit a query to the Soil Data Acccess (SDA) website in SQL, get the results as a dataframe.}

\usage{
SDA_query(q)
makeChunks(ids, size=100)
format_SQL_in_statement(x)
}

\arguments{
  \item{q}{a valid T-SQL query surrounded by double quotes}
  \item{ids}{vector of IDs for chunking, contents aren't used just length}
  \item{size}{target chunk size}
  \item{x}{character vector to be packed into an SQL `IN` statement}
}

\details{The SDA website can be found at \url{http://sdmdataaccess.nrcs.usda.gov} and query examples can be found at \url{http://sdmdataaccess.nrcs.usda.gov/QueryHelp.aspx}. A library of query examples can be found at \url{https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=SDA-SQL_Library_Home}.

SSURGO (detailed soil survey) and STATSGO (generalized soil survey) data are stored together within SDA. This means that queries that don't specify an area symbol may result in a mixture of SSURGO and STATSGO records. See the examples below and the \href{http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.html}{SDA Tutorial} for details.
}

\value{A dataframe containing the results. NULL is retutned when queries result in 0 matches rows.}
\author{D.E. Beaudette}
\note{This function requires the `httr`, `jsonlite`, and `XML` packages}

\seealso{\code{\link{mapunit_geom_by_ll_bbox}}}

\examples{
\donttest{

## get SSURGO export date for all soil survey areas in California
# there is no need to filter STATSGO 
# because we are filtering on SSURGO areasymbols
q <- "SELECT areasymbol, saverest FROM sacatalog WHERE areasymbol LIKE 'CA\%';"
x <- SDA_query(q)
head(x)


## get SSURGO component data associated with the 
## Amador series / major component only
# this query must explicitly filter out STATSGO data
q <- "SELECT cokey, compname, comppct_r 
FROM legend
INNER JOIN mapunit mu ON mu.lkey = legend.lkey
INNER JOIN component co ON mu.mukey = co.mukey 
WHERE 
-- critical: filter out STATSGO data
legend.areasymbol != 'US'
AND compname = 'Amador' ;"

res <- SDA_query(q)
str(res)


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
str(res)


## get tabular data based on result from spatial query
# there is no need to filter STATSGO because
# SDA_Get_Mukey_from_intersection_with_WktWgs84() implies SSURGO
#
# requires raster and rgeos packages because
library(raster) # suggested by soilDB
library(rgeos)  # additional

# text -> bbox -> WKT
# xmin, xmax, ymin, ymax
b <- c(-120.9, -120.8, 37.7, 37.8)
p <- writeWKT(as(extent(b), 'SpatialPolygons'))
q <- paste0("SELECT mukey, cokey, compname, comppct_r
            FROM component 
            WHERE mukey IN (
            SELECT DISTINCT mukey 
            FROM SDA_Get_Mukey_from_intersection_with_WktWgs84('", p, "') 
            )
            ORDER BY mukey, cokey, comppct_r DESC")

x <- SDA_query(q)
str(x)
}
}

\keyword{manip}

