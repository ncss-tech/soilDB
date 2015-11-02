\name{SDA_query}
\alias{SDA_query}
\alias{.cleanSDA}
\alias{.oldSDA_query}
\alias{format_SQL_in_statement}
\title{Soil Data Access Query}
\description{Submit a query to the Soil Data Acccess (SDA) website in SQL, get the results as a dataframe.}
\usage{SDA_query(q)}

\arguments{
  \item{q}{a text string containing valid SQL}
}
\details{The SDA website can be found at \url{http://sdmdataaccess.nrcs.usda.gov} and query examples can be found at \url{http://sdmdataaccess.nrcs.usda.gov/QueryHelp.aspx}}
\value{A dataframe containing the results. NULL is retutned when queries result in 0 matches rows.}
\author{D.E. Beaudette}
\note{This function requires the `httr`, `jsonlite`, and `XML` packages}

\seealso{\code{\link{MUKEYS_by_ll_bbox}}, \code{\link{mapunit_geom_by_ll_bbox}}}
\examples{

# SSURGO export metadata:
\dontrun{
q <- "SELECT areasymbol, saverest FROM sacatalog WHERE areasymbol LIKE 'CA\%';"
x <- SDA_query(q)
x$saverest <- as.Date(x$saverest, format="\%m/\%d/\%Y")
head(x)
}

# basic query:
\dontrun{
res <- SDA_query("select cokey, compname, comppct_r 
from component 
where compname = 'yolo' and majcompflag = 'Yes' ")
}

# get component-level data for a specific soil survey area (Yolo county, CA)
\dontrun{
q <- "SELECT 
component.mukey, cokey, comppct_r, compname, taxclname, 
taxorder, taxsuborder, taxgrtgroup, taxsubgrp
FROM legend
INNER JOIN mapunit ON mapunit.lkey = legend.lkey
LEFT OUTER JOIN component ON component.mukey = mapunit.mukey
WHERE legend.areasymbol = 'CA113'"

res <- SDA_query(q)
}

# get tabular data based on result from spatial query:
# requires raster and rgeos packages
\dontrun{
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
}



}

\keyword{manip}

