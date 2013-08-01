\name{SDA_query}
\alias{SDA_query}
\alias{cleanSDA}
\alias{format_SQL_in_statement}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Soil Data Access Query}
\description{Submit a query to the Soil Data Acccess (SDA) website in SQL, get the results as a dataframe.}
\usage{SDA_query(q)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{q}{a text string containing valid SQL}
}
\details{The SDA website can be found at \url{http://sdmdataaccess.nrcs.usda.gov} and query examples can be found at \url{http://sdmdataaccess.nrcs.usda.gov/QueryHelp.aspx}}
\value{A dataframe containing the results.}
\references{http://casoilresource.lawr.ucdavis.edu/}
\author{Dylan E. Beaudette}
\note{This function requires the `SSOAP` (\url{http://www.omegahat.org/SSOAP/}), `RCurl` (\url{http://www.omegahat.org/RCurl/}), `XML` (\url{http://www.omegahat.org/RSXML/}), and `XMLSchema` (\url{http://www.omegahat.org/XMLSchema/}) packages. Some Windows binary packages can be found here \url{http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/2.15/}. This function has been successfully tested on all Windows, MacOS, and Linux using SSOAP 0.8-1, XMLSchema 0.6-0, and XML 3.7-1.}

\seealso{\code{\link{MUKEYS_by_ll_bbox}}, \code{\link{mapunit_geom_by_ll_bbox}}}
\examples{
# install the required packages
\dontrun{
install.packages('RCurl', dep=TRUE)
install.packages('XML', dep=TRUE)
install.packages("SSOAP", repos = "http://www.omegahat.org/R", type="source")
install.packages("XMLSchema", repos = "http://www.omegahat.org/R", type="source")
}

# SSURGO export metadata:
\dontrun{
q <- "SELECT areasymbol, saverest FROM sacatalog WHERE areasymbol LIKE 'CA%';"
x <- SDA_query(q)
x$saverest <- as.Date(x$saverest, format="%m/%d/%Y")
head(x)
}

# basic query:
\dontrun{
res <- SDA_query("select cokey, compname, comppct_r 
from component 
where compname = 'yolo' and majcompflag = 'Yes' ")
}

# get tabular data based on result from spatial query:
\dontrun{
# bbox
b <- c(-120.9,37.7,-120.8,37.8)
# get map unit keys for this bbox
m <- MUKEYS_by_ll_bbox(b)
# make an SQL-compliant "in" statment
in.statement <- format_SQL_in_statement(m)
# format query:
q <- paste("
SELECT component.cokey, compname, comppct_r, hzdept_r, hzdepb_r, 
hzname, sandtotal_r, silttotal_r, claytotal_r
FROM component JOIN chorizon ON component.cokey = chorizon.cokey
WHERE majcompflag = 'Yes' AND mukey IN ", in.statement, 
"ORDER BY cokey, comppct_r DESC, hzdept_r ASC", sep="")
# now get component and horizon-level data for these map unit keys
res <- SDA_query(q)
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

}

\keyword{manip}

