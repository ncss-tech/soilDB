\name{hillslope.probability}
\alias{hillslope.probability}
\title{Hillslope Probability via SDA}
\description{Hillslope position probability estimates from the SDA query service (SSURGO)}
\usage{hillslope.probability(s)}
\arguments{
  \item{s}{a character vector of soil series names, in lower-case}
}

\details{This function constructs and executes a query that is sent to the \href{SDA}{http://sdmdataaccess.nrcs.usda.gov} webservice. Further information on the SDA webservice and query examples can be found at \url{http://sdmdataaccess.nrcs.usda.gov/QueryHelp.aspx}}

\value{A \code{data.frame} object with rows representing soil series, and columns representing probability estimates of that series occuring along the standard 2D hillslope positions.}

\note{Probability values are computed from SSURGO data.}

\author{D.E. Beaudette}

\examples{
\dontrun{
library(soilDB)

# soil series of interest
s <- c('auburn', 'pardee', 'amador', 'pentz')

# generate hillslope probability table
hillslope.probability(s)
}
}

\keyword{manip}

