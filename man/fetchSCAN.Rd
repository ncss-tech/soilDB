\name{fetchSCAN}
\alias{fetchSCAN}
\title{Fetch SCAN Data}
\description{Query soil/climate data from USDA-NRCS SCAN Stations (experimental)}
\usage{fetchSCAN(site.code, year, report='SCAN', req=NULL)}

\arguments{
  \item{site.code}{a vector of site codes}
  \item{year}{a vector of years}
  \item{report}{report name, single value only}
  \item{req}{list of SCAN request parameters, for backwards-compatibility only}
}
\details{An attempt is made to parse the column names from the data returned from the SCAN webservice. The data returned will depend on the report submitted. Column names contain the code (I, D, H): 

Daily sensors (e.g. TAVG.D-1) report a summary value for the previous day.  Hourly sensors (e.g. TAVG.H-1) report a summary value for the previous hour.  Instantaneous sensors (e.g. TOBS.I-1) report a single observation on the hour.

Some of the available reports include: SCAN, ALL, SOIL, SMS, STO, PREC, WEATHER, WIND, SOLAR.

See examples, details pending.}

\value{a \code{data.frame} object}
\references{http://www.wcc.nrcs.usda.gov/scan/}
\author{D.E. Beaudette}
\note{This is still an experimental function.}

\examples{
\dontrun{
# get data: new interface
x <- fetchSCAN(site.code=c(356, 2072), year=c(2015, 2016))
}
}
\keyword{manip}

