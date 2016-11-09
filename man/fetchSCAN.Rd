\name{fetchSCAN}
\alias{fetchSCAN}
\alias{SCAN_sensor_metadata}

\title{Fetch SCAN Data}
\description{Query soil/climate data from USDA-NRCS SCAN Stations (experimental)}

\usage{fetchSCAN(site.code, year, report='SCAN', req=NULL)}
\usage{SCAN_sensor_metadata(site.code)}

\arguments{
  \item{site.code}{a vector of site codes}
  \item{year}{a vector of years}
  \item{report}{report name, single value only}
  \item{req}{list of SCAN request parameters, for backwards-compatibility only}
}
\details{See \href{http://rstudio.com}{The fetchSCAN tutorial for details.}}

\value{a \code{data.frame} object}
\references{http://www.wcc.nrcs.usda.gov/scan/}
\author{D.E. Beaudette}

\examples{
\dontrun{
# get data: new interface
x <- fetchSCAN(site.code=c(356, 2072), year=c(2015, 2016))
str(x)

# get sensor metadata
m <- SCAN_sensor_metadata(site.code=c(356, 2072))
}
}
\keyword{manip}

