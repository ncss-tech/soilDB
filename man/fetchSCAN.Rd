\name{fetchSCAN}
\alias{fetchSCAN}
\alias{SCAN_sensor_metadata}

\title{Fetch SCAN Data}
\description{Query soil/climate data from USDA-NRCS SCAN Stations (experimental)}

\usage{
# get SCAN data
fetchSCAN(site.code, year, report='SCAN', req=NULL)
# get SCAN station metadata
SCAN_sensor_metadata(site.code)}

\arguments{
  \item{site.code}{a vector of site codes}
  \item{year}{a vector of years}
  \item{report}{report name, single value only}
  \item{req}{list of SCAN request parameters, for backwards-compatibility only}
}
\details{See \href{http://ncss-tech.github.io/AQP/soilDB/fetchSCAN-demo.html}{The fetchSCAN tutorial for details.} These functions require the `httr` and `rvest` libraries.}

\note{\code{SCAN_sensor_metadata()} is known to crash on 32bit R / libraries (Windows).}

\value{a \code{data.frame} object}
\references{https://www.wcc.nrcs.usda.gov/index.html}
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

