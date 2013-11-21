\name{seriesExtent}
\alias{seriesExtent}
\alias{seriesExtentAsGmap}
\title{Get/Display Soil Series Extent}
\description{Get or display the spatial extent of a named soil series using the SoilWeb query facility.}
\usage{
seriesExtent(s, timeout=60)
seriesExtentAsGmap(s, timeout=60, exp=1.25)
}
\arguments{
	\item{s}{the soil series name}
	\item{timeout}{time that we are willing to wait for a response, in seconds}
	\item{exp}{expansion factor used to expand Google Maps region}
}
\details{Soil series extent data are generated as KMZ files, and cached on the SoilWeb server. Initial creation of the KMZ extent may require 1-10+ seconds depending on the spatial extent of the series and server load. All subsequent requests for the same series will return cached data. The cache is purged whenever soil survey data are synced with the official repository.}

\value{when calling \code{seriesExtent}, a SpatialPolygonsDataFrame object}

\references{http://casoilresource.lawr.ucdavis.edu/}

\author{D.E. Beaudette}

\note{These function require the `rgdal` and `dismo` packages.}

\examples{
\dontrun{
# fetch series extent for the 'Amador' soil series
s <- seriesExtent('amador')
plot(s)
 
# fetch then plot the extent of the 'Amador' soil series
seriesExtentAsGmap('amador')
}
	 
}

\keyword{manip}
