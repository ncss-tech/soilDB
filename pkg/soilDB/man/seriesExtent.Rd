\name{seriesExtent}
\alias{seriesExtent}
\alias{seriesExtentAsGmap}
\title{Get/Display Soil Series Extent}
\description{Get or display the spatial extent of a named soil series using the Series Extent Explorer.}
\usage{
seriesExtent(s, timeout=60)
seriesExtentAsGmap(s, timeout=60, exp=1.25)
}
\arguments{
	\item{s}{the soil series name}
	\item{timeout}{time that we are willing to wait for a response, in seconds}
	\item{exp}{expansion factor used to expand Google Maps region}
}
\details{Soil series extent data are downloaded from a static cache of GeoJSON files on SoilWeb servers. Cached data are typically updated annually.}

\value{when calling \code{seriesExtent}, a SpatialPolygonsDataFrame object}

\references{http://casoilresource.lawr.ucdavis.edu/see}

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
