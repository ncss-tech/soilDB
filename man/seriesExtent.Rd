\name{seriesExtent}
\alias{seriesExtent}
\title{Get/Display Soil Series Extent}
\description{Get or display the spatial extent of a named soil series using the Series Extent Explorer.}
\usage{
seriesExtent(s, timeout=60)
}
\arguments{
	\item{s}{the soil series name}
	\item{timeout}{time that we are willing to wait for a response, in seconds}
}
\details{Soil series extent data are downloaded from a static cache of GeoJSON files on SoilWeb servers. Cached data are typically updated annually.}

\value{when calling \code{seriesExtent}, a \code{SpatialPolygonsDataFrame} object}

\references{http://casoilresource.lawr.ucdavis.edu/see}

\author{D.E. Beaudette}

\note{This function require the `rgdal` package.}

\examples{
\donttest{
# fetch series extent for the 'Amador' soil series
s <- seriesExtent('amador')

# plot SpatialPolygonsDataFrame
if(require(sp))
  plot(s)

}
}
\keyword{manip}
