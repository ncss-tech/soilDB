\name{seriesExtent}
\alias{seriesExtent}
\alias{seriesExtentAsGmap}
\title{Get/Display Soil Series Extent}
\description{Get or display the spatial extent of a named soil series using the SoilWeb query facility.}
\usage{seriesExtent(s, timeout=60)}
\usage{seriesExtentAsGmap(s, timeout=60, exp=1.25)}
\arguments{
	\item{s}{the soil series name}
	\item{timeout}{time that we are willing to wait for a response, in seconds}
	\item{exp}{expansion factor used to expand Google Maps region}
}
\details{These functions are only useful for querying the }

\value{when calling \code{seriesExtent}, a SpatialPolygonsDataFrame object}

\references{http://casoilresource.lawr.ucdavis.edu/}

\author{D. E. Beaudette}

\note{These function require the `rgdal` and `dismo` packages.}

\examples{
	# fetch series extent for the 'Amador' soil series
	\dontrun{
		s <- seriesExtent('amador')
	}
	 
	 # fetch then plot the extent of the 'Amador' soil series
	 \dontrun{
	 	seriesExtentAsGmap('amador')
	 }
	 
}

\keyword{manip}
