\name{soilDB-package}
\alias{soilDB.env}
\alias{soilDB-package}
\alias{soilDB}
\docType{package}
\title{Soil Database Interface}
\description{This package provides methods for extracting soils information from local PedonPC and AK Site databases (MS Access format), local NASIS databases (MS SQL Server), and the SDA webservice. Currently USDA-NCSS data sources are supported, however, there are plans to develop interfaces to outside systems such as the Global Soil Mapping project.}
\details{
It can be difficult to locate all of the dependencies required for sending/processing SOAP requests, especially on UNIX-like operating systems. Windows binary packages for the dependencies can be found \href{http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/2.15/}{here}. See \code{\link{fetchPedonPC}} for a simple wrapper function that should suffice for typical site/pedon/hz queries. An introduction to the soilDB package can be found \href{https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/soilDB-Intro.html?root=aqp}{here}.
}
\author{J.M. Skovlin and D.E. Beaudette}
\keyword{package}
\seealso{\code{\link{fetchPedonPC}, \link{fetchNASIS}, \link{SDA_query}, \link{loafercreek}}}


