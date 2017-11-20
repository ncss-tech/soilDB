\name{SDA_query_features}
\alias{SDA_make_spatial_query2}
\alias{SDA_make_spatial_query}
\alias{SDA_query_features}
\alias{processSDA_WKT}

\title{Soil Data Access Spatial Query}
\description{Iterate over Spatial* object features and submit spatial queries to the SDA web-service.}

\usage{
SDA_query_features(x, id='pedon_id')
processSDA_WKT(d, g='geom', p4s='+proj=longlat +datum=WGS84')
}

\arguments{
  \item{x}{a \code{Spatial*} object with more than 1 feature, any defined coordinate system}
  \item{id}{the column name in \code{x} that contains a unique ID for each feature}
  
  \item{d}{\code{data.frame} returned by \code{SDA_query}, containing WKT representation of geometry}
  \item{g}{name of column in \code{d} containing WKT geometry}
  \item{p4s}{PROJ4 CRS defs}
}

\details{The SDA website can be found at \url{http://sdmdataaccess.nrcs.usda.gov}. See the \href{http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.html}{SDA Tutorial} for detailed examples.}

\value{A dataframe containing the results.}

\author{D.E. Beaudette}
\note{This function requires the `httr`, `jsonlite`, `XML`, and `rgeos` packages}

\keyword{manip}

