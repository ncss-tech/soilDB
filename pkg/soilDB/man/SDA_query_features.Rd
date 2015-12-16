\name{SDA_query_features}
\alias{SDA_make_spatial_query}
\alias{SDA_query_features}
\alias{processSDA_WKT}

\title{Soil Data Access Spatial Query}
\description{Iterate over Spatial* object features and submit spatial queries to the SDA web-service.}

\usage{SDA_query_features(x, id='pedon_id')}

\arguments{
  \item{x}{a \code{Spatial*} object with more than 1 feature, any defined coordinate system}
  \item{id}{the column name in \code{x} that contains a unique ID for each feature}
}

\details{The SDA website can be found at \url{http://sdmdataaccess.nrcs.usda.gov}. A detailed vignette can be found at \url{https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/SDA-tutorial.html?root=aqp}.}
\value{A dataframe containing the results.}
\author{D.E. Beaudette}
\note{This function requires the `httr`, `jsonlite`, `XML`, and `rgeos` packages}

\keyword{manip}

