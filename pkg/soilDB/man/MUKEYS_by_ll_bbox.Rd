\name{MUKEYS_by_ll_bbox}
\alias{MUKEYS_by_ll_bbox}
\title{Fetch Map Unit Keys by Bounding Box}
\description{This function will return a vector of map unit keys, from the Soil Data Access website, that correspond with a bounding box defined by WGS84 long/lat pairs.}
\usage{
MUKEYS_by_ll_bbox(bbox)
}
\arguments{
  \item{bbox}{a bounding box in WGS84 geographic coordinates}
}
\details{The SDA website can be found at \url{http://sdmdataaccess.nrcs.usda.gov}. See examples for bounding box formatting. This website can be very slow. An updated approach can be found in \code{\link{SSURGO_spatial_query}}.}
\value{a vector of map unit keys}
\references{http://casoilresource.lawr.ucdavis.edu/}
\author{Dylan E Beaudette}
\note{This function requires the `RCurl` and `XML` packages.}
\seealso{\code{\link{SDA_query}}, \code{\link{mapunit_geom_by_ll_bbox}}}
\examples{
# fetch map unit keys from the bounding-box:
# 
#         +------------- (-120.8, 37.8)
#         |                     |
#         |                     |
# (-120.95, 37.7) --------------+

b <- c(-120.9,37.7,-120.8,37.8)
\dontrun{m <- MUKEYS_by_ll_bbox(b)}
}
\keyword{manip}
