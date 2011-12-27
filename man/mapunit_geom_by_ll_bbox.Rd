\name{mapunit_geom_by_ll_bbox}
\alias{mapunit_geom_by_ll_bbox}
\title{Fetch Map Unit Geometry from SDA}
\description{Fetch map unit geometry from the SDA website by WGS84 bounding box.}
\usage{
mapunit_geom_by_ll_bbox(bbox, source = 'sda')
}
\arguments{
  \item{bbox}{a bounding box in WGS coordinates}
  \item{source}{the source database, currently limited to soil data access (SDA)}
}
\details{The SDA website can be found at \url{http://sdmdataaccess.nrcs.usda.gov}. See examples for bounding box formatting.}
\value{A SpatialPolygonsDataFrame of map unit polygons, in WGS84 (long,lat) coordinates.}
\references{http://casoilresource.lawr.ucdavis.edu/}
\author{Dylan E Beaudette}
\note{It appears that SDA does not actually return the spatial intersecion of map unit polygons and bounding box. Rather, just those polygons that are completely within the bounding box / overlap with the bbox. This function requires the `rgdal` (\url{http://cran.r-project.org/web/packages/rgdal/index.html}) package.}
\seealso{}
\examples{
# fetch map unit geometry from a bounding-box:
# 
#         +------------- (-120.41, 38.70)
#         |                     |
#         |                     |
# (-120.54, 38.61) --------------+

\dontrun{
# basic usage
b <- c(-120.54,38.61,-120.41,38.70)
x <- mapunit_geom_by_ll_bbox(b) # about 20 seconds
plot(x)

# get map unit data for matching map unit keys
in.statement <- format_SQL_in_statement(unique(x$MUKEY))
q <- paste("SELECT mukey, muname FROM mapunit WHERE mukey IN ", in.statement, sep="")
res <- SDA_query(q)
}
}
\keyword{manip}




