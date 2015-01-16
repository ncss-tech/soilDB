\name{SSURGO_spatial_query}
\alias{SSURGO_spatial_query}

\title{Get SSURGO Data via Spatial Query}

\description{Get SSURGO Data via Spatial Query}
\usage{
SSURGO_spatial_query(bbox = NULL, coords = NULL, what = "mapunit", source = "soilweb")
}

\arguments{
  \item{bbox}{a bounding box in WGS84 geographic coordinates, see examples}
  \item{coords}{a coordinate pair in WGS84 geographic coordinates, see examples}
  \item{what}{data to query, currently ignored}
  \item{source}{the data source, currently ignored}
}

\note{This function should be considered experimental; arguments, results, and side-effects could change at any time.}

\details{Data are currently available from SoilWeb. These data are a snapshot of the "official" data. The snapshot date is encoded in the "soilweb_last_update" column in the function return value. Planned updates to this function will include a switch to determine the data source: "official" data via USDA-NRCS servers, or a "snapshot" via SoilWeb.}

\value{The data returned from this function will depend on the query style. See examples below.}

\author{D.E. Beaudette}

\examples{
# query by bbox
\dontrun{SSURGO_spatial_query(bbox=c(-122.05, 37, -122, 37.05))}

# query by coordinate pair
\dontrun{SSURGO_spatial_query(coords=c(-121, 38))}
}

\keyword{manip}

