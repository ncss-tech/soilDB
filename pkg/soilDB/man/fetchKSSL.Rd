\name{fetchKSSL}
\alias{fetchKSSL}
\title{Fetch KSSL Data (EXPERIMENTAL)}
\description{Get KSSL data via BBOX or series query, from the SoilWeb system.}
\usage{fetchKSSL(series = NULL, bbox = NULL)}

\arguments{
  \item{series}{a soil series name}
  \item{bbox}{a bounding box in WGS84 geographic coordinates e.g. \code{c(-120, 37, -122, 38)}}
}
\details{This is an experimental interface to most of the KSSL data as of June 2013. Series-queries are case insensitive. Series name queries are based on the `correlated as` name when present. When missing, the `sampled as` name is queried.}
\value{a \code{SoilProfileCollection} object}
\references{
\url{http://ncsslabdatamart.sc.egov.usda.gov/}
}
\author{D.E. Beaudette}
\note{SoilWeb maintains a snapshot of the KSSL data. Please use the link below for the live data.}

\seealso{\code{\link{fetchOSD}}}
\examples{
# search by series name
s <- fetchKSSL(series='auburn')

# search by bounding-box
# s <- fetchKSSL(bbox=c(-120, 37, -122, 38))

# how many pedons
length(s)

# plot 
par(mar=c(0,0,0,0))
plot(s, name='hzn_desgn', max.depth=150)
}

\keyword{utilities}
