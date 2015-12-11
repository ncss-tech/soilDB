\name{fetchKSSL}
\alias{fetchKSSL}
\title{Fetch KSSL Data (EXPERIMENTAL)}
\description{Get KSSL data via BBOX, MLRA, or series name query, from the SoilWeb system.}
\usage{fetchKSSL(series=NULL, bbox=NULL, mlra=NULL, pedlabsampnum=NULL, 
pedon_id=NULL, pedon_key=NULL)}

\arguments{
  \item{series}{a soil series name, case insensitive}
  \item{bbox}{a bounding box in WGS84 geographic coordinates e.g. \code{c(-120, 37, -122, 38)}}
  \item{mlra}{an MLRA ID}
  \item{pedlabsampnum}{KSSL pedon lab sample number}
  \item{pedon_id}{user pedon ID}
  \item{pedon_key}{KSSL internal pedon ID}
}

\details{This is an experimental interface to most of the KSSL data as of June 2015. Series-queries are case insensitive. Series name is based on the "correlated as" field (from KSSL snapshot) when present.  The "sampled as" classification was promoted to "correlated as" if the "correlated as" classification was missing.}

\value{a \code{SoilProfileCollection} object}

\author{D.E. Beaudette}
\note{SoilWeb maintains a snapshot of the KSSL data. The SoilWeb snapshot was developed using methods described here: \url{https://github.com/dylanbeaudette/process-kssl-snapshot}. Please use the link below for the live data.}

\references{
\url{http://ncsslabdatamart.sc.egov.usda.gov/}
}

\seealso{\code{\link{fetchOSD}}}
\examples{
\dontrun{
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
}

\keyword{utilities}
