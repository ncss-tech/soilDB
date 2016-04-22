\name{fetchKSSL}
\alias{fetchKSSL}
\title{Fetch KSSL Data (EXPERIMENTAL)}
\description{Get KSSL data via BBOX, MLRA, or series name query, from the SoilWeb system.}
\usage{fetchKSSL(series=NULL, bbox=NULL, mlra=NULL, pedlabsampnum=NULL, 
pedon_id=NULL, pedon_key=NULL, returnMorphologicData=FALSE)}

\arguments{
  \item{series}{a soil series name, case insensitive}
  \item{bbox}{a bounding box in WGS84 geographic coordinates e.g. \code{c(-120, 37, -122, 38)}}
  \item{mlra}{an MLRA ID}
  \item{pedlabsampnum}{KSSL pedon lab sample number}
  \item{pedon_id}{user pedon ID}
  \item{pedon_key}{KSSL internal pedon ID}
  \item{returnMorphologicData}{optionally request basic morphologic data, see details section}
}

\details{This is an experimental interface to most of the KSSL data as of December 2015. Series-queries are case insensitive. Series name is based on the "correlated as" field (from KSSL snapshot) when present.  The "sampled as" classification was promoted to "correlated as" if the "correlated as" classification was missing.

When \code{returnMorphologicData} is TRUE, the resulting object is a list. The standard output from \code{fetchKSSL} (\code{SoilProfileCollection} object) is stored in the named element "SPC". The additional elements are basic morphologic data, from a NASIS-derived snapshot. There is a 1:many relationship between the horizon data in "SPC" and the additional dataframes. See examples for ideas on how to "flatten" these tables.
}

\value{a \code{SoilProfileCollection} object when \code{returnMorphologicData} is FALSE, otherwise a list.}

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

# get morphologic data too
s <- fetchKSSL(series='amador', returnMorphologicData = TRUE)

# extract SPC
pedons <- s$SPC

# simplify color data
s.colors <- simplifyColorData(s$morph$phcolor, id.var = 'labsampnum')

}
}

\keyword{utilities}
