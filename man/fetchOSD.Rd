\name{fetchOSD}
\alias{fetchOSD}

\title{Fetch Official Series Description (OSD) Data}
\description{This functions fetches a limited subset of horizon and site-level attributes for named soil series, from the SoilWeb system.}
\usage{
fetchOSD(soils, colorState = 'moist')
}

\arguments{
  \item{soils}{a character vector of named soil series}
  \item{colorState}{color state for horizon soil color visualization: "moist" or "dry"}
}
\details{the search is case-insensitive}
\value{a \code{SoilProfileCollection} object containing basic soil morphology and taxonomic information.}
\references{
\url{http://www.nrcs.usda.gov/wps/portal/nrcs/detailfull/soils/home/?cid=nrcs142p2_053587}
}
\author{D.E. Beaudette}
\note{SoilWeb maintains a snapshot of the Official Series Description data. Please use the link above for the live data.}


\examples{
\dontrun{
# soils of interest
s.list <- c('musick', 'cecil', 'drummer', 'amador', 'pentz', 
'reiff', 'san joaquin', 'montpellier', 'grangeville', 'pollasky', 'ramona')

# fetch and convert data into an SPC
s.moist <- fetchOSD(s.list, colorState='moist')
s.dry <- fetchOSD(s.list, colorState='dry')

# plot profiles
# moist soil colors
par(mar=c(0,0,0,0), mfrow=c(2,1))
plot(s.moist, name='hzname', cex.names=0.85, axis.line.offset=-4)
plot(s.dry, name='hzname', cex.names=0.85, axis.line.offset=-4)
}
}

\keyword{manip}
