\name{fetchOSD}
\alias{fetchOSD}

\title{Fetch Official Series Description (OSD) Data}

\description{This functions fetches a limited subset of horizon and site-level attributes for named soil series, from the SoilWeb system.}

\usage{
fetchOSD(soils, colorState = 'moist', extended=FALSE)
}

\arguments{
  \item{soils}{a character vector of named soil series, case insensitive}
  \item{colorState}{color state for horizon soil color visualization: "moist" or "dry"}
  \item{extended}{if TRUE additional soil series level summary data are returned, see details}
  
  \item{stringsAsFactors}{logical: should character vectors be converted to factors? This argument is passed to the uncode() function. It does not convert those vectors that have set outside of uncode() (i.e. hard coded). The 'factory-fresh' default is TRUE, but this can be changed by setting options(stringsAsFactors = FALSE)}
}

\details{


\describe{
	\item{SPC}{}
	\item{geomcomp}{}
	\item{hillpos}{}
	\item{mtnpos}{}
	\item{pmkind}{}
	\item{pmorigin}{}
	\item{mlra}{}
	} 

}

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

# extended mode: return a list with SPC + summary tables
x <- fetchOSD(s.list, extended = TRUE, colorState = 'dry')

par(mar=c(0,0,1,1))
plot(x$SPC)

str(x, 1)

}
}

\keyword{manip}
