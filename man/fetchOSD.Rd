\name{fetchOSD}
\alias{fetchOSD}

\title{Fetch Data by Soil Series Name}

\description{This functions fetches a varity of data associated with named soil series, extracted from the USDA-NRCS Official Series Description text files and detailed soil survey (SSURGO). These data are periodically updated and made available via SoilWeb.}

\usage{
fetchOSD(soils, colorState = 'moist', extended=FALSE)
}

\arguments{
  \item{soils}{a character vector of named soil series, case insensitive}
  \item{colorState}{color state for horizon soil color visualization: "moist" or "dry"}
  \item{extended}{if TRUE additional soil series summary data are returned, see details}
}

\details{
The standard set of "site" and "horizon" data are returned as a \code{SoilProfileCollection} object (\code{extended=FALSE}. The "extended" suite of summary data can be requested by setting \code{extended=TRUE}. The resulting object will be a \code{list} with the following elements:)

\describe{
	\item{SPC}{\code{SoilProfileCollection} containing standards "site" and "horizon" data}
	\item{competing}{competing soil series from the SC database snapshot}
	\item{geomcomp}{empirical probabilities for geomorphic component, derrived from the current SSURGO snapshot}
	\item{hillpos}{empirical probabilities for hillslope position, derrived from the current SSURGO snapshot}
	\item{mtnpos}{empirical probabilities for mountain slope position, derrived from the current SSURGO snapshot}
	\item{pmkind}{empirical probabilities for parent material kind, derrived from the current SSURGO snapshot}
	\item{pmorigin}{empirical probabilities for parent material origin, derrived from the current SSURGO snapshot}
	\item{mlra}{empirical MLRA membership values, derrived from the current SSURGO snapshot}
	\item{climate}{experimental climate summaries from PRISM stack}
	\item{metadata}{metadata associated with SoilWeb cached summaries}
	} 

Further details pending.
}

\value{a \code{SoilProfileCollection} object containing basic soil morphology and taxonomic information.}

\references{
USDA-NRCS OSD search tools: \url{http://www.nrcs.usda.gov/wps/portal/nrcs/detailfull/soils/home/?cid=nrcs142p2_053587}
}

\author{D.E. Beaudette}

\note{SoilWeb maintains a snapshot of the Official Series Description data. Please use the link above for the live data.}

\seealso{
\link{OSDquery}, \link{siblings}
}

\examples{
\donttest{
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
