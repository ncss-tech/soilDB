\name{fetchOSD}
\alias{fetchOSD}

\title{Fetch Data by Soil Series Name}

\description{}

\usage{
fetchOSD(soils, colorState = 'moist', extended=FALSE)
}

\arguments{
  \item{soils}{}
  \item{colorState}{}
  \item{extended}{}
}

\details{

Related documentation can be found in the following tutorials
\itemize{
  \item{\href{http://ncss-tech.github.io/AQP/soilDB/soil-series-query-functions.html}{overview of all soil series query functions}}
  
	\item{\href{https://ncss-tech.github.io/AQP/soilDB/competing-series.html}{competing soil series}}
	
	\item{\href{https://ncss-tech.github.io/AQP/soilDB/siblings.html}{siblings}}
	}


The standard set of "site" and "horizon" data are returned as a \code{SoilProfileCollection} object (\code{extended=FALSE}. The "extended" suite of summary data can be requested by setting \code{extended=TRUE}. The resulting object will be a \code{list} with the following elements:)

\describe{
	\item{SPC}{\code{SoilProfileCollection} containing standards "site" and "horizon" data}
	\item{competing}{competing soil series from the SC database snapshot}
	\item{geomcomp}{empirical probabilities for geomorphic component, derrived from the current SSURGO snapshot}
	\item{hillpos}{empirical probabilities for hillslope position, derrived from the current SSURGO snapshot}
	\item{mtnpos}{empirical probabilities for mountain slope position, derrived from the current SSURGO snapshot}
	\item{terrace}{empirical probabilities for river terrace position, derrived from the current SSURGO snapshot}
	\item{flats}{empirical probabilities for flat landscapes, derrived from the current SSURGO snapshot}
	\item{pmkind}{empirical probabilities for parent material kind, derrived from the current SSURGO snapshot}
	\item{pmorigin}{empirical probabilities for parent material origin, derrived from the current SSURGO snapshot}
	\item{mlra}{empirical MLRA membership values, derrived from the current SSURGO snapshot}
	\item{climate}{experimental climate summaries from PRISM stack}
	\item{metadata}{metadata associated with SoilWeb cached summaries}
	} 


When using `extended=TRUE`, there are a couple of scenarios in which series morphology contained in `SPC` do not fully match records in the associated series summaries (e.g. `competing`).

\describe{
	\item{A query for soil series that exist entirely outside of CONUS (e.g. PALAU).}{Climate summaries are empty \code{data.frames} becuase these summaries are currently generated from PRISM. We are working on a solution.}
	
	\item{A query for data within CONUS, but OSD morphology missing due to parsing error (e.g. formatting, typos).}{Extended summaries are present but morphology missing from `SPC`. A warning is issued.}
	
	\item{A query for multiple soil series, with one more more listed as "inactive" (e.g. BREADSPRINGS).}{Extended summaries are present but morphology missing from `SPC`. A warning is issued.}
	
	} 

These last two cases are problematic for analysis that makes use of morphology and extended data, such as outlined in this tutorial on \href{https://ncss-tech.github.io/AQP/soilDB/competing-series.html}{competing soil series}.

}

\value{a \code{SoilProfileCollection} object containing basic soil morphology and taxonomic information.}

\references{
USDA-NRCS OSD search tools: \url{https://www.nrcs.usda.gov/wps/portal/nrcs/detailfull/soils/home/?cid=nrcs142p2_053587}
}

\author{D.E. Beaudette}

\note{SoilWeb maintains a snapshot of the Official Series Description data. Please use the link above for the live data.}

\seealso{
\link{OSDquery}, \link{siblings}
}

\examples{
\donttest{
if(requireNamespace("curl") &
    curl::has_internet()) {
        
    # soils of interest
    s.list <- c('musick', 'cecil', 'drummer', 'amador', 'pentz', 
    'reiff', 'san joaquin', 'montpellier', 'grangeville', 'pollasky', 'ramona')
    
    # fetch and convert data into an SPC
    s.moist <- fetchOSD(s.list, colorState='moist')
    s.dry <- fetchOSD(s.list, colorState='dry')
    
    # plot profiles
    # moist soil colors
    if(require("aqp")) {
    
      par(mar=c(0,0,0,0), mfrow=c(2,1))
      plot(s.moist, name='hzname', 
           cex.names=0.85, axis.line.offset=-4)
      plot(s.dry, name='hzname', 
           cex.names=0.85, axis.line.offset=-4)
    
      # extended mode: return a list with SPC + summary tables
      x <- fetchOSD(s.list, extended = TRUE, colorState = 'dry')
    
      par(mar=c(0,0,1,1))
      plot(x$SPC)
      str(x, 1)
    }
}
}
}

\keyword{manip}
