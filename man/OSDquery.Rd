\name{OSDquery}
\alias{OSDquery}

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

\note{SoilWeb maintains a snapshot of the Official Series Description data.}

\seealso{
\code{\link{fetchOSD}}
}

\examples{
# find all series that list Pardee as a geographically associated soil.
s <- OSDquery(geog_assoc_soils = 'pardee')
# get data for these series
x <- fetchOSD(s$series, extended = TRUE, colorState = 'dry')
# simple figure
par(mar=c(0,0,1,1))
plot(x$SPC)
}

\keyword{manip}
