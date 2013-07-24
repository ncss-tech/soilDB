\name{fetchRaCA}
\alias{fetchRaCA}
\title{Fetch KSSL Data (EXPERIMENTAL)}
\description{Get Rapid Carbon Assessment (RaCA) data via BBOX or series query, from the SoilWeb system.}
\usage{fetchRaCA(series = NULL, bbox = NULL)}

\arguments{
  \item{series}{a soil series name, case insensitive}
  \item{bbox}{a bounding box in WGS84 geographic coordinates e.g. \code{c(-120, 37, -122, 38)}}
}

\value{
 \describe{
    \item{\code{pedons}:}{a \code{SoilProfileCollection} object containing site/pedon/horizon data}
    \item{\code{trees}:}{a \code{data.frame} object containing tree DBH and height}
    \item{\code{veg}:}{a \code{data.frame} object containing plant species}
    \item{\code{conc}:}{a \code{data.frame} object containing carbon concentrations at standardized depths}
    \item{\code{stock}:}{a \code{data.frame} object containing carbon quantities (stocks) at standardized depths}
  }
}
\references{
  \url{http://soils.usda.gov/survey/raca/index.html}
}
\author{D.E. Beaudette}

\seealso{\code{\link{fetchOSD}}}
\examples{
\dontrun{
  # search by series name
  s <- fetchRaCA(series='auburn')
  
  # search by bounding-box
  # s <- fetchRaCA(bbox=c(-120, 37, -122, 38))
  
  # check structure
  str(s, 1)
  
  # extract pedons
  p <- s$pedons
  
  # how many pedons
  length(p)
  
  # plot 
  par(mar=c(0,0,0,0))
  plot(p, name='hzn_desgn', max.depth=150)
}
}

\keyword{utilities}
