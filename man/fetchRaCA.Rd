\name{fetchRaCA}
\alias{fetchRaCA}
\title{Fetch KSSL Data (EXPERIMENTAL)}
\description{Get Rapid Carbon Assessment (RaCA) data via state, geographic bounding-box, RaCA site ID, or series query from the SoilWeb system.}
\usage{fetchRaCA(series = NULL, bbox = NULL, state = NULL, rcasiteid = NULL, get.vnir = FALSE)}

\arguments{
  \item{series}{a soil series name, case insensitive}
  \item{bbox}{a bounding box in WGS84 geographic coordinates e.g. \code{c(-120, 37, -122, 38)}, constrained to a 5-degree block}
  \item{state}{a two-letter US state abbreviation, case insensitive}
  \item{rcasiteid}{an RaCA site id (e.g. 'C1609C01')}
  \item{get.vnir}{boolean, should associated VNIR spectra be downloaded? (see details)}
}

\value{
 \describe{
    \item{\code{pedons}:}{a \code{SoilProfileCollection} object containing site/pedon/horizon data}
    \item{\code{trees}:}{a \code{data.frame} object containing tree DBH and height}
    \item{\code{veg}:}{a \code{data.frame} object containing plant species}
    \item{\code{stock}:}{a \code{data.frame} object containing carbon quantities (stocks) at standardized depths}
    \item{\code{sample}:}{a \code{data.frame} object containing sample-level bulk density and soil organic carbon values}
    \item{\code{spectra}:}{a numeric \code{matrix} containing VNIR reflectance spectra from 350--2500 nm}
  }
}

\details{The VNIR spectra associated with RaCA data are quite large [each gzip-compressed VNIR spectra record is about 6.6kb], so requests for these data are disabled by default. Note that VNIR spectra can only be queried by soil series or geographic BBOX.}

\references{
  \url{http://soils.usda.gov/survey/raca/index.html}
  \href{https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/RaCA-demo.html?root=aqp}{fetchRaCA() Tutorial}
}
\author{D.E. Beaudette, USDA-NRCS staff}

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
