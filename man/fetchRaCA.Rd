% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetchRaCA.R
\name{fetchRaCA}
\alias{fetchRaCA}
\title{Get Rapid Carbon Assessment (RaCA) data}
\usage{
fetchRaCA(
  series = NULL,
  bbox = NULL,
  state = NULL,
  rcasiteid = NULL,
  get.vnir = FALSE
)
}
\arguments{
\item{series}{a soil series name; case-insensitive}

\item{bbox}{a bounding box in WGS84 geographic coordinates e.g. \code{c(-120, 37, -122, 38)}, constrained to a 5-degree block}

\item{state}{a two-letter US state abbreviation; case-insensitive}

\item{rcasiteid}{a RaCA site id (e.g. 'C1609C01')}

\item{get.vnir}{logical, should associated VNIR spectra be downloaded? (see details)}
}
\value{
{
\describe{
\item{\code{pedons}:}{a \code{SoilProfileCollection} object containing site/pedon/horizon data}
\item{\code{trees}:}{a \code{data.frame} object containing tree DBH and height}
\item{\code{veg}:}{a \code{data.frame} object containing plant species}
\item{\code{stock}:}{a \code{data.frame} object containing carbon quantities (stocks) at standardized depths}
\item{\code{sample}:}{a \code{data.frame} object containing sample-level bulk density and soil organic carbon values}
\item{\code{spectra}:}{a numeric \code{matrix} containing VNIR reflectance spectra from 350--2500 nm}
}
}
}
\description{
\strong{NOTICE:} The SoilWeb snapshot of the RaCA data has been deprecated. The latest version of the data, including values measured by the Kellogg Soil Survey Laboratory, and supporting documentation, are available here: \url{https://www.nrcs.usda.gov/resources/data-and-reports/rapid-carbon-assessment-raca}. Download link on National Agricultural Library Ag Data Commons: \url{https://data.nal.usda.gov/dataset/rapid-carbon-assessment-raca}

Get Rapid Carbon Assessment (RaCA) data by state, geographic bounding-box, RaCA site ID, or soil series query from the SoilWeb API. This interface to the data was an experimental delivery service that does not include the latest soil organic carbon (SOC) measurements.

Please use \href{https://data.nal.usda.gov/dataset/rapid-carbon-assessment-raca}{current RaCA distribution} if you need lab \emph{measured} SOC rather than SOC estimated by VNIR.

This interface will be updated sometime calendar year 2022 to include the latest soil morphology, taxonomic classification, and measured SOC values. More detailed coordinates for sample sites should also be available.
}
\details{
The VNIR spectra associated with RaCA data are quite large (each gzip-compressed VNIR spectra record is about 6.6kb), so requests for these data are disabled by default. Note that VNIR spectra can only be queried by soil series or geographic BBOX.
}
\references{
{
\url{https://data.nal.usda.gov/dataset/rapid-carbon-assessment-raca}
}
}
\seealso{
\code{\link{fetchOSD}}
}
\author{
D.E. Beaudette, USDA-NRCS staff
}
