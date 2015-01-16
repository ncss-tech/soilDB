\name{SoilTaxonomyDendrogram}
\alias{SoilTaxonomyDendrogram}

\title{Soil Taxonomy Dendrogram}
\description{Plot a dendrogram based on the first 4 levels of Soil Taxonomy, with soil profiles hanging below. A dissimmilarity matrix is computed using Gower's distance metric for nominal-scale variables, based on order, sub order, great group, and subgroup level taxa. See the Details and Examples sections below for more information.}
\usage{
SoilTaxonomyDendrogram(spc, name = "hzname", max.depth = 150, 
n.depth.ticks = 6, scaling.factor = 0.015, cex.names = 0.75, 
cex.id = 0.75, axis.line.offset = -4, width = 0.1, y.offset = 0.5, 
cex.taxon.labels = 0.66)
}

\arguments{
  \item{spc}{a \code{SoilProfileCollection} object, see details}
  \item{name}{column name containing horizon names}
  \item{max.depth}{depth at which profiles are truncated for plotting}
  \item{n.depth.ticks}{suggested number of ticks on the depth axis}
  \item{scaling.factor}{scaling factor used to convert depth units into plotting units}
  \item{cex.names}{character scaling for horizon names}
  \item{cex.id}{character scaling for profile IDs}
  \item{axis.line.offset}{horizontal offset for depth axis}
  \item{width}{width of profiles}
  \item{y.offset}{vertical offset between dendrogram and profiles}
  \item{cex.taxon.labels}{character scaling for taxonomic information}
}

\details{This function looks for specific site-level attributes named: \code{soilorder}, \code{suborder}, \code{greatgroup}, and \code{subgroup}.

}

\value{An invisibly-returned list containing:
\describe{
\item{\code{dist}}{pair-wise dissimilarity matrix}
}
}

\references{
D.E. Beaudette, P. Roudier and A.T. O'Geen. 2012. Algorithms for Quantitative Pedology, a Toolkit
for Soil Scientists. Computers & Geosciences: 52: 258--268. (doi: 10.1016/j.cageo.2012.10.020)

\url{http://aqp.r-forge.r-project.org/}
}
\author{D.E. Beaudette}

\examples{
\dontrun{
library(soilDB)

# soils of interest
s.list <- c('musick', 'cecil', 'drummer', 'amador', 'pentz', 'reiff', 
'san joaquin','montpellier','grangeville','pollasky','ramona')

# fetch and convert data into an SPC
h <- fetchOSD(s.list)

# plot dendrogram + profiles
SoilTaxonomyDendrogram(h)

# again, this time save the pair-wise dissimilarity matrix
# note that there isn't a lot of discrimination between soils
(d <- SoilTaxonomyDendrogram(h))
}
}

\keyword{hplots}

