\name{sample.by.poly}
\alias{sample.by.poly}

\title{Sample by Polygon}
\description{Sample by polygon, according to a specified sampling density}
\usage{sample.by.poly(p, n.pts.per.ac = 1, min.samples = 5, 
sampling.type = "hexagonal", iterations = 10, ...)}

\arguments{
  \item{p}{a Polygon object, with coordinates from a projected CRS}
  \item{n.pts.per.ac}{requested sampling density in points per acre (results will be close)}
  \item{min.samples}{minimum requested samples / polygon}
  \item{sampling.type}{sampling type, see \code{spsample}}
  \item{iterations}{number of tries that \code{spsample} will attempt}
  \item{\dots}{additional arguments to \code{spsample}}
}
\details{This function is typically used within the context of a loop or similar function that iterates over a list of Polygons associated with a \code{SpatialPolygons} object. See examples.}
\value{A \code{SpatialPoints} object.}
\author{D.E. Beaudette}
\note{This function expects that the \code{Polygon} object has coordinates associated with a projected CRS-- e.g. units of meters.}

\keyword{manip}
