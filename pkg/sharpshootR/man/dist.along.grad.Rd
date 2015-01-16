\name{dist.along.grad}
\alias{dist.along.grad}

\title{Compute Euclidean distance along a gradient.}
\description{This function computes Euclidean distance along points aligned to a given gradient (e.g. elevation).}
\usage{
dist.along.grad(coords, var, grad.scaled.min, grad.scaled.max)
}

\arguments{
  \item{coords}{a matrix of x and y coordinates in some projected coordinate system}
  \item{var}{a vector of the same length as \code{coords}, describing the gradient of interest}
  \item{grad.scaled.min}{min value of rescaled gradient values}
  \item{grad.scaled.max}{max value of rescaled gradient values}
}

\details{This function is primarily intended for use within \code{\link{plotTransect}}.}

\value{A \code{data.frame} object:
\describe{
  \item{scaled.grad}{scaled gradient values}
  \item{scaled.distance}{cumulative distance, scaled to the interval of \code{0.5, nrow(coords) + 0.5}}
  \item{distance}{cumulative distance computed along gradient, e.g. transect distance}
  \item{variable}{sorted gradient values}
  \item{x}{x coordinates, ordered by gradient values}
  \item{y}{y coordinate, ordered by gradient values}
  \item{grad.order}{a vector index describing the sort order defined by gradient values}
  }
}

\author{D.E. Beaudette}
\note{This function is very much a work in progress, ideas welcome.}

\seealso{\code{\link{plotTransect}}}

\keyword{manip}

