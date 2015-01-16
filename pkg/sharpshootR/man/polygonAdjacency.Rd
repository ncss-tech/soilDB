\name{polygonAdjacency}
\alias{polygonAdjacency}
\title{Evaluate Spatial Adjacency of SpaitialPolygonsDataFrame Objects}
\description{This function utilizes the `spdep` and `igraph` packages to evaluate several measures of spatial connectivity.}
\usage{
polygonAdjacency(x, v = "MUSYM")
}

\arguments{
  \item{x}{a \code{SpatialPolygonsDataFrame} object}
  \item{v}{name of the field in the attribute table to use when searching for `common lines`, see details}
}
\details{Pending.}

\value{A \code{list} object containing:
\describe{
  \item{commonLines}{An integer vector of feature IDs, that share a common boundary and attribute \code{v}. Sometimes referred to as "common soil lines".}
  \item{adjMat}{A weighted adjacency matrix}
  }
}
\author{D.E. Beaudette}

% \note{}
\examples{
\dontrun{
library(spdep)
library(rgdal)
library(igraph)
library(sharpshootR)

# load some data
x <- readOGR(dsn='FG_CA630_OFFICIAL.gdb', layer='ca630_a', stringsAsFactors=FALSE)

# compute spatial adjacency summary
res <- polygonAdjacency(x)

# graphical check: slow for large number of features
plot(x)
plot(x[res$commonLines, ], col='red', add=TRUE)

# save to SHP
writeOGR(x[res$commonLines, ], dsn='.', 
layer='common-soil-lines', driver='ESRI Shapefile', 
overwrite_layer=TRUE)

# plot spatial adjacency information
par(mar=c(0,0,0,0))
plotSoilRelationGraph(res$adjMat, 
vertex.scaling.factor = 1)
plotSoilRelationGraph(res$adjMat, spanning.tree='max', 
edge.scaling.factor=0.1, vertex.scaling.factor=1)
}


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

