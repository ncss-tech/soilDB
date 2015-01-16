\name{plotSoilRelationGraph}
\alias{plotSoilRelationGraph}
\title{Plot a component relation graph}
\description{Plot a component relation graph based on an adjacency or similarity matrix.}
\usage{
plotSoilRelationGraph(m, s='', plot.style='network', spanning.tree=NULL, 
del.edges=NULL, vertex.scaling.factor=2, edge.scaling.factor=1, 
edge.transparency=1, edge.col=grey(0.5), edge.highlight.col='royalblue', 
g.layout=layout.fruchterman.reingold,
...)
}

\arguments{
  \item{m}{adjacency matrix}
  \item{s}{central component; an empty character string is interpreted as no central component}
  \item{plot.style}{plot style ('network', or 'dendrogram')}
  \item{spanning.tree}{plot the minimim or maximum spaning tree ('min', 'max'), or, max spanning tree plus edges with weight greater than the n-th quantile specified in `spanning.tree`. See details and examples.}
  \item{del.edges}{optionally delete edges with weights less than the specified quantile (0-1)}
  \item{vertex.scaling.factor}{scaling factor applied to vertex size}
  \item{edge.scaling.factor}{optional scaling factor applied to edge width}
  \item{edge.transparency}{optional transparency setting for edges (0-1)}
  \item{edge.col}{edge color, applied to all edges}
  \item{edge.highlight.col}{edge color applied to all edges connecting to component named in \code{s}}
  \item{g.layout}{an igraph layout function, defaults to \code{layout.fruchterman.reingold}}
  \item{\dots}{further arguments passed to plotting function}
}

\note{This function is a work in progress, ideas welcome.}

\details{Vertex size is based on a normalized index of connectivity: size = sqrt(degree(g)/max(degree(g))) * scaling.factor. Edge width can be optionally scaled by edge weight by specifying an \code{edge.scaling.factor} value. The maximum spanning tree represents a sub-graph where the sum of edge weights are maximized. The mimimum spanning tree represents a sub-graph where the sum of edge weights are minimized. The maximum spanning tree is likely a more useful simplification of the full graph, in which only the strongest relationships (e.g. most common co-occurrences) are preserved.

The maximum spanning tree + edges with weights > n-th quantile is an experimental hybrid. The 'backbone' of the graph is created by the maximum spanning tree, and augmented by 'strong' auxillary edges-- defined by a value between 0 and 1.}

\value{an igraph `graph` object is invisibly returned}

\author{D.E. Beaudette}


\examples{
# load sample data set
data(amador)

# create weighted adjacency matrix (see ?component.adj.matrix for details)
m <- component.adj.matrix(amador)

# plot network diagram, with Amador soil highlighted
plotSoilRelationGraph(m, s='amador')

# dendrogram representation
plotSoilRelationGraph(m, s='amador', plot.style='dendrogram')

# compare methods
m.o <- component.adj.matrix(amador, method='occurrence')

par(mfcol=c(1,2))
plotSoilRelationGraph(m, s='amador', plot.style='dendrogram')
title('community matrix')
plotSoilRelationGraph(m.o, s='amador', plot.style='dendrogram')
title('occurence')

# investigate max spanning tree
plotSoilRelationGraph(m, spanning.tree='max')

# investigate max spanning tree + edges with weights > 75-th pctile
plotSoilRelationGraph(m, spanning.tree=0.75)

\dontrun{
# get similar data from soilweb, for the Pardee series
s <- 'pardee'
u <- url(URLencode(paste(
'http://casoilresource.lawr.ucdavis.edu/soil_web/reflector_api/soils.php?',
'what=soil_series_component_query&q_string=', s, sep='')))

# fetch data
d <- read.table(u, sep='|', header=TRUE, stringsAsFactors=FALSE)

# normalize component names
d$compname <- tolower(d$compname)

# keep only major components
d <- subset(d, subset=compkind == 'Series')

# build adj. matrix and plot
m <- component.adj.matrix(d)
plotSoilRelationGraph(m, s=s, plot.style='dendrogram')

# alter plotting style, see ?plot.phylo
plotSoilRelationGraph(m, s=s, plot.style='dendrogram', type='fan')
plotSoilRelationGraph(m, s=s, plot.style='dendrogram', type='unrooted', use.edge.length=FALSE)
}
}

\keyword{hplot}

