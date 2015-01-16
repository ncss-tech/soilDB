## Ideas:
# http://bommaritollc.com/2012/06/17/summary-community-detection-algorithms-igraph-0-6/?utm_source=rss&utm_medium=rss&utm_campaign=summary-community-detection-algorithms-igraph-0-6
# http://stackoverflow.com/questions/9471906/what-are-the-differences-between-community-detection-algorithms-in-igraph

## TODO: community evaluation will crash with most community detection algorithms:
#     examples: plano, miami

## NOTE: dendrogram representation of community structure is only possible with some community detection algorithms

.maximum.spanning.tree <- function(x){
  # convert cost representation of weights to "strength"
  E(x)$weight <- -1 * E(x)$weight
  # compute min spanning tree on "strength"
  x <- minimum.spanning.tree(x)
  # convert "strength" representation of weights back to cost
  E(x)$weight <- -1 * E(x)$weight
  return(x)
}

# dendrogram representation relies on ape plotting functions
# ... are passed onto plot.igraph or plot.phylo
plotSoilRelationGraph <- function(m, s='', plot.style='network', spanning.tree=NULL, del.edges=NULL, vertex.scaling.factor=2, edge.scaling.factor=1, edge.transparency=1, edge.col=grey(0.5), edge.highlight.col='royalblue', g.layout=layout.fruchterman.reingold, ...) {
	
  # dumb hack to make R CMD check happy
  weight <- NULL
  
	# generate graph
	g <- graph.adjacency(m, mode='upper', weighted=TRUE)
  
  # optionally prune weak edges less than threshold quantile
  if(!is.null(del.edges))
	  g <- delete.edges(g, E(g) [ weight < quantile(weight, del.edges) ])
  
	# optionally compute min/max spanning tree
  if(! is.null(spanning.tree)) {
    # min spanning tree: not clear how this is useful
    if(spanning.tree == 'min'){
      g <- minimum.spanning.tree(g)
    }
    
    # max spanning tree: useful when loading an entire region
    if(spanning.tree == 'max'){
      g <- .maximum.spanning.tree(g)
    }
    
    ## TODO: this needs more testing
    # max spanning tree + edges with weights > n-tile added back
    if(is.numeric(spanning.tree)){
      # select edges and store weights
      es <- E(g) [ weight > quantile(weight, spanning.tree) ]
      es.w <- es$weight
      # trap ovious errors
      if(length(es.w) < 1)
        stop('no edges selected, use a smaller value for `spanning.tree`')
      
      # convert edge sequence to matrix of vertex ids
      e <- get.edges(g, es)
      # compute max spanning tree
      g.m <- .maximum.spanning.tree(g)
      
      ## TODO: there must be a better way
      # add edges and weight back one at a time
      for(i in 1:nrow(e)){
        g.m <- add.edges(g.m, e[i, ], weight=es.w[i])
      }
      # remove duplicate edges
      g <- simplify(g.m)
    }
  }
  
	# transfer names
	V(g)$label <- V(g)$name 

	# adjust size of vertex based on sqrt(degree / max(degree))
  g.degree <- degree(g)
	v.size <- sqrt(g.degree/max(g.degree)) * 10 * vertex.scaling.factor
  
  # optionally adjust edge width based on weight
  if(!missing(edge.scaling.factor))
    E(g)$width <- sqrt(E(g)$weight) * edge.scaling.factor
  
	# community metrics
	g.com <- fastgreedy.community(g) ## this can crash with some networks
	g.com.length <- length(g.com)
	g.com.membership <- membership(g.com)

	# colors for communities: choose color palette based on number of communities
	if(g.com.length <= 9 & g.com.length > 2) 
		cols <- brewer.pal(n=g.com.length, name='Set1') 
	if(g.com.length < 3) 
		cols <- brewer.pal(n=3, name='Set1')
	if(g.com.length > 9)
		cols <- colorRampPalette(brewer.pal(n=9, name='Set1'))(g.com.length)
	
	# set colors based on community membership
	cols.alpha <- alpha(cols, 0.65)
	V(g)$color <- cols.alpha[membership(g.com)]
  
  # get an index to edges associated with series specified in 's'
  el <- get.edgelist(g)
	idx <- unique(c(which(el[, 1] == s), which(el[, 2] == s)))
	
	# set default edge color
  E(g)$color <- edge.col
	# set edge colors based on optional series name to highlight
  E(g)$color[idx] <- alpha(edge.highlight.col, edge.transparency)
  
  # previous coloring of edges based on in/out community
  # E(g)$color <- alpha(c('grey','black')[crossing(g.com, g)+1], edge.transparency)
  
	# generate vector of fonts, highlighting main soil
	font.vect <- rep(1, times=length(g.com.membership))
	font.vect[which(names(g.com.membership) == s)] <- 2
	
	if(plot.style == 'network') {
		set.seed(1010101) # consistant output
		plot(g, layout=g.layout, vertex.size=v.size, vertex.label.color='black', vertex.label.font=font.vect, ...)
		}
	if(plot.style == 'dendrogram') {
		dendPlot(g.com, label.offset=0.1, font=font.vect, col='black', colbar=cols, ...)
		}
  
  # invisibly return the graph
  invisible(g)
}
