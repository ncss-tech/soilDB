
# this function only works when clustering Soil Taxonomy elements
# ideally sourced from fetchOSD()
SoilTaxonomyDendrogram <- function(spc, name='hzname', max.depth=150, n.depth.ticks=6, scaling.factor=0.015, cex.names=0.75, cex.id=0.75, axis.line.offset=-4, width=0.1, y.offset=0.5, cex.taxon.labels=0.66) {
	
	# convert relevant columns into factors
	spc$soilorder <- factor(spc$soilorder)
	spc$suborder <- factor(spc$suborder)
	spc$greatgroup <- factor(spc$greatgroup)
	spc$subgroup <- factor(spc$subgroup)
	
	# extract site attributes as data.frame
	s <- site(spc)
	# copy soil ID to row.names, so that they are preserved in the distance matrix
	row.names(s) <- s[[idname(spc)]]
	
	# compute distance matrix from first 4 levels of Soil Taxonomy
	s.dist <- daisy(s[, c('soilorder', 'suborder', 'greatgroup', 'subgroup')], metric='gower')
	s.hclust <- as.hclust(diana(s.dist))
	
	# convert to phylo class
	dend <- as.phylo(s.hclust)
	
	# determine best-possible locations for taxa names
	max.dist <- max(s.dist)
	taxa.lab.y.vect <- c(max.dist / 1.6666666, (max.dist / 1.6666666) + 0.12)
	
	
	# setup plot and add dendrogram
	par(mar=c(0,0,0,0))
	plot(dend, cex=0.8, direction='up', y.lim=c(4,0), x.lim=c(0.5, length(spc)+1), show.tip.label=FALSE)
	
	# get the last plot geometry
	lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  
	# vector of indices for plotting soil profiles below leaves of dendrogram
	new_order <- s.hclust$order
	
	# plot the profiles, in the ordering defined by the dendrogram
	# with a couple fudge factors to make them fit
	plot(spc, name=name, plot.order=new_order, max.depth=max.depth, n.depth.ticks=n.depth.ticks, scaling.factor=scaling.factor, cex.names=cex.names, cex.id=cex.id, axis.line.offset=axis.line.offset, width=width, y.offset=max(lastPP$yy) + y.offset, id.style='side', add=TRUE)
	
	# generate taxonomic labels and their positions under the dendrogram
	lab <- s[new_order, 'subgroup']
	unique.lab <- unique(lab)
	group.lengths <- rle(as.numeric(lab))$lengths
	lab.x.positions <- (cumsum(group.lengths) - (group.lengths / 2)) + 0.5
	lab.y.positions <- rep(taxa.lab.y.vect, length.out=length(unique.lab))
	
	# add labels-- note manual tweaking of y-coordinates
	text(lab.x.positions, lab.y.positions, unique.lab, cex=cex.taxon.labels, adj=0.5, font=3)
	
	# invisibly return some information form the original objects
	invisible(list(dist=s.dist))
}
