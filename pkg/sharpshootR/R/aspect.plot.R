# plot a vector of aspect measurements, with units of degrees, measured via compass
aspect.plot <- function(p, q=c(0.05, 0.5, 0.95), p.bins=60, p.bw=30, stack=TRUE, p.axis=seq(0, 350, by=10), plot.title=NULL, line.col='RoyalBlue', line.lwd=1, line.lty=2, arrow.col=line.col, arrow.lwd=1, arrow.lty=1, arrow.length=0.15, ...) {
	# remove NA
	p <- na.omit(p)
	
	# make a circular class object: using native degrees, template sets zero and direction
	c.p <- circular(p, units='degrees', template="geographics", modulo='2pi')
  
  # compute quantiles
	q.p <- quantile(c.p, probs=q)
  
	# compute mean resultant length
	rho.p <- rho.circular(c.p)
	
	# setup custom axis
	a.p <- circular(p.axis, units='degrees', template="geographics")
	
	# circular histogram
	plot(c.p, axes=FALSE, stack=stack, bins=p.bins, shrink=1.45, sep=0.06, ...)
	
	# add circular density, bw is th  e smoothness parameter
	lines(density(c.p, bw=p.bw), col=line.col, lty=line.lty, lwd=line.lwd)
	
	# add axes:
	axis.circular(at=a.p, labels=a.p, cex=0.6) #  buggy  in circular <= 0.4-3 (2011-07-18)	
	
	# annotate north
	text(0, 1.125, 'North', cex=0.85, font=1)
	
	# annotate stats with a arrows
	arrows.circular(q.p, shrink=rho.p, length=arrow.length, col=arrow.col, lwd=arrow.lwd, lty=arrow.lty)
	
	# add title
	text(0, -0.25, plot.title, cex=0.85, font=2)
  
  # invisibly return stats
  invisible(q.p)
}