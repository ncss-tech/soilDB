# ideas: 
# spatial.median (ICSNP)
# multivariate quantiles (mvtnorm)
# http://stats.stackexchange.com/questions/23054/how-to-find-quantiles-for-multivariate-data-using-r
# it should make sense to do the quasi-multivatiate quantile estmation in additive-log-ratio space, or in the cartesian space used by soil.texture
#
# "approximation" of the 2D quantile function only uses clay+silt
# this is likely wrong, and maybe we can use ideas from aplpack::bagplot() for a more robust version


## TODO:
## 1. allow iteration over groups for plotting multiple horizons
## 2. consider embedding into lattice panels
## 3. consider integration with soiltexture package

# helper function
.get.ssc.low.rv.high <- function(ssc, p, delta) {
  # sanity checks
  if(delta < 1)
    stop('delta values smaller than 1 may result in memory overflow', call.=FALSE)
  
  # compute _univariate_ low - rv - high by variable
  ssc.stats <- apply(ssc, 2, quantile, probs=p)
  
  # re-order for plotting
  sand.stats <- sort(ssc.stats[, 1])
  silt.stats <- sort(ssc.stats[, 2])
  clay.stats <- sort(ssc.stats[, 3])
 
  # make a grid of sand, silt, clay values within the low--high range of the data
  sand.seq <- round(seq(from=sand.stats[1], to=sand.stats[3], by=delta))
  silt.seq <- round(seq(from=silt.stats[1], to=silt.stats[3], by=delta))
  clay.seq <- round(seq(from=clay.stats[1], to=clay.stats[3], by=delta))
  g <- expand.grid(sand=sand.seq, silt=silt.seq, clay=clay.seq)
  
  # subset to only include those sand, silt, clay values that sum to 100%
  real.textures <- which(apply(g, 1, sum) - 100 == 0)
  g <- g[real.textures, ]
  
  # plot low and high values with no symbol, so that we can access the {x,y} screen coordinates
  tp.low.high <- triax.points(g, col.symbols='black', pch=NA)
  
  # create range polygon
  poly.order <- chull(tp.low.high$x, tp.low.high$y)
  
  # return RV and bounding polygon geometry
  return(list(stats=ssc.stats, range=list(x=tp.low.high$x[poly.order], y=tp.low.high$y[poly.order])))
}


# compute and plot "low"--"representative value"--"high" soil textures based on:
# ssc: data.frame/matrix of [sand, silt, clay]
# p: requested percentiles
texture.triangle.low.rv.high <- function(ssc, p=c(0.05, 0.5, 0.95), delta=1, pop.rv.col='red', range.col='RoyalBlue', range.alpha=75, sim=FALSE, sim.n=1000, sim.rv.col='yellow', sim.col=grey(0.95), sim.alpha=150, legend.cex=0.75, ...) {
	
	# setup colors
	range.col <- rgb(t(col2rgb(range.col)), maxColorValue=255, alpha=range.alpha)
  sim.col <- rgb(t(col2rgb(sim.col)), maxColorValue=255, alpha=sim.alpha)
  
	# setup legend elements
	low.high.range.text <- paste0('Low-High Range (', paste(p[c(1,3)], collapse='-'), ')')
	legend.text <- c('Population RV', low.high.range.text)
	legend.cols <- c('black', 'black')
	legend.bg <- c(pop.rv.col, range.col)
	legend.pch <- c(22, 22)
  
	# setup plot, without symbols at textures
  soil.texture(ssc, show.names=FALSE, axis.labels=c('Sand', 'Silt', 'Clay'), show.grid=TRUE, pch=NA)
	
  # optionally simulate data from a composition of normally distributed data
  # using means, and var-cov matrix from original data
  if(sim) {
    if(!require(compositions))
      stop('pleast install the `compositions` package.', call.=FALSE)
    # compute RV / range polygon for normally dist data
    # convert to compositional class, note range is now [0,1]
    ssc.acomp <- acomp(ssc)
    # simulate normally-distributed composition based on data
    ssc.sim <- rnorm.acomp(n=sim.n, mean=meanCol(ssc.acomp), var=cov(ssc.acomp))
    # get range, and rv after converting back to [0,100] interval
    res.sim <- .get.ssc.low.rv.high(as.data.frame(unclass(ssc.sim) * 100),  p=p, delta=delta)
    
    # add polgon defining range of normally dist data
    polygon(res.sim$range$x, res.sim$range$y, col=sim.col, lty=2, lwd=2)
  }
	
  # compute RV / range polygon for data
  res <- .get.ssc.low.rv.high(ssc,  p=p, delta=delta)
  # add polgon defining range of data
  polygon(res$range$x, res$range$y, col=range.col)
	
  # add original data, passing in additional arguments
  triax.points(ssc, ...)
  
	# plot population RV
	suppressWarnings(triax.points(matrix(res$stats[2, ], nrow=1), bg.symbols=pop.rv.col, pch=22, cex=1.25))
  # optionally plot simulated RV
  if(sim)
	  suppressWarnings(triax.points(matrix(res.sim$stats[2, ], nrow=1), bg.symbols=sim.rv.col, pch=22, cex=1.25))
  
  # optionally add legend elements for simulation
  if(sim) {
    legend.text <- c(legend.text, 'Simulated RV', paste0(sim.n, ' Simulations (normal composition)'))
    legend.bg <- c(legend.bg, sim.rv.col, sim.col)
    legend.cols <- c(legend.cols, 'black', 'black')
    legend.pch <- c(legend.pch, 22, NA)
    legend.lty <- c(NA, NA, NA, 2)
    
    legend('topleft', legend=legend.text, pt.bg=legend.bg, pch=legend.pch, col=legend.cols, lty=legend.lty, bty='n', cex=legend.cex, pt.cex=1.25, ncol=ifelse(sim, 2, 1))
  }
  # if no simulation, don't specifiy lty
  else 
    legend('topleft', legend=legend.text, pt.bg=legend.bg, pch=legend.pch, col=legend.cols, bty='n', cex=legend.cex, pt.cex=1.25, ncol=ifelse(sim, 2, 1))
	
	

}
