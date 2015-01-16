
# convert volume pct [0,100] into DF of points along a res x res grid
.volume2df <- function(v, depth, res) {
	# test for no data
	if(is.na(v))
		return(data.frame())
	
	# convert volume pct into fraction
	v <- v / 100
	
	# init matrix with NA
	cells <- (depth * res)
	m <- matrix(nrow=depth, ncol=res)
	m[] <- NA
	
	# determine number of cells required to symbolize volume fraction
	v.n <- round(v * cells)
	v.cells <- sample(1:cells, size=v.n)
	# mark cells with 1
	m[v.cells] <- 1
	
	# convert matrix into data.frame
	d <- expand.grid(x=(1:res), y=(1:depth))
	d$val <- m[1:cells]
	# keep only those cells with val = 1
	d <- d[which(d$val == 1), ]
	
	# scrub extra columns and return
	d$val <- NULL
	return(d)
	}


# add volume fraction information to an existing SPC plot
addVolumeFraction <- function(x, colname, res=10, cex.min=0.1, cex.max=0.5, pch=1, col='black') {
	
	# get plotting details from aqp environment
	lsp <- get('last_spc_plot', envir=aqp.env)
	w <- lsp$width
	plot.order <- lsp$plot.order
	
	# horizontal shrinkage factor
	w.offset <- w / 5
	depth.offset <- 0.5
	
	# get top/bottom colnames
	hd <- horizonDepths(x)
	
	# iterate over profiles
	for(p.i in 1:length(x)) {
		
		h <- horizons(x[plot.order[p.i], ])
	
		# determine left/right extent of symbols
		x.center <- p.i
		x.left <- x.center - (w - w.offset)
		x.right <- x.center + (w - w.offset)
	
		# iterate over horizons
		for(h.i in 1:nrow(h)) {
			this.hz <- h[h.i, ]
			hz.thick <- this.hz[[hd[2]]] - this.hz[[hd[1]]]
			
			# convert this horizon's data
			v <- .volume2df(v=this.hz[[colname]], depth=hz.thick, res=res)
			
			## TODO: still throws errors
			# just those with data
			if(nrow(v) > 0 ) {
        # jitter and rescale x-coordinates
				v$x <- rescale(jitter(v$x), to=c(x.left, x.right))
		
				# rescale y-coordinates
				y.top <- this.hz[[hd[1]]] + depth.offset
				y.bottom <- this.hz[[hd[2]]] - depth.offset
				v$y <- rescale(jitter(v$y), to=c(y.top, y.bottom))
		
				# generate random symbol size
				p.cex <- runif(nrow(v), min=cex.min, max=cex.max)
		
				# add jittered points
				points(v$x, v$y, cex=p.cex, col=col, pch=pch)
			}
		}
	}	
}








