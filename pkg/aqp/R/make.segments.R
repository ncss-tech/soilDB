
## TODO: iterate over profile IDs instead of groups
# TODO: document this
make.segments <- function(df) {
	
	# group data
	ll <- levels(df$groups)	
	n_groups = length(ll)
	
	# get the current group index
	m <- match(unique(df$group), ll)
	
	# get the number of depth slices
	n_hz <- length(df$prop) / 2
	
	# load current line style
    superpose.line <- trellis.par.get("superpose.line")
    
    # repeat enough times for the current number of groups
    line.col <- rep(superpose.line$col, length.out=n_groups)
	line.lty <- rep(superpose.line$lty, length.out=n_groups)
	line.lwd <- rep(superpose.line$lwd, length.out=n_groups)
	
	
	# need at least 2 horizons
	if(n_hz > 1)
		{
		# re-make dataframe for plotting segments
		df.new <- data.frame(top=df$bnd[1:n_hz], bottom=df$bnd[(n_hz+1):length(df$prop)], prop=df$prop[1:n_hz], stringsAsFactors=FALSE)
		
		# vertical segments
		panel.segments(df.new$prop, df.new$top, df.new$prop, df.new$bottom, 
		lwd=line.lwd[m], col=line.col[m], lty=line.lty[m]) 
		 
		# horizontal segments
		panel.segments(df.new$prop[-n_hz], df.new$bottom[-n_hz], df.new$prop[-1], df.new$top[-1], 
		lwd=line.lwd[m], col=line.col[m], lty=line.lty[m])
		}
		
	else
		{
		message(paste('only 1 horizon, skipping!', df$groups[1]))
		}
	
	}

