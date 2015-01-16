
## TODO: still not completely generalized
# annotate elements from @diagnostic with brackets 
# mostly a helper function for addBracket()
addDiagnosticBracket <- function(s, kind, id=idname(s), top='featdept', bottom='featdepb', ...) {
	
  # get plotting details from aqp environment
  lsp <- get('last_spc_plot', envir=aqp.env)
    
  ## TODO: integrate these
  y.offset <- lsp$y.offset
  scaling.factor <- lsp$scaling.factor
  plot.order <- lsp$plot.order
  
  # extract diagnostic horizon information
  d <- diagnostic_hz(s)
  d <- d[which(d$diag_kind == kind), ]
  
  # generate index linking our top/bottom depths with original ordering
  key <- match(d[[id]], profile_id(s))
  
  # add backets
  addBracket(top=d[[top]], bottom=d[[bottom]], idx=key, ...)
}

## TODO: add proper documentation
## NOTE: this function is vectorized
# internal function for plotting a bracket (usually defines a diagnostic feature or similar)
# idx: (optional) integer index to profile
# top: top depth
# bottom: bottom depth
# tick.length: bracket tick length
# offset: left-hand offset from profile center
addBracket <- function(top, bottom, idx=NULL, tick.length=0.05, arrow.length=0.05, offset=-0.3, missing.bottom.depth=25, ...) {
	
  # get plotting details from aqp environment
  lsp <- get('last_spc_plot', envir=aqp.env)
  
  # if missing an specific index, assume plotting order
  if(is.null(idx))
    plot.order <- lsp$plot.order
  else
    plot.order <- idx
  
  ## TODO: integrate these
  y.offset <- lsp$y.offset
  scaling.factor <- lsp$scaling.factor
  w <- lsp$width
  
	# normal case: both top and bottom defined
	if(!missing(top) & !missing(bottom)) {
    # x-positions
    x.1 <- plot.order + offset
    x.2 <- x.1 + tick.length
		# top tick
		segments(x.1, top, x.2, top, lend=2, ...)
		# bottom tick
		segments(x.1, bottom, x.2, bottom, lend=2, ...)
		# vertical bar
		segments(x.1, top, x.1, bottom, lend=2, ...)
	}
	
	# missing bottom: replace bottom tick with arrow head
	if(!missing(top) & missing(bottom)) {
	  # x-positions
	  x.1 <- plot.order + offset
	  x.2 <- x.1 + tick.length
		# top tick
		segments(x.1, top, x.2, top, lend=2, ...)
		# vertical bar is now an arrow
		arrows(x.1, top, x.1, top+missing.bottom.depth, length=arrow.length, lend=2, ...)
	}
	
}



# simple function to convert horizon boundary distinctness codes into vertical (+/-) offsets
hzDistinctnessCodeToOffset <- function(x, codes=c('A','C','G','D'), offset=c(0.5, 1.5, 5, 10)) {	
	x <- as.character(x)
	x.code <- match(x, codes)
	x.offset <- offset[x.code]
	x.offset <- ifelse(is.na(x.offset), 0, x.offset)
	return(x.offset)
}





# TODO: behavior not defined for horizons with an indefinate lower boundary
# TODO: save important elements of geometry from last plot to aqp.env
# TODO: move some of the processing outside of the main loop: column names, etc.

## basic function
plotSPC <- function(x, color='soil_color', width=0.2, name=NULL, label=idname(x), alt.label=NULL, alt.label.col='black', cex.names=0.5, cex.depth.axis=cex.names, cex.id=cex.names+(0.2*cex.names), print.id=TRUE, id.style='auto', plot.order=1:length(x), add=FALSE, scaling.factor=1, y.offset=0, n=length(x), max.depth=max(x), n.depth.ticks=5, shrink=FALSE, shrink.cutoff=3, abbr=FALSE, abbr.cutoff=5, divide.hz=TRUE, hz.distinctness.offset=NULL, hz.distinctness.offset.col='black', hz.distinctness.offset.lty=2, axis.line.offset=-2.5, density=NULL, col.label=color, col.palette = rev(brewer.pal(10, 'Spectral')), lwd=1, lty=1, ...) {
  
  # save arguments to aqp env
  lsp <- list('width'=width, 'plot.order'=plot.order, 'y.offset'=y.offset, 'scaling.factor'=scaling.factor)
  assign('last_spc_plot', lsp, envir=aqp.env)
  
  # get horizons
  h <- horizons(x)
  
  # get column names from horizon dataframe
  nm <- names(h)
  
  # if the user has not specified a column containing horizon designations,
  # attempt to guess
  if(missing(name)) {
    possible.name <- nm[grep('name', nm, ignore.case=TRUE)]
    # use the first valid guess
    if(length(possible.name) > 0) {
      possible.name <- possible.name[1]
      name <- possible.name
      message(paste('guessing horizon designations are stored in `', name, '`', sep=''))
    }
    else {
      message('unable to guess column containing horizon designations')
      name <- NA # set column name to NA, details handled farther down in the function
    }
  }
  
  # setup horizon colors:
  # 1. numeric vector, rescale and apply color ramp
  if(is.numeric(h[[color]])) {
    cr <- colorRamp(col.palette)
    # note that this may contain NAs
    c.rgb <- cr(rescale(h[[color]]))
    cc <- which(complete.cases(c.rgb))
    h$.color <- NA
    # convert non-NA values into colors
    h$.color[cc] <- rgb(c.rgb[cc, ], maxColorValue=255)
    # generate range / colors for legend
    pretty.vals <- pretty(h[[color]])
    color.legend.data <- list(legend=pretty.vals, col=rgb(cr(rescale(pretty.vals)), maxColorValue=255))
  }
  # 2. character vector, assume these are valid colors
  if(is.character(h[[color]])) {
    h$.color <- h[[color]]
  }
  
  # get top/bottom column names
  IDcol <- idname(x)
  hzDepthCols <- horizonDepths(x)
  tcol <- hzDepthCols[1]
  bcol <- hzDepthCols[2]
  
  # get profile IDs
  pIDs <- profile_id(x)
  
  # get profile labels from @site
  pLabels <- site(x)[[label]]
  
  # if profile style is auto, determin style based on font metrics
  if(id.style == 'auto') {
  	sum.ID.str.width <- sum(sapply(pLabels, strwidth, units='inches', cex=cex.id, font=2))
  	plot.width <- par('pin')[1]
  	ID.width.ratio <- sum.ID.str.width  / plot.width
#   	print(ID.width.ratio)
  	
  	if(ID.width.ratio > 0.7)
  		id.style <- 'side'
  	else
  		id.style <- 'top'
  	}
  
  
  # fudge factors
  extra_x_space <- 2
  extra_y_space <- 2
  
  # pre-compute nice range for depth axis, also used for plot init
  depth_axis_intervals <- pretty(seq(from=0, to=max.depth, by=1), n=n.depth.ticks)
  
  # init plotting region, unless we are appending to an existing plot
  # note that we are using some fudge-factors to get the plotting region just right
  if(!add) {
    # par(mar=c(0.5,1,0,1)) # is it wise to adjust the plotting area?
	  plot(0, 0, type='n', xlim=c(1-(extra_x_space/5), n+(extra_x_space)), ylim=c(max(depth_axis_intervals), -4), axes=FALSE, xlab='', ylab='')
	}
  
  
  # add horizons in specified order	
  for(i in 1:n) {
	  # convert linear sequence into plotting order
	  profile_i <- plot.order[i]
	  
	  # extract the current profile's horizon data
    this_profile_label <- pLabels[profile_i]
	  this_profile_id <- pIDs[profile_i]
	  this_profile_data <- h[h[IDcol] == this_profile_id, ]
	  
    # extract column names
    cn <- names(this_profile_data)
    
    # extract / generate horizon color
    m <- match(color, cn)
    if(! is.na(m))
      this_profile_colors <- this_profile_data$.color
    else # no user-defined color column, or it is missing
      this_profile_colors <- 'white'
    
	  # extract / generate horizon fill density
	  if(! missing(density)) {
	  	# if a single number was given, then recylce it over all horizons
	  	if(is.numeric(density))
	  		this_profile_density <- density
	  	# otherwise we have a column name
	  	else {
	  		m <- match(density, cn)
	  		if(! is.na(m))
		  		this_profile_density <- this_profile_data[[m]]
		  	else # user-defined column is missing
			  	this_profile_density <- NULL
	  	}
	  }
	  else # no user-defined density column
	  	this_profile_density <- NULL
	  
    # extract / generate horizon name
    m <- match(name, cn)
    if(! is.na(m))
      this_profile_names <- this_profile_data[[m]]
      # otherwise use an empty string
    else
      this_profile_names <- ''
    
	  
	  # generate rectangle geometry
	  # get vectors of horizon boundaries, and scale
	  y0 <- (this_profile_data[, bcol] * scaling.factor) + y.offset
	  y1 <- (this_profile_data[, tcol] * scaling.factor) + y.offset
	
	
	##
	## TODO: use horizon boundary type and topography to modify figures
	##
	## i.e. clear-wavy = dashed lines at an angle, based on red book
	  
	# create horizons + colors
    # default are filled rectangles
    if(divide.hz) {
	    rect(i-width, y0, i + width, y1, col=this_profile_colors, border=NULL, density=this_profile_density, lwd=lwd, lty=lty)
	 
	 # optionally add horizon boundary distinctiveness
	 if(! is.null(hz.distinctness.offset)) {
	 	hz.dist.offset <- this_profile_data[, hz.distinctness.offset]
	 	segments(i-width, y0 - hz.dist.offset, i+width, y0 - hz.dist.offset, col=hz.distinctness.offset.col, lty=hz.distinctness.offset.lty)
		segments(i-width, y0 + hz.dist.offset, i+width, y0 + hz.dist.offset, col=hz.distinctness.offset.col, lty=hz.distinctness.offset.lty)	
     }
	    
	 }
    
    # otherwise, we only draw the left, top, right borders, and then fill
    else {
      rect(i-width, y0, i + width, y1, col=this_profile_colors, border=NA, density=this_profile_density, lwd=lwd, lty=lty)
      segments(i-width, y0, i-width, y1, lwd=lwd, lty=lty) # left-hand side
      segments(i+width, y0, i+width, y1, lwd=lwd, lty=lty) # right-rand side
      segments(i-width, min(y1), i+width, min(y1), lwd=lwd, lty=lty) # profile top
      segments(i-width, max(y0), i+width, max(y0), lwd=lwd, lty=lty) # profile bottom
    }
      
    
	  # annotate with names
	  # first get the horizon mid-point
	  mid <- ( y1 + y0 )/2
	  
	  # optionally shrink the size of names if they are longer than a given thresh
	  if(shrink) {
		  names.to.shrink <- which(nchar(this_profile_names) > shrink.cutoff)
		  cex.names.shrunk <- rep(cex.names, length(this_profile_data[, tcol]))
		  cex.names.shrunk[names.to.shrink] <- cex.names.shrunk[names.to.shrink] * 0.8
		  text(i + width, mid, this_profile_names, pos=4, offset=0.1, cex=cex.names.shrunk)
		  }
	  # standard printing of names, all at the same size
	  else
		  text(i + width, mid, this_profile_names, pos=4, offset=0.1, cex=cex.names)		
	  
	  # add the profile ID
	  if(print.id) {
			# optionally abbreviate
			if(abbr)
		  	id.text <- abbreviate(as.character(this_profile_label), abbr.cutoff)
	
			# no abbreviations of th ID
			else
	  		id.text <- as.character(this_profile_label)
		
			# add the text: according to style
			if(id.style == 'top')
				text(i, y.offset, id.text, pos=3, font=2, cex=cex.id)
	
			if(id.style == 'side')
				text(i-(width+0.025), y.offset, id.text, adj=c(1, -width), font=2, cex=cex.id, srt=90)
			}
	  }
  
  # axis:
  depth_axis_tick_locations <- (depth_axis_intervals * scaling.factor) + y.offset
  depth_axis_labels <- paste(depth_axis_intervals, depth_units(x))
  axis(side=4, line=axis.line.offset, las=2, at=depth_axis_tick_locations, labels=depth_axis_labels, cex.axis=cex.depth.axis)
  
  # plot alternate labels
  if(!missing(alt.label)) {
  	al <- site(x)[[alt.label]]
  	al <- al[plot.order]
  	text(1:length(x), y.offset+3, al, srt=90, adj=c(1, 0.5), font=2, cex=cex.id * 1.5, col=alt.label.col)
  }
  
  ## experimental color legend
  if(exists('color.legend.data')) {
    # If no title given, set col.label is set to color
    mtext(side=3, text=col.label, font=2, line=1.6)
    legend('bottom', legend=color.legend.data$legend, col=color.legend.data$col, bty='n', pch=15, horiz=TRUE, xpd=TRUE, inset=c(0, 0.99))
  }
  }



# method dispatch
setMethod("plot", signature("SoilProfileCollection"), definition=plotSPC)


