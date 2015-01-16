# this function is called by ddply() thus requires a data.frame as the return value
mix_and_clean_colors <- function(x) {
	# fill missing weights with 1
	x$pct[is.na(x$pct)] <- 1
	
	# skip horizons with a single color
	tab <- table(x$phiid)
	if(tab > 1) {
		r <- with(x, wtd.mean(r, weights=pct))
		g <- with(x, wtd.mean(g, weights=pct))
		b <- with(x, wtd.mean(b, weights=pct))
		v <- with(x, wtd.mean(colorvalue, weights=pct))
		# composite
		df <- data.frame(r=r, g=g, b=b, colorvalue=v)
	}
	
	# note that if there is no mixing, then we have to reference the Munsell value by 'colorvalue'
	else
		df <- x[, c('r', 'g', 'b', 'colorvalue')]
	
	# done
	return(df)
}

