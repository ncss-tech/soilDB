## TODO; more testing required

# this function is called by ddply() thus requires a data.frame as the return value
mix_and_clean_colors <- function(x) {
	
  # sanity check: no NA
  if(any(c(is.na(x$r), is.na(x$g), is.na(x$b))))
    return(data.frame(r=NA, g=NA, b=NA, colorhue=NA, colorvalue=NA, colorchroma=NA, sigma=NA))
  
  # fill missing weights with 1
	x$pct[is.na(x$pct)] <- 1
	
  # compute weighted mixtures in RGB space
	r <- with(x, wtd.mean(r, weights=pct))
	g <- with(x, wtd.mean(g, weights=pct))
	b <- with(x, wtd.mean(b, weights=pct))
	
  # back-transform mixture to Munsell
	m <- rgb2munsell(cbind(r, g, b))
  
  # adjust names to match NASIS
  names(m) <- c("colorhue", "colorvalue", "colorchroma", "sigma")
  
	# composite
	df <- cbind(r=r, g=g, b=b, m)
	
	# done
	return(df)
}

