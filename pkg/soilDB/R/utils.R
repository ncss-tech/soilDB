## 
## misc functions used by soilDB
##

## TODO: consider toggling paralithic contact to FALSE when lithic contact is TRUE


# convert diagnostic horizon info into wide-formatted, boolean table
diagHzLongtoWide <- function(d) {
	
	# get unique vector of diagnostic hz
	d.unique <- na.omit(unique(d$diag_kind))
	
	# init list for storing initial FALSE for each peiid / diag kind
	l <- vector(mode='list')
	
	# add unique peiid
	l[['peiid']] <- unique(d$peiid)
	
	# make a vector of FALSE, matching the length of unique peiid
	f <- rep(FALSE, times=length(l[['peiid']]))
	
	# iterate over diagnostic hz kind
	for(i in d.unique) {
		# fill this list element with FALSE
		l[[i]] <- f
		# lookup those peiid with this feature
		matching.peiid <- d$peiid[which(d$diag_kind == i)]
		# toggle FALSE-->TRUE for these pedons
		l[[i]][which(l[['peiid']] %in% matching.peiid)] <- TRUE
	}
	
	# convert to DF
	return(as.data.frame(l))
		
}
