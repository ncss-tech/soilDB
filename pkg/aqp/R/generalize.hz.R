# return a vector of generalized horizon names, based on:
# a vector of new names
# a vector of patterns
# code for any non-matching hz designations
generalize.hz <- function(x, new, pat, non.matching.code='not-used') {
	
	# init vector of 'other', same length as original horizon name vector
	g <- rep(non.matching.code, times=length(x))
	
	# iterate over new new names, match with patterns, assign new hz names
	for(i in seq_along(new)) {
		g[grep(pat[i], x)] <- new[i]
	}
	# convert to factor, and re-level
	g <- factor(g, levels=c(new, non.matching.code))
	return(g)
}
