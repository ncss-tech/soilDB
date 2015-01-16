# example objective function for full pattern matching
f.noise <- function(inits, pure.patterns, sample.pattern, eps.total=0.05) {
	
	# how many components in the mixture?
	# the last element is the noise component
	n.comp <- length(inits) - 1
	
	# pure substance weights -- to be estimated
	pure.weights <- inits[1:n.comp]
	
	# noise weight -- to be estimated
	noise.component <- inits[n.comp+1]
	
	# check: proportions are always > 0
	if( any(pure.weights < 0))
		return(Inf)
	
	# check: proportions are always < 1
	if( any(pure.weights > 1) )
		return(Inf)
	
	# can't have negative noise
	if(noise.component < 0)
		return(Inf)
	
	## this only makes sense when component weights sum to ~1
# 	# check to make sure proportions add to approx 1
# 	weight.sum <- sum(pure.weights)
# 	if( abs(weight.sum - 1) > eps.total)
# 		return(Inf)
		
	# scale pure patterns with guessed values
	s.mix <- apply(sweep(pure.patterns, 2, pure.weights, '*'), 1, sum)
	
	# add in a noise component
	s.mix <- noise.component + s.mix
	
	# compute abs difference.. could use squared differences
	d <- sum(abs(sample.pattern - s.mix))
	
	# done
	return(d)
	}
	

