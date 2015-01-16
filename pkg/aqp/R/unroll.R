# convert a set of horizon depths and property into a continuous sequence
# returning a vector of standardized length
# suitable for slotting
# horizons must be in order by depth!
unroll <- function(top, bottom, prop, max_depth, bottom_padding_value=NA, strict=FALSE) {
	
	# currently this will only work with integer depths
	if(any( !as.integer(top[top != 0]) == top[top != 0] ) | any( !as.integer(bottom) == bottom))
		stop('this function can only accept integer horizon depths', call.=FALSE)
	
	# are horizons in the correct order?
	if(! all.equal(top,sort(top)))
		stop('Error: horizons are not sorted by depth', call.=FALSE)
	
	# number of horizons
	n.horizons <- length(top)
	
	# all bottom values above the last horizon should be in the SET of top values below the first horizon
	hz.test.bottom_hz_in_top <- bottom[-n.horizons] %in% top[-1]
	if(length(which(hz.test.bottom_hz_in_top)) != (n.horizons - 1)) {
		
		if(strict) {
			stop('error unrolling profile')
			}
		else {
			warning('error unrolling profile, stop execution with strict=TRUE');
			}
		}
		
	# inverse RLE, to generate repeating sequence of property, n times
	p <- inverse.rle(list(lengths=bottom-top, values=prop))
	
	# total depth, in unit length
	p.len <- length(p)
	
	# number of NAs to prepend, in case our profile does not start at 0
	num.NA.prepend <- abs(0 - min(top))
	
	# number of NAs we need to append to match the deepest profile
	num.NA.append <- max_depth - (p.len + num.NA.prepend)
	
	# debug
	# print(paste(max_depth, num.NA.prepend, p.len, num.NA.append))
	
	# padd the result with NA: from the top down
	p.pad <- c(rep(NA, times=num.NA.prepend), p)
	
	# but only if the number of NA to append is positive
	if(sign(num.NA.append) == 1)
		p.pad <- c(p.pad, rep(bottom_padding_value, times=num.NA.append))
	
	# return vector, padded and truncated to max_depth
	return(as.vector(p.pad)[1:max_depth])
	}

