## TODO: this checking assumes that the data are sorted!
## TODO: give a reason for failing

# this should be further documented
test_hz_logic <- function(i, topcol, bottomcol, strict=FALSE) {
  
	# test for overlapping OR non-contiguous horizon boundaries
	if(strict) {
		n <- nrow(i)
		res <- all.equal(i[[topcol]][-1], i[[bottomcol]][-n])
		if(res != TRUE)
			res <- FALSE
		names(res) <- 'hz_logic_pass'
		return(res)
	}
	
	# test for NA
	if(any(c(is.na(i[[topcol]])), is.na(i[[bottomcol]]))) {
		res <- FALSE
		names(res) <- 'hz_logic_pass'
		return(res)
	}
	  
	# test for over-lapping horizons
	# note: this will fail if an O horizon is described using the old style O 3--0cm
	m <- cbind(i[[topcol]], i[[bottomcol]])
	unzipped.depths <- unlist(apply(m, 1, function(i) seq(from=i[1], to=i[2], by=1)))
	len.overlapping <- length(which(table(unzipped.depths) > 1))
	n.hz <- nrow(i)
	
	# there should be 1 fewer segments of overlap than there are horizons  	
	if(len.overlapping > (n.hz - 1))
		res <- FALSE
	else
		res <- TRUE
	names(res) <- 'hz_logic_pass'
	return(res)
  
}
