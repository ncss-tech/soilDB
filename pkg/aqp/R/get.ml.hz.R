# generate a data.frame of ML horizonation
# using the output from slab() and a vector of horizon names
get.ml.hz <- function(x, o.names=attr(x, which='original.levels')) {
  
  # trick R CMD check
  top = bottom = NULL
  
  # just in case, make DF-safe names
  safe.names <- make.names(o.names)
  
	# get index to max probability, 
	# but only when there is at least one value > 0 and all are not NA
	.f.ML.hz <- function(i) {
		if(any(i > 0) & !all(is.na(i)))
			which.max(i)
		else
			NA
	}
	
	
	# get most probable, original,  horizon designation by slice
	x$name <- o.names[apply(x[, safe.names], 1, .f.ML.hz)]
	
	# extract ML hz sequences
	x.rle <- rle(as.vector(na.omit(x$name)))
	x.hz.bounds <- cumsum(x.rle$lengths)
	
	# composite into a data.frame
	# note: we always start from 0
	x.ml <- data.frame(hz=x.rle$value, top=c(0, x.hz.bounds[-length(x.hz.bounds)]), bottom=x.hz.bounds, stringsAsFactors=FALSE)
	
	# in cases where probability depth-functions cross more than once,
	# it is necessary to account for overlaps
	x.ml <- ddply(x.ml, 'hz', summarise, top=min(top), bottom=max(bottom))
	
	# re-order using vector of original horizon names-- this will result in NAs if a named horizon was not the most likely
	x.ml <- x.ml[match(o.names, x.ml$hz), ]
	x.ml <- na.omit(x.ml)
	
	# integrate probability density function over ML bounds
	x.ml$confidence <- NA
	for(i in seq_along(x.ml$hz)) {
		slice.seq <- seq(from=x.ml$top[i], to=x.ml$bottom[i])
		x.i <- x[slice.seq, x.ml$hz[i]]
		hz.int.prob.pct <- round( (sum(x.i) / length(slice.seq)) * 100)
		x.ml$confidence[i] <- hz.int.prob.pct
	}
	
  # compute a pseudo-brier score using ML hz as the "true" outcome
  # brier's multi-class score : http://en.wikipedia.org/wiki/Brier_score#Original_definition_by_Brier
  x.bs <- ddply(x[!is.na(x$name), ], 'name', function(x.i) {
    # save the gen hz probabilities into new df
    x.pr <- x.i[, safe.names]
    # init new matrix to store most-likely gen hz class
    m <- matrix(0, ncol=ncol(x.pr), nrow=nrow(x.pr))
    # same structure as x.pr
    dimnames(m)[[2]] <- names(x.pr)
    # set appropriate genhz to 1
    for(i in 1:nrow(x.i)) {
      ml.hz.i <- x.i$name[i]
      m[i, ml.hz.i] <- 1
    }
    # compute bs for this gen hz
    bs <- sum((x.pr - m)^2, na.rm=TRUE) / nrow(x.pr)
  })
  
  # fix names for joining
  names(x.bs) <- c('hz', 'pseudo.brier')
  
  # join brier scores to ML hz table
  x.ml <- join(x.ml, x.bs, by='hz')
  
	return(x.ml)
}
