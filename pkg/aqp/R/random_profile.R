
.lpp <- function(x, a, b, u, d, e) {
  # the exponential term
  f.exp <- exp((x + d * log(e) - u) / d)
  # first part
  f1 <- (b/u) * (1 + f.exp)^((-e - 1) / e)
  # second part
  f2 <- f.exp * (e + 1)^((e+1) / e)
  # combine pieces
  res <- a + f1 * f2
  return(res)
  }


random_profile <- function(id, n=c(3,4,5,6), min_thick=5, max_thick=30, n_prop=5, exact=FALSE, method='random_walk', HzDistinctSim=FALSE, ...) {

  # sanity check
  if(missing(id))
	  stop('must specify an id')

  if(max_thick < min_thick)
	  stop('illogical horizon thickness constraints')

  if(! method %in% c('random_walk', 'LPP'))
	stop('invalid method')

  # get extra arguments
  dots <- list(...)

  # if requested, give back the exact number of horizons
  if(length(n) == 1 & exact)
	  n_hz <- n

  # otherwise randomly choose from suggestions
  else
	  n_hz <- sample(n, 1)

  # generate hz top bnd
  tops <- integer(n_hz-1)
  for(i in 1:(n_hz-1))
	  tops[i] <- sample(min_thick:max_thick, 1)

  # add 0, then generate bottom bnd
  tops <- as.integer(c(0, tops))
  bottoms <- as.integer(c(tops[-1], sample(min_thick:max_thick, 1)))

  # combine into a df
  d <- data.frame(id=id, top=cumsum(tops), bottom=cumsum(bottoms), name=paste('H',1:n_hz,sep=''))

  # generate several properties
  # with different means / sd
  for(i in 1:n_prop) {
	# init storage
	  p <- numeric(n_hz)
	
	if(method == 'random_walk') {
		p[1] <- rnorm(1, mean=runif(n=1, min=-10, max=10), sd=runif(n=1, min=1, max=10))
		for(j in 2:n_hz)
			p[j] <- p[j-1] + rnorm(1, mean=runif(n=1, min=-10, max=10), sd=runif(n=1, min=1, max=10))
	  }
	
	if(method == 'LPP') {
	  # generate synthetic values at horizon mid-points
	  mids <- with(d, (top + bottom)/2)
	  
	  # generate LPP parameters from uniform dist if not given as arguments
	  if(is.null(dots[['lpp.a']]))
			lpp.a <- runif(n=1, min=5, max=25)
	  else
			lpp.a <- dots[['lpp.a']]
	  
	  if(is.null(dots[['lpp.b']]))
			lpp.b <- runif(n=1, min=20, max=60)
	  else
			lpp.b <- dots[['lpp.b']]
	  
	  if(is.null(dots[['lpp.u']]))
			lpp.u <- runif(n=1, min=10, max=90)
	  else
			lpp.u <- dots[['lpp.u']]
	  
	  if(is.null(dots[['lpp.d']]))
			lpp.d <- runif(n=1, min=1, max=10)
	  else
			lpp.d <- dots[['lpp.d']]
	  
	  if(is.null(dots[['lpp.e']]))
			lpp.e <- runif(n=1, min=5, max=20)
	  else
			lpp.e <- dots[['lpp.e']]
	  
	  # generate vector of synthetic values based on LPP
	  p <- .lpp(mids, a=lpp.a, b=lpp.b, u=lpp.u, d=lpp.d, e=lpp.e)
	  }
	
	  # add generated depth profile to horizons
	  new_col <- paste('p',i, sep='')
	  d[,new_col] <- p
	  }
	
  # optionally add horizon distinctness codes:
  # these are based on USDA-NCSS codes and approximate vertical offsets
	# codes are constrained to the thickness of the horizon
  if(HzDistinctSim) {
  	# standard codes and offsets
  	codes <- c('A','C','G','D')
  	offsets <- hzDistinctnessCodeToOffset(codes)
  	# compute horizon thickness vector
  	thick <- with(d, bottom-top)
  	
  	# create matrix of distinctness codes based on (1/3) horizon thickness
  	# 1 when possible, 0 when impossible
  	prob.matrix <- t(sapply(thick, function(i) (i/3) >= offsets))
  	prob.matrix[which(prob.matrix)] <- 1
		
  	d.codes <- vector(mode='character', length=n_hz)
  	for(i in 1:n_hz) {
  		d.codes[i] <- sample(codes, size=1, prob=prob.matrix[i, ])
  	}
  	
  	d$HzDistinctCode <- d.codes
  }
  
  # all done
  return(d)
}


