## see the convertColor() function from grDevices
## ... our function gives "better" looking colors

## TODO: interpolate '2.5' values for all common soil colors
## TODO: optimize with arrays
## TODO: implement in LAB / xyz colorspace 
# color is a vector of RGB values in range of [0,1] -- ideally output from munsell2rgb()
rgb2munsell <- function(color) {
  
  # vectorize via for-loop
  n <- nrow(color)
  res <- vector(length=n, mode='list')
  
  # This is a hack to avoid munsell2rgb: "no visible binding for global variable munsell" at package R CMD check
  munsell <- NULL
  
  # load look-up table from our package
  # This should be moreover more foolproof than data(munsell) c/o PR
  load(system.file("data/munsell.rda", package="aqp")[1]) 
  
  # iterate over colors
  for(i in 1:n) {
    # convert current color to matrix, this will allow matrix and DF as input
    this.color <- as.matrix(color[i, ])
    
    # euclidean distance (in RGB space) is our metric for closest-color
    # d = sqrt(r^2 + g^2 + b^2)
    sq.diff <- sweep(munsell[, 4:6], MARGIN=2, STATS=this.color, FUN='-')^2
    sq.diff.sum.sqrt <- sqrt(rowSums(sq.diff))
    idx <- which.min(sq.diff.sum.sqrt)
    
    # with NA as an input, there will be no output
    if(length(idx) == 0)
      res[[i]] <- data.frame(hue=NA, value=NA, chroma=NA, sigma=NA, stringsAsFactors=FALSE)
    
    # otherwise return the closest color
    else
      res[[i]] <- data.frame(munsell[idx, 1:3], sigma=sq.diff.sum.sqrt[idx])
  }
  
  # convert to DF and return
  return(ldply(res))
}

# convert munsell Hue, Value, Chroma into RGB
# user can adjust how rgb() function will return an R-friendly color
# TODO if alpha is greater than maxColorValue, there will be an error
munsell2rgb <- function(the_hue, the_value, the_chroma, alpha=1, maxColorValue=1, return_triplets=FALSE) {
	## important: change the default behavior of data.frame and melt
  opt.original <- options(stringsAsFactors = FALSE)
  
  # check for missing data
	if(missing(the_hue) | missing(the_chroma) | missing(the_value))
		stop('Must supply a valid Munsell color.')
	
	# check to make sure that each vector is the same length
	if(length(unique( c(length(the_hue),length(the_value),length(the_chroma)))) != 1)
		stop('All inputs must be vectors of equal length.')
	
  ## plyr <= 1.6 : check to make sure hue is a character
  if(class(the_hue) == 'factor') {
    cat('Notice: converting hue to character\n')
    the_hue <- as.character(the_hue)
  }
  

  # This is a hack to avoid munsell2rgb: "no visible binding for global variable munsell" at package R CMD check
  munsell <- NULL
  
  # load look-up table from our package
  # This should be moreover more foolproof than data(munsell) c/o PR
  load(system.file("data/munsell.rda", package="aqp")[1]) 
  
  # join new data with look-up table
  d <- data.frame(hue=the_hue, value=the_value, chroma=the_chroma, stringsAsFactors=FALSE)
  res <- join(d, munsell, type='left', by=c('hue','value','chroma')) # result has original munsell + r,g,b
	
  # reset options:
  options(opt.original)
  
	# if the user wants the raw RGB triplets, give those back
	if(return_triplets)
		return(res[, c('r','g','b')])
	
	# keep track of NA values
	rgb.na <- which(is.na(res$r))
	
	# not really an ideal solution, but seems to work
	# if alpha > maxColorValue -- clamp alpha at maxColorValue
	if(alpha > maxColorValue)
		alpha <- maxColorValue
	
	# convert to R color 
	res$soil_color <- NA # init an empy column
	
  # account for missing values if present: we have to do this because rgb() doesn't like NA
	if(length(rgb.na > 0))
		res$soil_color[-rgb.na] <- with(res[-rgb.na,], rgb(red=r, green=g, blue=b, alpha=alpha, maxColorValue=maxColorValue) )
  # no missing values
	else
		res$soil_color <- with(res, rgb(red=r, green=g, blue=b, alpha=alpha, maxColorValue=maxColorValue) )
	
  # default behavior, vector of colors is returned
	return(res$soil_color)
	}
