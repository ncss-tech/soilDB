
##############################################################
## profile classification functions ##
##############################################################

# define function for summing a list of dissimilarity matrices
# that have had NA replaced with 0
.SumDistanceList <- function(x) Reduce("+", x)


## consider using 'ff' package for file-based storage of VERY large objects. Probably just the dissimilarity matrix

## slice() would be a better approach than using every nth depth slice

## TODO: site/hz properties combined:
## 2012-02-28: partially implemented, but no way to control weighting
## D = (D_hz/max(D_hz) * w_hz) + (D_site/max(D_site) * w_site) / (w_hz + w_site)

## TODO: we are suppressing warnings from daisy() when input is all NA
##       this is fine for now, but we should figure out a better way

## TODO: allow for other distance-computing functions
## TODO: allow for 'weights' argument (when metric = 'Gower') to daisy()

## TODO: determine practical significance of 'filter' argument

## low-level function that the user will probably not ever use directly
# Seems to scale to 1000 profiles with 5 variables, could use optimization
# function requires at least two attributes
# hard coded reference to s$id
# set k to 0 for no depth weighting 
pc <- function(s, vars, max_d, k, filter=NULL, sample_interval=NA, replace_na=TRUE, add_soil_flag=TRUE, return_depth_distances=FALSE, strict_hz_eval=FALSE, progress='none', plot.depth.matrix=FALSE, 
rescale.result=FALSE, verbose=FALSE) {
	
	# currently this will only work with integer depths
	# test by attempting to cast to integers
	# if there is no difference, then we are fine
	top.test <- any( ! as.integer(na.omit(s$top)) == na.omit(s$top))
	bottom.test <- any( ! as.integer(na.omit(s$bottom)) == na.omit(s$bottom))
	if(top.test | bottom.test)
		stop('this function can only accept integer horizon depths', call.=FALSE)
	
	## TODO: this should be updated
	# check to make sure that there is an 'id' column
	if(is.null(s$id))
		stop("'s' must contain a column named 'id' ", call.=FALSE)
	
	## TODO: evaluate the practical implications of this
	# optionally filter the data
	if(!missing(filter)) {
		s <- s[filter, ]
	}
	
	## TODO: put this into its own function
	## TODO: weight result using horizon thickness
	# iterate over profiles and compute percent missing data by variable
	pct_missing <- ddply(s, 'id', .fun=function(i, v=vars) {
		## TODO: rows.to.review may not be needed
		# only evaluate missing data within horizons that aren't completly NA (Oi, Cr, R horizons)
		# all.missing.test <- apply(sapply(i[, v], is.na), 1, all)
		# rows.to.review <- which( ! all.missing.test )
		round(sapply(i[, v], function(j) length(which(is.na(j)))) / nrow(i) * 100)
	})
  
	# keep track of profiles missing any or all of their data
	problem.profiles.idx <- which(apply(pct_missing[, vars], 1, function(i) any(i > 0)) )
	bad.profiles.idx <- which(apply(pct_missing[, vars], 1, function(i) all(i == 100)) )
  
	if(length(problem.profiles.idx) > 0) {
	  assign('problem.profiles', value=pct_missing[problem.profiles.idx,], envir=aqp.env)
		message('Missing data will bias results, check inputs.\nPercent missing:')
		print(pct_missing[problem.profiles.idx, ])
	}
  
	if(length(bad.profiles.idx) > 0) {
	  # stop stop and let the user know
	  bad.profiles <- unique(s$id)[bad.profiles.idx]
	  stop(paste('no non-NA values associated with profiles:', paste(bad.profiles, collapse=', '), '\nConsider removing these profiles and re-running.'), call.=FALSE)
	}
	
	# identify the number of profiles
  # n.profiles <- length(s)
	n.profiles <- length(unique(s$id))
	
	# number of variables
	n.vars <- length(vars)
	
	# sequence describing depth slice indices	
	# use a sequence from 1 ... max depth
	depth_slice_seq <- 1:max_d
	
	# use decimated sampling if requested
	if(!is.na(sample_interval) & sample_interval != 1)
		depth_slice_seq <- depth_slice_seq[depth_slice_seq %% sample_interval == 1]
	
	# compute a weighting vector based on k	
	w <- 1 * exp(-k * depth_slice_seq)
		
	## TODO: convert to slice()
	## unroll each named soil property, for each soil profile
	## the result is a list matricies with dimensions: depth, num_properties 
	# this approach requires a named list of soil properties
	s.unrolled <- dlply(s, "id", .progress=progress, .fun=function(di, p=vars, d=max_d, strict=strict_hz_eval, .parallel=getOption('AQP_parallel', default=FALSE)) {
		
		# iterate over the set of properties, unrolling as we go
		# the result is a [z by p] matrix unrolled to max_d
		m <- try(sapply(p, function(p_i) unroll(di$top, di$bottom, prop=di[, p_i], max_depth=d, strict=strict) ))
		
		## TODO: could be better
		# check for a non-NULL attribute of 'class'
		# this will only happen when there was an error
		if( !is.null(attr(m, 'class'))) {
			if(attr(m, 'class') == 'try-error') {
				stop(paste('Error: bad horizon structure in soil id', as.character(unique(di$id))), call.=FALSE)
				}
			}
		else
			return(m)
		}
	)
	
	
	## generate a matrix storing a flag describing soil vs. non-soil at each slice
	## note that this will truncate a profile to the max depth of actual data
	## profiles missing data in all variables will cause function to stop here
	if(add_soil_flag){
    
		# keep temp subset of the data so that soil/non-soil matrix is 
		# evaluated based on presence of real data in at least 1 variable
		s.sub <- na.omit(s[, c('id', 'top', 'bottom', vars)])
    
    ## BUG!!! tapply() re-orders the results based on sorting of s.sub$id
    ## ----> this will cause problems when input isn't sorted by ID
		# get the depth of each profile
		s.slices_of_soil <- tapply(s.sub$bottom, s.sub$id, max, na.rm=TRUE)
		
		# truncate to the max requested depth
		s.slices_of_soil <- ifelse(s.slices_of_soil <= max_d, s.slices_of_soil, max_d)
		s.slices_of_non_soil <- max_d - s.slices_of_soil
		
		s.slices_of_soil.length <- length(s.slices_of_soil)
		
		# init a matrix with dimensions: depth slices, number of profiles
		soil.matrix <- matrix(ncol=s.slices_of_soil.length, nrow=max_d)
		
		# fill with TRUE for 'soil' or FALSE for 'non-soil'
		for(s.i in 1:s.slices_of_soil.length) {
			soil.matrix[, s.i] <- c(rep(TRUE, s.slices_of_soil[s.i]), rep(FALSE, s.slices_of_non_soil[s.i]))
		}
		
		# plot a diagnostic image, may not be useful for n > 100 profiles
		if(plot.depth.matrix) {
			# define color scheme: if all TRUE, then we only need 1 color
			if(length(table(soil.matrix)) > 1)
				image.cols <- c(NA, 'grey')
			else
				image.cols <- c('grey')
			
      # labs <- profile_id(s)
		  labs <- unique(s.sub$id)
		  image(x=1:n.profiles, y=1:max_d, z=t(soil.matrix), col=image.cols, ylim=c(max_d, 0), xlab='ID', ylab='Slice Number (usually eq. to depth)', main='Soil / Non-Soil Matrix', axes=FALSE)
		  box()
		  abline(v=seq(1, n.profiles)+0.5, lty=2)
		  axis(side=2, at=pretty(c(0, depth_slice_seq)), las=1)
		  axis(side=1, at=1:n.profiles, labels=labs, las=2, cex.axis=0.75)
		  }
    
		# cleanup
		rm(s.sub)
		}

	
	##
	## new version for computing slice-wise dissimilarities... fast! 
	## 
	message(paste('Computing dissimilarity matrices from', n.profiles, 'profiles'), appendLF=FALSE)
	
	## TEMP HACK to supress warnings generated by calling daisy with all NA input
	ow <- options('warn')
	options(warn=-1)
	
	d <- llply(depth_slice_seq, .parallel=getOption('AQP_parallel', default=FALSE), .progress=progress, .fun=function(i, su=s.unrolled) {
	  
	  ## this could be a source of slowness, esp. the t()
	  ps <- sapply(su, function(dz, z_i=depth_slice_seq[i]) { dz[z_i,] })
	  sp <- t(ps)
	  
		# compute distance metric for this depth
		# distance metric has large effect on results
		# Gower's distance gives the best looking results, and automatically standardizes variables
		
		## this is where we run into memory-size limitations
		## an ff object would help here... however it can not preserve all of the information 
		## that a list can... we would need to store these data as raw matrices
	  
	  ## TODO: don't call daisy on bogus input data, temp fix: disable warnings
	  ## if all of the input to daisy is NA, then we get warnings from min() and max()
	  ## this happens when we set a max depth that is beyond most profiles
	  d.i <- daisy(sp, metric='gower')
		return(d.i)
	  }
	)
  # reset warning options
	options(ow)
	
	## TODO: does this actually do anything?
	# clean-up
	rm(s.unrolled) ; gc()	
	
	# print total size of D
	message(paste(" [", round(object.size(d) / 1024^2, 2), " Mb]", sep=''))
	
	# should NA in the dissimilarity matrix be replaced with max(D) ?
	if(replace_na) {
		# replace all NA with the MAX distance between any observations
		# note that down deep, there may not be enough data for any pair-wise comparisons
		# therefore, we should not attempt to calculate max() on a matrix of all NA
		max.distance.vect <- sapply(d, function(i) if(all(is.na(i))) NA else max(i, na.rm=TRUE))
		max.distance <- max(max.distance.vect, na.rm=TRUE)
    
		## note: this will not work with sample_interval set
		# should we use a more expensive approach, that uses the soil/non_soil flag?
		if(add_soil_flag) {
			# kind of messy: re-using an object like this
			for(i in 1:length(d)) {
				d_i <- as.matrix(d[[i]])

				# set all pairs that are made between deep vs. shallow soil
				# to the maximum distance- by row and column
				cells.with.na.rows <- which(is.na(d_i[, which(soil.matrix[i, ])]))
				cells.with.na.cols <- which(is.na(d_i[which(soil.matrix[i, ]), ]))
				
				d_i[, which(soil.matrix[i, ])][cells.with.na.rows] <- max.distance
				d_i[which(soil.matrix[i, ]), ][cells.with.na.cols] <- max.distance
				
				# convert back to dist object
				d_i <- as.dist(d_i)
				
				# copy original attributes
				attributes(d_i) <- attributes(d[[i]])
				
				# save back to original position in list
				d[[i]] <- d_i
				}
			
			# remove the soil.matrix object to save some space
			rm(soil.matrix) ; gc()
			}
		# use a less expensive approach, where all NA are replaced by the max distance
		else {
			d <- lapply(d, function(d_i) 
				{
				cells.with.na <- which(is.na(d_i))
				d_i[cells.with.na] <- max.distance
				return(d_i)
				} )
			}
		}
	
	
	
	# optionally return the distances for each depth
	# depth-weighting is performed, but NA is not converted to 0
	if(return_depth_distances) {
		# depth-weighting
		for(i in seq_along(depth_slice_seq))
			d[[i]] <- d[[i]] * w[i]
		return(d)
		}
		
	
	# final tidy-ing of the list of dissimilarity matrices
	for(i in seq_along(depth_slice_seq)) {
		# convert NA -> 0
		na.idx <- which(is.na(d[[i]]))
		if(length(na.idx) > 0)
			d[[i]][na.idx] <- 0
		
		# depth-weighting
		d[[i]] <- d[[i]] * w[i]
		}
	
	
	# reduce list of dissimilarity matrices by summation
	D <- .SumDistanceList(d)
	
	# add distance metric
	attr(D, 'Distance Metric') <- 'Gower'
	
	# remove previous warnings about NA
	attr(D, 'NA.message') <- NULL
	
	# normalize by dividing by max(D)
	# this is important when incorporating site data
	# causes problems for some functions like sammon
	if(rescale.result)
		D <- D/max(D, na.rm=TRUE)
	
	## DEBUG
	if(verbose) {
	  cat('depth-slice seq:\n')
	  print(depth_slice_seq)
	  
	  cat('depth-weighting vector:\n')
	  print(round(w, 2))
    
	  cat(paste('max dissimilarity:', max.distance, '\n'))
	}
  
  
	# return the distance matrix, class = 'dissimilarity, dist'
	return(D)	
	}


pc.SPC <- function(s, vars, rescale.result=FALSE, ...){
	# default behavior: do not normalize D
	
	# extract horizons
	s.hz <- horizons(s)
	
	# extract site
	s.site <- site(s)
	sn <- names(s.site)
	
	# check for any site data, remove and a save for later
	if(any(vars %in% sn)) {
		
		# extract site-level vars
		matching.idx <- na.omit(match(sn, vars))
		site.vars <- vars[matching.idx]
		
		# remove from hz-level vars
		vars <- vars[-matching.idx]
		
		
    ## TODO: BUG!!! horizon data are rescaled via D/max(D) !!!
		## TODO: allow user to pass-in variable type information
		# compute dissimilarty on site-level data: only works with 2 or more variables
		# rescale to [0,1]
		if(length(site.vars) >= 2) {
			message(paste('site-level variables included:', paste(site.vars, collapse=', ')))
			d.site <- daisy(s.site[, site.vars], metric='gower')
			d.site <- rescale(d.site)
			
			# reset default behavior of hz-level D
			rescale.result=TRUE
			
			## TODO: there might be cases where we get an NA in d.site ... seems like it happens with boolean variables
			## ... but why ? read-up on daisy
			if(any(is.na(d.site))) {
				warning('NA in site-level dissimilarity matrix, replacing with min dissimilarity', call.=FALSE)
				d.site[which(is.na(d.site))] <- min(d.site, na.rm=TRUE)
			}
			
			## TODO: ordering of D_hz vs D_site ... assumptions safe?
		}
		
		else
			stop("cannot compute site-level dissimilarity with fewer than 2 variables", call.=FALSE)	
	}
	
	# setup a dummy D_site
	else
		d.site <- NULL
	
	## 
	## TODO: update this next part
	##
	# add old-style, hard-coded {id, top, bottom} column names        
	s.hz$id <- s.hz[[idname(s)]]
	hzDepthCols <- horizonDepths(s)
	s.hz$top <- s.hz[[hzDepthCols[1]]]
	s.hz$bottom <- s.hz[[hzDepthCols[2]]]
	
	# invoke data.frame method
	res <- profile_compare(s.hz, vars=vars, rescale.result=rescale.result, ...)
	
	# if we have site-level data and a valid D_site
	# combine via weighted average: using weights of 1 for now
	if(inherits(d.site, 'dist')) {
		res <- 	(res + d.site) / 2
		# re-scale to [0,1]
		res <- rescale(res)
	}
	
	# result is a distance matrix
	return(res)
}

##############
## S4 stuff ##
##############

## NOTE: don't mess with this!
# setup generic function
if (!isGeneric("profile_compare"))
  setGeneric("profile_compare", function(s, ...) standardGeneric("profile_compare"))

# temp interface to SPC class objects
setMethod(f='profile_compare', signature='SoilProfileCollection', definition=pc.SPC)

# temp interface for dataframes
setMethod(f='profile_compare', signature='data.frame', definition=pc)
