# ##############################################################
# ## slotting functions ##
# ##############################################################
# 
# # data.frame method
# slab.DF <- function(data, fm, progress='none', ...) {
# 	## important: change the default behavior of data.frame and melt
# 	opt.original <- options(stringsAsFactors = FALSE)
# 	
# 	# sanity check:
# 	if(! inherits(fm, "formula"))
# 		stop('must provide a valid formula: groups ~ var1 + var2 + ...', call.=FALSE)
# 	
# 	# extract components of the formula:
# 	g <- all.vars(update(fm, .~0)) # left-hand side
# 	vars <- all.vars(update(fm, 0~.)) # right-hand side
# 	
# 	# check for bogus left/right side problems with the formula
# 	if(any(g %in% names(data)) == FALSE & g != '.') # bogus grouping column
# 		stop('group name either missing from formula, or does match any columns in dataframe', call.=FALSE)
# 	
# 	if(any(vars %in% names(data)) == FALSE) # bogus column names in right-hand side
# 		stop('column names in formula do not match column names in dataframe', call.=FALSE)
# 	
# 	# currently this will only work with integer depths
# 	if(any( !as.integer(data$top[data$top != 0]) == data$top[data$top != 0] ) | any( !as.integer(data$bottom) == data$bottom))
# 		stop('This function can only accept integer horizon depths', call.=FALSE)
# 	
# 	# if there is no left-hand component in the formula, we are aggregating all data in the collection
# 	if(g == '.') { 
# 		g <- 'all.profiles' # add new grouping variable to horizons
# 		data[, g] <- 1
# 	}
# 	
# 	# convert into long format
# 	d.long <- melt(data, id.vars=c('id','top','bottom', g), measure.vars=vars)
# 	
# 	## TODO work on fixing this
# 	# temp hack: make a column called 'prop' ... as soil.slot is expecting this!
# 	d.long$prop <- d.long$value
# 	
# 	# TODO: plyr 1.8 now supports parallel computation with ddply() research and implement
# 	# apply slotting group-wise and return in long format
# 	# note '...' is gobbled by soil.slot()
# 	d.slotted <- ddply(d.long, .variables=c('variable', g), .progress=progress, .parallel=getOption('AQP_parallel', default=FALSE), .fun=soil.slot, ...) 
# 	
# 	# convert tops and bottoms to integers
# 	d.slotted$top <- as.integer(d.slotted$top)
# 	d.slotted$bottom <- as.integer(d.slotted$bottom)
# 	
# 	# reset options:
# 	options(opt.original)
# 	
# 	# done
# 	return(d.slotted)
# }
# 
# 
# # setup generic function
# if (!isGeneric("slab"))
# 	setGeneric("slab", function(data, fm, ...) standardGeneric("slab"))
# 
# 
# ##
# ## TODO: update to current standards, see slice()
# ##
# # current interface to data.frame objects
# setMethod(f='slab', signature='data.frame', definition=slab.DF)
# 
# ## TODO: integrate methods for SPC / data.frame
# ## TODO: there is no simple way to get back an SPC object, as there are several vars / slab returned
# ## TODO: use.wts doesn't work with SPC objects
# # temp interface to SPC class objects
# setMethod(f='slab', signature='SoilProfileCollection', definition=slab2)
# 
# 
# ## 
# ## calculation of segment-wise summary statistics
# ## 
# ##TODO: figure out how to do weighted tables
# seg.summary <- function(l.recon, prop.class, user.fun, prop.levels=NA, class_prob_mode=1) {
# 	
# 	# this is the number of slices contributing the the slice-wise aggregate
# 	contributing_fraction <- sapply(l.recon, function(i) length(na.omit(i)) / length(i))
# 	
# 	# sequence for iterating over multiple lists
# 	l.seq <-1:length(l.recon)
# 	
# 	# numeric variables
# 	if(prop.class %in% c('numeric','integer')) {
# 				
# 		# mean
# 		p.mean <- sapply(l.recon, mean, na.rm=TRUE)
# 		
# 		# SD: this estimate may be too low when using a segment size > 1 cm
# 		p.sd <- sqrt(sapply(l.recon, wtd.var, na.rm=TRUE))
# 		
# 		# Harrell-Davis quantile estimator: better for small-med. size samples
# 		q.probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
# 		p.quantiles <- data.frame(t(sapply(l.recon, hdquantile, probs=q.probs, na.rm=TRUE)))
# 		names(p.quantiles) <- paste('p.q', round(q.probs * 100), sep='')
# 		
# 		
# # 		# profile-weighted statistics
# # 		if(use.wts == TRUE) {
# # 			# weighted mean calculation: reduces to standard mean, when weights are equal
# # 			p.wtmean <- sapply(l.seq, function(i) wtd.mean(l.recon[[i]], weights=l.recon.wts[[i]], na.rm=TRUE)) 
# # 			
# # 			# weighted standard deviation: reduces to regular SD when weights are equal
# # 			p.wtsd <- sqrt(sapply(l.seq, function(i) wtd.var(l.recon[[i]], weights=l.recon.wts[[i]], normwt=TRUE, na.rm=TRUE)))
# # 			
# # 			# weighted quantiles: reduces to standard quantiles when weights are equal
# # 			p.wtquantiles <- data.frame(t(sapply(l.seq, function(i) wtd.quantile(l.recon[[i]], weights=l.recon.wts[[i]], probs=q.probs, normwt=TRUE, na.rm=TRUE))))
# # 			names(p.wtquantiles) <- paste('p.wtq', round(q.probs * 100), sep='')
# # 			} # end processing weighted mean, SD, quantiles
# 		
# 		
# 		# try user defined function
# 		# TODO: catch errors
# 		if(!is.null(user.fun)) {
# 			p.user <- try( sapply(l.recon, user.fun) )
# 			
# 				df.stats <- data.frame(contributing_fraction, p.mean, p.sd, p.quantiles, p.user)
# 			}
# 		# no user function	
# 		else {
# 				df.stats <- data.frame(contributing_fraction, p.mean, p.sd, p.quantiles)
# 			}
# 		
# 		} # end processing numeric variables
# 	
# 	
# 	## TODO: wtd.table() does not return objects similar to table()... can't use it right now
# 	## xtabs() is another approach, but it drops the count of NA
# 	## xtabs(wts ~ obs, data=data.frame(wts=wts, obs=tf))
# 	# categorical variables
# 	else {
# 		# get a vector of all possible categories
# 		# note that l.recon contains factor codes
# 		p.unique.classes <- as.vector(na.omit(unique(unlist(l.recon))))
# 		
# 		# tabular frequences for complete set of possible categories
# 		p.table <- sapply(l.seq, function(i, cpm=class_prob_mode, wts=l.recon.wts) {
# 			tf <- factor(l.recon[[i]], levels=p.unique.classes, labels=prop.levels[p.unique.classes])
# 			
# 			# probabilities are relative to number of contributing profiles
# 			if(cpm == 1) {
# 			  tb <- table(tf, useNA='no')
# 			  # convert to proportions
# 			  pt <- prop.table(tb)
# 			  }
# 			
# 			# probabilities are relative to total number of profiles
# 			else if(cpm == 2) {
# 			  tb <- table(tf, useNA='always')
# 			  # convert to proportions, 
# 			  # the last column will be named 'NA', and contains the tally of NAs --> remove it
# 			  pt <- prop.table(tb)
# 			  pt <- pt[-length(pt)]
# 			  }
# 			  
# 			return(pt)
# 			} 
# 		)
# 		
# 		# convert into a dataframe: if there are > 1 classes, 
# 		# then we must transpose p.table when converting to a data.frame
# 		if(length(p.unique.classes) > 1)
# 			p.prop <- data.frame(t(p.table))
# 			
# 		# when there is only 1 class, things are more complicated:
# 		# 1. no need to transpose p.table
# 		# 2. need to manually add the class name
# 		else {
# 			p.prop <- data.frame(p.table)
# 			names(p.prop) <- prop.levels[p.unique.classes]
# 			}
# 		
# 		# combine with contributing fraction
# 		df.stats <- data.frame(contributing_fraction, p.prop)
# 		}
# 	
# 	# done
# 	return(df.stats)
# 	}
# 
# 
# 
# 
# 
# ## this function will break when horizon boundaries do not make sense
# ## 
# # TODO: we only need x.recon for 1cm aggregation, otherwise l.recon is used
# # TODO: return the number of profiles + number of unique horizons when using custom segmenting interval
# # TODO: replace by() with equivilant plyr functions
# soil.slot <- function(data, seg_size=NA, seg_vect=NA, strict=FALSE, user.fun=NULL, class_prob_mode=1) {
# 	
# 	#################################################################################
# 	##### Initialization: check for fatal errors, and do some clean-up
# 	#################################################################################
# 	
# 	# currently this will only work with integer depths
# 	if(any( !as.integer(data$top[data$top != 0]) == data$top[data$top != 0] ) | any( !as.integer(data$bottom) == data$bottom))
# 		stop('This function can only accept integer horizon depths', call.=FALSE)
# 	
# 	# no NA allowed in top or bottom
# 	hz.test.top <- is.na(data$top)
# 	hz.test.bottom <- is.na(data$bottom)
# 	
# 	if(any(hz.test.top)) {
# 		print(data[which(hz.test.top), ])
# 		stop('NA in horizon top boundary', call.=FALSE)
# 		}
# 		
# 	if(any(hz.test.bottom)) {
# 		print(data[which(hz.test.bottom), ])
# 		stop('NA in horizon bottom boundary', call.=FALSE)
# 		}
# 	
# 	# can't pass in a bogus aggregate function
# 	if(!is.null(user.fun) & !is.function(user.fun)) 
# 		stop(paste('`', user.fun, '` is not a function', sep=''), call.=FALSE)
# 		
# 	
# 	# re-level id factor according to account for subsets
# 	data$id <- factor(data$id)
# 	
# 	# what is the datatype of 'prop'
# 	prop.class <- class(data$prop)
# 	
# 	# if we have a character, then convert to factor
# 	if(prop.class == 'character') {
# 		message('Note: converting categorical variable to factor.')
# 		data$prop <- factor(data$prop)
# 		}
# 	
# 	if(prop.class == 'factor') {
# 		# save the levels of our categorical variable
# 		prop.levels <- levels(data$prop) 
# 		}
# 	
# 	# for numerical variables, set this to NA
# 	else
# 		prop.levels <- NA
# 		
# 	# get the max depth for the entire dataset
# 	max_d <- max(data$bottom)
# 	
# 
# 	#################################################################################
# 	##### Step 1: unroll profiles in the collection: result is a list    ############
#   ##### TODO: this could be done with slice()                          ############
# 	#################################################################################
# 	x.unrolled <- dlply(data, "id", .fun=function(i, m=max_d) {
# 		
# 		u <- try(unroll(top=i$top, bottom=i$bottom, prop=i$prop, max_depth=m, strict=strict))
# 		
# 		## TODO: could be better
# 		# check for a non-NULL attribute of 'class'
# 		# this will only happen when there was an error
# 		if( !is.null(attr(u, 'class'))) {
# 			if(attr(u, 'class') == 'try-error') {
# 				print(i)
# 				stop('Error: bad horizon structure')
# 				}
# 			}
# 		else
# 			return(u)
# 		} )
# 	
# 	
# 	#######################################################################################
# 	##### Step 2: reconstitute into a matrix with 1:n-segment rows, and n-pedons columns ##
# 	##### TODO: use lists for everything, so that segment size does not affect agg. calcs #
# 	##### TODO: don't use hard-coded weight column
# 	#######################################################################################
# 	# values
# 	x.recon <- sapply(x.unrolled, '[')
# 	
#   # ## TODO: weights should be stored as a 'site' atttribute in an SPC object
# 	# # weights
# 	# if(use.wts == TRUE) {
# 		# message('Note: profile weights are still experimental, use with caution!')
# 		
# 		# # unroll a weight vector for each pedon
# 		# x.unrolled.wts <- by(data, data$id, function(i, m=max_d) unroll(top=i$top, bottom=i$bottom, prop=i$wt, max_depth=m))
# 		
# 		# # reconstitute weights:
# 		# x.recon.wts <- sapply(x.unrolled.wts, '[')
# 		# x.recon.wts <- x.recon.wts / max(x.recon.wts, na.rm=TRUE)
# 		
# 		# # clean-up
# 		# rm(x.unrolled.wts); gc()
# 		# }
# 
# 
# 	## TODO: this is wasteful, as we can work with the x.unrolled instead
# 	# convert to lists
# 	l.recon <- by(x.recon, 1:max_d, unlist)
# 		
# 	# # optionally setup weight list
# 	# if(use.wts == TRUE)
# 		# l.recon.wts <- by(x.recon.wts, 1:max_d, unlist)
# 	
# 	
# 	
# 	############################################################################################
# 	##### Step 3a: generate a segmenting index and compute stats along user-defined segments  ##
# 	############################################################################################
# 	
# 	# if we have a regular-interval segment size, re-group the data following the segmenting id
# 	# note that we are testing for the default value of seg_size and seg_vect
# 	# must be a better way to do this..
# 	if(!missing(seg_size) | !missing(seg_vect)) {
# 				
# 		# use a user-defined segmenting vector
# 		if(!missing(seg_vect)) {
# 			# if seg_vect starts with 0: simple case
# 			if(seg_vect[1] == 0)
# 				wind.idx <- rep(seg_vect[-1], times=diff(seg_vect))[1:max_d]
# 			
# 			# user only cares about some slab of soil  > 0 and < max_depth
# 			# should only be a seg_vect of length 2
# 			else {
# 				if(length(seg_vect) != 2)
# 					stop('seg_vect must either start from 0, or contain two values between 0 and the soil depth')
# 				
# 				# obvious
# 				slab.thickness <- diff(seg_vect)
# 				# how many slices of NA before the slab?
# 				padding.before <- rep(NA, times=seg_vect[1])
# 				# how many slices of NA afer the slab
# 				padding.after <- rep(NA, times=max_d - seg_vect[2])
# 				# make a new label for the slab
# 				new.label <- paste(seg_vect, collapse='-')
# 				# generate an index for the slab
# 				slab.idx <- rep(new.label, times=slab.thickness)
# 				# generate the entire index: padding+slab+padding = total number of slices (max_d)
# 				wind.idx <- c(padding.before, slab.idx, padding.after)
# 				}
# 			}
# 			
# 		# using a fixed-interval segmenting vector
# 		else {
# 			# generate a vector of unique segment ids
# 			segment_label <- 1:((max_d/seg_size)+1)
# 			
# 			# generate combined segment id vector
# 			# truncating at the max depth
# 			wind.idx <- rep(segment_label, each=seg_size)[1:max_d]
# 			}
# 		
# 		
# 		# generate a list, where each entry is a vector corresponding to the collection of 
# 		# values from all profiles, for those depths defined by each slice
# 		# we can't directly rbind this list together into a DF, because there are cases where the last
# 		# entry has a shorter length than all of the other entries
# 		# this is caused by a seg_size that does not divide evenly into our max depth (max_d)
# 		l.recon <- by(x.recon, wind.idx, unlist)
# 		
# 		# # optionally setup weight list
# 		# if(use.wts == TRUE)
# 			# l.recon.wts <- by(x.recon.wts, wind.idx, unlist)
# 		
# 		
# 		# user-defined segmenting vector, starting from 0
# 		if(!missing(seg_vect)) {
# 			
# 			# if the user requests multiple slices that are beyond the length of the deepest profile
# 			# throw an error
# 			if(length(which(seg_vect > max_d)) > 1)
# 				stop(paste('multiple requested segments extend beyond the maximum soil depth within the profile collection: ', paste(seg_vect[which(seg_vect > max_d)], collapse=','), sep=''), call.=FALSE)
# 				
# 			# get length and lower boundary of the requested segments
# 			len.seg_vect <- length(seg_vect)
# 			max.seg_vect <- max(seg_vect)
# 			
# 			# the actual max depth may be less than the requested segments
# 			# in that case we will need to truncate the horizon label vector to max_d
# 			if(max_d < max.seg_vect) {
# 				seg_vect_legal_idx <- which( (seg_vect - max_d) <= 0)
# 				sv_clean <- c(seg_vect[seg_vect_legal_idx], max_d)
# 				warning(paste('Note: truncating requested lower segment (', max.seg_vect, ') to max profile depth (', max_d, ').', sep=''), call.=FALSE)
# 				}
# 				
# 			# the actual depth may be more than, or equal to, the deepest requested segment
# 			# in that case we are truncating aggregation to the deepest requested lower boundary
# 			else {
# 				sv_clean <- seg_vect
# 				}
# 		
# 			
# 			# generate segment tops and bottoms
# 			# this generates an extra row sometimes, check for it below
# 			df.top_bottom <- data.frame(top=sv_clean[-len.seg_vect], bottom=sv_clean[-1])
# 			
# 			
# 			# check for lower horizon where top == bottom, and remove it
# 			bad_hz_list_TF <- with(df.top_bottom, top == bottom)
# 			
# 			if(TRUE %in% bad_hz_list_TF) {
# 				bad_hz_list_idx <- which(bad_hz_list_TF)
# 				warning(paste('removing horizon with 0 thickness (hz ', bad_hz_list_idx, ')', sep=''), call.=FALSE)
# 				df.top_bottom <- df.top_bottom[-bad_hz_list_idx, ]
# 				}
# 						
# 			} # done with user-defined segment vector
# 			
# 		# using a fixed-interval segmenting vector
# 		else {
# 			# get the length of our segmented data set
# 			# and generate a new sequence of depths
# 			len <- length(l.recon)
# 			l.seq <- 1:len
# 			dz <- l.seq * seg_size
# 			
# 			# generate segment tops and bottoms
# 			df.top_bottom <- data.frame(top=c(0,dz[-length(dz)]), bottom=c(dz[-len], max_d))
# 								
# 			# done with fixed-interval segmenting vector
# 			}
# 		
# 		# compute segment-wise summary statistics
# 		df.stats <- seg.summary(l.recon, prop.class, user.fun, prop.levels, class_prob_mode)
# 		} # done with segmenting
# 	
# 	
# 	###############################################################
# 	##### Step 3b: compute stats along single-interval segments  ##
# 	###############################################################
# 	else {
# 	
# 		# make the top and bottom hz labels
# 		df.top_bottom <- data.frame(top=0:(max_d-1), bottom=1:max_d)
# 		
# 		# compute row-wise summary statistics
# 		df.stats <- seg.summary(l.recon, prop.class, user.fun, prop.levels, class_prob_mode)
# 		} # done with 1-unit interval aggregation
# 
# 	
# 	
# 	
# 	## form into dataframe for returning to the user 
# 	# this is usually where we have problems, caused by bad horizon boundaries
# 	if(nrow(df.top_bottom) == nrow(df.stats)) {
# 		x.slotted <- data.frame(df.top_bottom, df.stats)
# 		}
# 	# something is wrong
# 	else {
# 		stop('The number of rows in aggregate data do not match number of segments. This was probably caused by incorrect horizon boundaries.')
# 		}
# 	
# 	# covert tops / bottoms to integers
# 	x.slotted$top <- as.integer(x.slotted$top)
# 	x.slotted$bottom <- as.integer(x.slotted$bottom)
# 	
# 	# done
# 	return(x.slotted)
# 	}
# 
