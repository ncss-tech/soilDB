## this scales fairly well:
## n = (100, 1000, 10000) --> (0.53, 4.21, 38) seconds

## TODO: this function cannot deal with overlapping horizons (usually an error):  > 1 row / slice
## it would be useful to support these kind of data, as many lab-sampled sites have sub-samples

# this function is run on the horizon data, once for each depth slice
get.slice <- function(h, id, top, bottom, vars, z, include='top', strict=TRUE) {
  
  # 1. get indices to rows matchings current depth slice (z)
  # this is the default method
  if(include == 'top')
    idx <- which(z >= h[[top]] & z < h[[bottom]])  
  # not sure why someone would use this approach, but include it anyways  
  if(include == 'bottom')
  	idx <- which(z > h[[top]] & z <= h[[bottom]])

  # 2. extract data.frame along slice, and named vars + id
  h <- h[idx, c(id, vars)]
  
  # 2.5 compute fraction missing
  # if there is only 1 variable, don't try to compute this value
  # if all data are missing NA is returned
  h$.pctMissing <- apply(as.matrix(h[, vars]), 1, function(i, n=length(vars)) length(which(is.na(i))) / n)
  
  # 3. QA/QC
  # how many unique IDs?
  l.ids <- length(unique(h[[id]]))   
  # how many rows in the result?
  n.res <- nrow(h)
  
  # more rows than IDs --> bad horizonation
  if(l.ids != n.res) {
  	if(strict == TRUE) {
  	  # get offending IDs
  	  id.tab <- table(h[[id]])
  	  bad.ids <- paste(names(id.tab)[which(id.tab > 1)], collapse=', ')
  	  stop(paste('bad horizonation in IDs:', bad.ids), call.=FALSE)
  	  }
  	
  	# looser interp of the data... issue warning and return multuple rows/ID
  	# join(..., match='first') will correct the problem
    else
      warning('Bad horizonation detected, first matching horizon selected. Use strict=TRUE to enforce QA/QC.')
  	}
  
  # done: return subset of original data + pct not NA
  return(h)
  }


## TODO: further optimization should be possible.
## this is a much more robust + fast version of slice.slow
## needs a little more testing, and then will be ready
slice.fast <- function(object, fm, top.down=TRUE, just.the.data=FALSE, strict=TRUE){
  
  ## important: change the default behavior of data.frame and melt
  opt.original <- options(stringsAsFactors = FALSE)
  
  # test for logical input
  if(! inherits(fm, "formula"))
    stop('must provide a valid formula: ~ var1 + var2 + ...', call.=FALSE)

  # extract components of the formula:
  formula <- str_c(deparse(fm, 500), collapse="")
  elements <- str_split(formula, fixed("~"))[[1]]
  formula <- lapply(str_split(elements, "[+*]"), str_trim)

  # TODO: this will have to be changed when we implement no LHS = all slices
  if (length(formula) > 2)
    stop("please provide a valid formula", call.=FALSE)

  # extract parsed formula components
  vars <- formula[[2]] # RHS, simple enough
  # LHS: could be either single integer or vector of slices
  z <- as.numeric(eval(parse(text=formula[[1]])))

  # get horizons + depth column names + ID column name
  h <- horizons(object)
  hd <- horizonDepths(object)
  top <- hd[1] ; bottom <- hd[2] # convenience vars
  id <- idname(object)
  id.order <- profile_id(object) # this is the original ordering of profiles
    
  # check for bogus left/right side problems with the formula
  if(any(z < 0) | any(is.na(z)))
    stop('z-slice must be >= 1', call.=FALSE)

  ## TODO: this will have to be updated for z-slices defined by data in @site
  if(! class(z) %in% c('numeric','integer')) # bogus z-slice
		stop('z-slice must be either numeric or integer', call.=FALSE)

  # check for '.' --> all variables, minus ID/depths
  if(any(vars == '.')) {
  	nh <- names(h)
  	cols.to.remove.idx <- match(c(id, top, bottom), nh)
  	vars <- nh[-cols.to.remove.idx]
  }
  	
  
  # check for column names that don't exist
  if(any(vars %in% names(h)) == FALSE) # bogus column names in right-hand side
		stop('column names in formula do not match any horizon data', call.=FALSE)

  
  ## extract all vars by slice_i
  # pre-allocate storage as list
  hd.slices <- vector(mode='list', length=length(z))
  # prepare an index for the list
  slice.idx <- seq_along(z)
  
  # convert h into an imutable data.frame for speed
  # h <- idata.frame(h)
  
  # iterate over this index
  for(slice.i in slice.idx) {
    
    # extract all vars for current slice
    # TODO: this is wasteful as the entire pile of horizons is passed to get.slice in each iteration of the loop
    m.i.sub <- get.slice(h, id=id, top=top, bottom=bottom, vars=vars, z=z[slice.i], strict=strict)
    
    # join with original IDs in order to account for NA, or bad horizonation
    d <- data.frame(temp_id=id.order)
    names(d) <- id
    
    ## BUG: join doesn't work when ID is a factor
    m.i <- join(d, m.i.sub, by=id, type='left', match='first')
    
    # add depth range:
    # top-down, means that the slice starts from the user-defined depths (default)
    if(top.down) {
      m.i[[top]] <- z[slice.i]      # "top"
      m.i[[bottom]] <- z[slice.i] + 1  # "bottom"
    }
    # otherwise, the slice starts at the bottom (why would someone do this?)
    else {
      m.i[[top]] <- z[slice.i] - 1 # "top"
      m.i[[bottom]] <- z[slice.i]     # "bottom"
    }
    # save to the list
    hd.slices[[slice.i]] <- m.i
    }
  
  # convert list into DF
  hd.slices <- ldply(hd.slices)
  
  # re-order by id, then top
  # keep only data we care about
  # note that we have a new column in there used to store pct not NA
  hd.slices <- hd.slices[order(match(hd.slices[[id]], id.order), hd.slices[[top]]), c(id, top, bottom, vars, '.pctMissing')]
  
  # if we just want the data:
  if(just.the.data)
    return(hd.slices)

  # if spatial data and only a single slice: SPDF
  if(nrow(coordinates(object)) == length(object) & length(z) == 1) {
    cat('result is a SpatialPointsDataFrame object\n')
    # check for site data, if present - join to our sliced data
    if(nrow(site(object)) > 0 )
      hd.slices <- join(hd.slices, site(object), by=id)
    
    # since the order of our slices and coordinates are the same
    # it is safe to use 'match.ID=FALSE'
    # this gets around a potential problem when dimnames(object)[[1]] aren't consecutive 
    # values-- often the case when subsetting has been performed
    return(SpatialPointsDataFrame(coordinates(object), data=hd.slices, match.ID=FALSE))
    }
  
  
  # otherwise return an SPC, be sure to copy over the spatial data
  depths(hd.slices) <- as.formula(paste(id, '~', top, '+', bottom))
  hd.slices@sp <- object@sp
  
  # if site data: return an SPC + @site
  # note that we should have a proper setter for this
  if(nrow(site(object)) > 0 )
    hd.slices@site <- site(object)
  
  # copy over any diagnostic features
  diagnostic_hz(hd.slices) <- diagnostic_hz(object)
  
  # copy over metadata
  metadata(hd.slices) <- metadata(object)
  
  # reset options:
  options(opt.original)
  
  # done
  return(hd.slices)
  }


## slice:
if (!isGeneric("slice"))
  setGeneric("slice", function(object, fm, top.down=TRUE, just.the.data=FALSE, strict=TRUE) standardGeneric("slice"))


## TODO: allow the use of site data (PSC etc.) to determine the z-slice
setMethod(f='slice', signature='SoilProfileCollection', slice.fast)
