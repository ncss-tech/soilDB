## init
"SoilProfileCollection" <- function(
idcol='id',
depthcols=c('top','bottom'),
metadata=data.frame(stringsAsFactors=FALSE),
horizons,
site=data.frame(stringsAsFactors=FALSE),
sp=new('SpatialPoints'), # this is a bogus place-holder
diagnostic=data.frame(stringsAsFactors=FALSE)
){
  # creation of the object (includes a validity check)
  new("SoilProfileCollection", idcol=idcol, depthcols=depthcols, metadata=metadata, horizons=horizons, site=site, sp=sp, diagnostic=diagnostic)
}



## show
setMethod(
  f='show',
  signature='SoilProfileCollection',
  definition=function(object) {
  	n.profiles <- length(object)
  	
    cat("Object of class ", class(object), "\n", sep = "")
    cat("Number of profiles: ", n.profiles, "\n", sep="")
  	
  	if(n.profiles > 1)
			cat("Depth range: ", min(object), "-", max(object), " ", depth_units(object), "\n", sep="")
		
  	cat("\nHorizon attributes:\n")
  	print(head(horizons(object)))

		# in the presence of site data
    if (nrow(site(object)) > 0) {
      cat("\nSampling site attributes:\n")
      print(head(site(object)))
    }

    # presence of spatial data
    if(nrow(coordinates(object)) == n.profiles) {
    	cat('\nSpatial Data:\n')
    	show(object@sp@bbox)
    	show(proj4string(object))
    }

  }
)



## summary





##
## accessors
##

## ID column name
if (!isGeneric("idname"))
    setGeneric("idname", function(object, ...) standardGeneric("idname"))

setMethod("idname", "SoilProfileCollection",
  function(object)
    return(object@idcol)
)


## distinct profile IDs
if (!isGeneric("profile_id"))
  setGeneric("profile_id", function(object, ...) standardGeneric("profile_id"))

setMethod("profile_id", "SoilProfileCollection",
  function(object)
    unique(as.character(horizons(object)[[idname(object)]]))
)


## horizon depth column names
if (!isGeneric("horizonDepths"))
    setGeneric("horizonDepths", function(object, ...) standardGeneric("horizonDepths"))

setMethod("horizonDepths", "SoilProfileCollection",
  function(object)
    return(object@depthcols)
)


## spatial data: coordinates
setMethod("coordinates", "SoilProfileCollection",
  function(obj) {
  return(coordinates(obj@sp))
  }
)

## site data
if (!isGeneric("site"))
  setGeneric("site", function(object, ...) standardGeneric("site"))

# retrieves the site data frame
setMethod("site", "SoilProfileCollection",
  function(object) {
  return(object@site)
  }
)

## diagnostic horizons: stored as a DF, must be join()-ed to other data via ID
## note: ordering may or may not be the same as in site data
if (!isGeneric("diagnostic_hz"))
  setGeneric("diagnostic_hz", function(object, ...) standardGeneric("diagnostic_hz"))

setMethod(f='diagnostic_hz', signature='SoilProfileCollection',
  function(object){
  return(object@diagnostic)
  }
)


## horizon data
# returns a data.frame with horizons data
if (!isGeneric("horizons"))
  setGeneric("horizons", function(object, ...) standardGeneric("horizons"))

setMethod(f='horizons', signature='SoilProfileCollection',
  function(object){
  return(object@horizons)
  }
)

## metadata
# returns a data.frame
if (!isGeneric("metadata"))
  setGeneric("metadata", function(object, ...) standardGeneric("metadata"))

setMethod(f='metadata', signature='SoilProfileCollection',
  function(object){
  return(object@metadata)
  }
)

## depth_units
# returns a data.frame
if (!isGeneric("depth_units"))
  setGeneric("depth_units", function(object, ...) standardGeneric("depth_units"))

setMethod(f='depth_units', signature='SoilProfileCollection',
  function(object){
	u <- as.character(metadata(object)[['depth_units']])
	  # give a warning if not defined
	if(u == '')
	  message('Note: depth depth_units have not yet been defined.')

	return(u)
  }
)



##
## overloads
##


## concatentation
## NOTE: data are re-sorted according to idname(objects[[1]])
## TODO: duplicates in @sp will cause errors
## TODO: duplicates are removed in all other slots... does this make sense?
rbind.SoilProfileCollection <- function(...) {
	# setup some defaults
	options(stringsAsFactors=FALSE)
	
	# parse dots
	objects <- list(...)
	names(objects) <- NULL
	
	# short-circuits
	if(length(objects) == 0)
		return(NULL)
	if(length(objects) == 1)
		return(objects[1])
	
	# combine pieces
	# should have length of 1
	o.idname <- unique(lapply(objects, idname))
	o.depth.units <- unique(lapply(objects, depth_units))
	o.hz.depths <- unique(lapply(objects, horizonDepths))
	o.m <- unique(lapply(objects, metadata))
	o.coords <- unique(lapply(objects, function(i) ncol(coordinates(i))))
	o.p4s <- unique(lapply(objects, proj4string))
	
	# should have length > 1
	o.h <- lapply(objects, horizons)
	o.s <- lapply(objects, site)
	o.d <- lapply(objects, diagnostic_hz)
	o.sp <- lapply(objects, function(i) i@sp)
	
	# sanity checks:
	if(length(o.idname) > 1)
		stop('inconsistent ID names', call.=FALSE)
	if(length(o.depth.units) > 1)
		stop('inconsistent depth units', call.=FALSE)
	if(length(o.hz.depths) > 1)
		stop('inconsistent depth columns', call.=FALSE)
	if(length(o.m) > 1)
		stop('inconsistent metadata', call.=FALSE)
	
	# spatial data may be missing...
	if(length(o.coords) > 1)
		stop('inconsistent spatial data', call.=FALSE)
	if(length(o.p4s) > 1)
		stop('inconsistent CRS', call.=FALSE)
	
	# generate new SPC components
	o.h <- unique(do.call('rbind', o.h)) # horizon data, must be re-ordered
	o.s <- unique(do.call('rbind', o.s)) # site data, must be re-ordered
	o.d <- unique(do.call('rbind', o.d)) # diagnostic data, leave as-is
	
	# generate ording vector for horizon and site data based on new vector of profile IDs
  id <- o.idname[[1]] # copy ID name from first object into convenience variable
	new.hz.order <- order(o.h[[id]])
  new.site.order <- order(o.s[[id]])
  
  # re-order horizon/site data: NOTE funky syntax required to accomodate data.frames with only a single column
	o.h <- o.h[new.hz.order, , drop=FALSE]
  o.s <- o.s[new.site.order, , drop=FALSE]
  
	# spatial points require some more effort when spatial data are missing
	o.1.sp <- objects[[1]]@sp
	if(ncol(coordinates(o.1.sp)) == 1) # missing spatial data
		o.sp <- o.1.sp # copy the first filler
	
	## TODO: how can we make sure that unique-ness is enforced? 
	# not missing spatial data
	else { 
		o.sp <- do.call('rbind', o.sp) # rbind properly
    # re-order based on new ordering of IDs, from site data
		o.sp <- o.sp[new.site.order, ]
  }
  
	# make SPC and return
	res <- SoilProfileCollection(idcol=o.idname[[1]], depthcols=o.hz.depths[[1]], metadata=o.m[[1]], horizons=o.h, site=o.s, sp=o.sp, diagnostic=o.d)
	
  # warn user that data have been re-sorted
  message(paste('resulting SPC has been re-sorted according to', id))
  
	return(res)
	}


# return a concatenated vector of horizon + site names
# note that we strip out the ID column name from @site
setMethod("names", "SoilProfileCollection",
  function(x) {
  res <- c(horizons=names(horizons(x)), site=names(site(x))[-1])
  return(res)
  }
)

# overload min() to give us the min depth within a collection
setMethod(f='min', signature='SoilProfileCollection',
definition=function(x, v=NULL) {
  # get bottom depth column name
  hz_bottom_depths <- horizonDepths(x)[2]
  
  # optionally use a horizon-level property refine calculation
  if(!missing(v)) {
  	# combine bottom depths with IDs and variable
  	h <- horizons(x)[, c(hz_bottom_depths, idname(x), v)]
  }
  else {
  	# combine bottom depths with IDs
  	h <- horizons(x)[, c(hz_bottom_depths, idname(x))]
  }
  
  # filter out missing data
  h <- h[complete.cases(h), ]
  # compute max by ID
  d <- tapply(h[, 1], h[, 2], max, na.rm=TRUE)
  
  # return the shallowest depth
  return(min(d, na.rm=TRUE))
  }
)

# overload max() to give us the max depth within a collection
setMethod(f='max', signature='SoilProfileCollection',
definition=function(x, v=NULL){
	# get bottom depth column name
	hz_bottom_depths <- horizonDepths(x)[2]
	
	# optionally use a horizon-level property refine calculation
	if(!missing(v)) {
		# combine bottom depths with IDs and variable
		h <- horizons(x)[, c(hz_bottom_depths, idname(x), v)]
	}
	else {
		# combine bottom depths with IDs
		h <- horizons(x)[, c(hz_bottom_depths, idname(x))]
	}
	
	# filter out missing data
	h <- h[complete.cases(h), ]
	# compute max by ID
	d <- tapply(h[, 1], h[, 2], max, na.rm=TRUE)
	
  # return the deepest depth
  return(max(d, na.rm=TRUE))
  }
)

# overload length() to give us the number of profiles in the collection
setMethod(f='length', signature='SoilProfileCollection',
  definition=function(x){
  l <- length(profile_id(x))
  return(l)
  }
)

# overload nrow() to give us the number of horizons in the collection
setMethod(f='nrow', signature='SoilProfileCollection',
  definition=function(x){
  nrow(x@horizons)
  }
)


# overload unique() via digest eval of unique profiles
# currently only works with horizon-level attributes
setMethod(f='unique', signature='SoilProfileCollection',
definition=function(x, vars){
  if(require(digest)) {
	md5 <- profileApply(x, function(i) digest(unlist(horizons(i)[, vars])))

	# get unique hashes
	u.md5 <- unique(md5)

	# list profile idx by hash:
	profiles.by.hash <- sapply(u.md5, function(i) which(md5 == i), simplify=FALSE)

	# get an index of the first copy of each profile
	u.profiles <- sapply(profiles.by.hash, function(i) i[1])
	
	# return an index of unique profiles
	# down-grade to un-named vector of indices
	return(as.vector(u.profiles))
	}
  else
		stop('This function requres the `digest` package.', call.=FALSE)
	
  }
)




## standard column access: search horizons, then site
setMethod("$", "SoilProfileCollection",
  function(x, name) {

	# get names from site and hz data
	s.names <- names(site(x))
	h.names <- names(horizons(x))

	# when site data are initialized from an external DF, it is possible that
	# there will be duplicate column names
	if(name %in% h.names & name %in% s.names)
		warning('column name is present in horizon and site data, extracting from horizon data only', call.=FALSE)

	# get column from horizon data
    if (name %in% h.names)
      res <- horizons(x)[[name]]

    # otherwise check site data
    else
      if (name %in% s.names)
		res <- site(x)[[name]]

	  # if still missing return NULL
	  else
		res <- NULL

	return(res)
  }
)


## problem: when making new columns how  can the function determine where to insert the replacement>?
setReplaceMethod("$", "SoilProfileCollection",
  function(x, name, value) {
  	# extract hz and site data
  	h <- horizons(x)
		s <- site(x)

    # working with horizon data
    if (name %in% names(h)) {
      h[[name]] <- value
      horizons(x) <- h
      return(x)
      }
      
    # working with site data  
    if(name %in% names(s)) {
	  s[[name]] <- value
      # TODO: use site(x) <- s
      x@site <- s
      return(x)
      }
    
    # ambiguous: use length of replacement to determing: horizon / site   
    else {
      n.site <- nrow(s)
      n.hz <- nrow(h)
      l <- length(value)

      if(l == n.hz) {
      	h[[name]] <- value
	    horizons(x) <- h
	    return(x)
      	}
      
      if(l == n.site) {
      	s[[name]] <- value
      	# TODO: use site(x) <- s
      	x@site <- s
      	return(x)
      	}
	
	  else
	  	stop('length of replacement must equal number of sites or number of horizons')
    		
    }
  # done  
  }
)




## subset method for SoilProfileCollection objects
## s: site-level subsetting criteria (properly quoted)
## h: horizon-level subsetting criteria (properly quoted)
## result: SoilProfileCollection with all profiles that match _either_ criteria- i.e. greedy matching
if (!isGeneric("subsetProfiles"))
  setGeneric("subsetProfiles", function(object, s, h, ...) standardGeneric("subsetProfiles"))
  
setMethod("subsetProfiles", "SoilProfileCollection",
  function(object, s, h, ...) {
  	
  	# sanity checks
  	if(missing(s) & missing(h))
  		stop('must provide either, site or horizon level subsetting criteria', call.=FALSE)
  	
  	# extract parts
  	s.d <- site(object)
  	h.d <- horizons(object)
  	id.col <- idname(object)
  	object.ids <- profile_id(object)
  	
  	# subset using conventional data.frame methods
  	if(!missing(s))
  		s.d.sub.IDs <- subset(s.d, select=id.col, subset=eval(parse(text=s)))[, 1] # convert to vector
  	else
  		s.d.sub.IDs <- NA
  	
  	if(!missing(h))
  		h.d.sub.IDs <- subset(h.d, select=id.col, subset=eval(parse(text=h)))[, 1] # convert to vector
  	else
  		h.d.sub.IDs <- NA
  	
    # intersect IDs if s and h were used
    if(!missing(h) & !missing(s))
      matching.IDs <- intersect(s.d.sub.IDs, h.d.sub.IDs)
    
  	# if only h, or only s were used, then 
    else
  	  matching.IDs <- unique(na.omit(c(s.d.sub.IDs, h.d.sub.IDs)))
  	
  	# convert IDs into a numerical profile index
  	# note: no matches results in idx == 0
  	idx <- match(matching.IDs, object.ids)
  	
  	# subset SoilProfileCollection
  	return(object[idx, ])
  	}
)




### NOTE: this DOES NOT re-order data, only subsets!
##
## matrix / DF style access: only to horizon data
##
## i = profile index
## j = horizon / slice index
##
setMethod("[", "SoilProfileCollection",
  function(x, i, j, ...) {
		
  	# check for missing i and j
  	if(missing(i) & missing(j))
  		stop('must provide either a profile index or horizon/slice index, or both', call.=FALSE)
  	
  	# convert to integer
  	if(!missing(i)) {
  	  if(any(is.na(i)))
  	    stop('NA not permitted in profile index', call.=FALSE)
      # convert logical to integer per standard vector/list indexing rules (thanks Jos? Padarian for the suggestion!)
  	  if(is.logical(i)) i <- (1:length(x))[i]
  	  i <- as.integer(i)
  	}
    else # if no index is provided, the user wants all profiles
      i <- 1:length(x)

    # sanity check
    if(!missing(j)) {
      j <- as.integer(j)
      if(any(is.na(j)))
      stop('NA not permitted in horizon/slice index', call.=FALSE)
    }

    # extract requested profile IDs
    p.ids <- profile_id(x)[i]

    # extract all horizon data
    h <- horizons(x)
	
    # keep only the requested horizon data (filtered by pedon ID)
    h <- h[h[[idname(x)]] %in% p.ids, ]
    
    # keep only the requested site data, (filtered by pedon ID)
    s.all <- site(x)
    s.i <- which(s.all[[idname(x)]] %in% p.ids)
  	s <- s.all[s.i, , drop=FALSE] # need to use drop=FALSE when @site contains only a single column

    # subset spatial data if exists
    if(nrow(coordinates(x)) == length(x))
      sp <- x@sp[i]
    else
      sp <- x@sp
    
    # subset diagnostic data, but only if it exists
    # note that not all profiles have diagnostic hz data
    d <- diagnostic_hz(x)
    if(length(d) > 0) # some data
    	d <- d[which(d[[idname(x)]] %in% p.ids), ]
    
    
    # subset horizons/slices based on j --> only when j is given
    if(!missing(j))
      h <- ddply(h, idname(x), .fun=function(y) y[j, ])

    # if there is REAL data in @sp, and we only have 1 row of hz per coordinate- return SPDF
    # for now test for our custom dummy SP obj: number of coordinates == number of profiles
    # also need to test that there is only 1 horizon/slice per location
  	# only produces a SPDF when j index is present
    if(nrow(coordinates(x)) == length(x) & length(p.ids) == nrow(h) & !missing(j)) {
      # combine with coordinates
      cat('result is a SpatialPointsDataFrame object\n')
      # note that we are filtering based on 'i' - an index of selected profiles
			
      # since the order of our slices and coordinates are the same
      # it is safe to use 'match.ID=FALSE'
      # this gets around a potential problem when dimnames(x)[[1]] aren't consecutive 
      # values-- often the case when subsetting has been performed
      
      # if site data, join hz+site
      if(nrow(s) > 0) {
      	return(SpatialPointsDataFrame(as(x, 'SpatialPoints')[i, ], data=join(h, s, by=idname(x)), match.ID=FALSE))
      }
      # no site data
      else {
      	return(SpatialPointsDataFrame(as(x, 'SpatialPoints')[i, ], data=h, match.ID=FALSE))	
      }
    }

    # in this case there may be missing coordinates, or we have more than 1 slice of hz data
    else {
      SoilProfileCollection(idcol=x@idcol, depthcols=x@depthcols, metadata=x@metadata, horizons=h, site=s, sp=sp, diagnostic=d)
    }
    
  # done
  }
)


