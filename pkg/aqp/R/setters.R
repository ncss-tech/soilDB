##
## initialize metadata: object modification in-place
##
if (!isGeneric('metadata<-'))
  setGeneric('metadata<-', function(object, value) standardGeneric('metadata<-'))

setReplaceMethod("metadata", "SoilProfileCollection",
  function(object, value) {

	# quick sanity check
	if(nrow(value) > 1 | nrow(value) < 1)
	  stop("metadata should be a 1-row data frame", call.=FALSE)

	# otherwise assign
	object@metadata <- value

	# done
	return(object)
	}
)

##
## initialize depth_units: object modification in-place, depth_units stored in @metadata
##
if (!isGeneric('depth_units<-'))
  setGeneric('depth_units<-', function(object, value) standardGeneric('depth_units<-'))

setReplaceMethod("depth_units", "SoilProfileCollection",
  function(object, value) {

	# quick sanity check: character, length 1

	# keep existing metadata
	md <- metadata(object)

	# default depth_units are always in metadata
	# replace what ever is there
	md[['depth_units']] <- value

	# replace metadata
	metadata(object) <- md

	# done
	return(object)
	}
)


##
## depths<- setter method - to create AQP objects: sorts based on ID and top depth
##
if (!isGeneric('depths<-'))
  setGeneric('depths<-', function(object, value) standardGeneric('depths<-'))

setReplaceMethod("depths", "SoilProfileCollection",
	function(object, value) {
		message('This is already a SoilProfilecollection-class object, doing nothing.')
		object
	})


setReplaceMethod("depths", "data.frame",
  function(object, value) {
    if (inherits(value, "formula")) {
      # extract components of formula: 1. user id, 2. top, 3. bottom
      mf <- model.frame(value, object)
      res <- .initSPCfromMF(data=object, mf=mf)
    }
    else {
      if (inherits(value, "character")) { # initialization by colnames
	      mf <- object[,value]
	      res <- .initSPCfromMF(data=object, mf=mf)
      }
      else
	      stop('invalid initialization for SoilProfile object', call.=FALSE)
    }

    # add default metadata: depths are cm
    metadata(res) <- data.frame(depth_units='cm', stringsAsFactors=FALSE)
    
    # add default site data: profile IDs in same order as hz
    site.temp <- data.frame(xxx=profile_id(res), stringsAsFactors=FALSE)
    names(site.temp) <- idname(res)
    res@site <- site.temp
    
    # done
    return(res)
  }
)


##
## initialize SP/SPC objects from a model.frame
##
.initSPCfromMF <- function(data, mf){
  # get column names containing id, top, bottom
  nm <- names(mf)

  # re-order data: IDs, top hz depths
  new.order <- order(data[[nm[1]]], data[[nm[2]]])
  
  # check for factor-class ID
  if(class(data[[nm[1]]]) == 'factor') {
    warning('converting IDs from factor to character', call.=FALSE)
    data[[nm[1]]] <- as.character(data[[nm[1]]])
  }
    
  
  # create object
  depthcols <- c(nm[2], nm[3])
  res <- SoilProfileCollection(idcol=nm[1], depthcols=depthcols, horizons=data[new.order, ])
  
  # done
  return(res)
}


##
## initialize site data
##
if (!isGeneric('site<-'))
  setGeneric('site<-', function(object, value) standardGeneric('site<-'))

setReplaceMethod("site", "SoilProfileCollection",
  function(object, value) {
	# get the corresponding vector of IDs, will be used to compute distinct site attributes
    ids <- as.character(horizons(object)[[idname(object)]])

	# creation of site data from horizon data
    if (inherits(value, "formula")) {
      mf <- model.frame(value, horizons(object), na.action=na.pass)
      nm <- names(mf)
      mf <- data.frame(ids, mf, stringsAsFactors=FALSE) # don't automatically make strings into factors
      names(mf) <- c(idname(object), nm)
      object <- .createSiteFromHorizon(object, mf)
    }
    
    # creation of site data from an external data.frame via join(..., type='left')
    if (inherits(value, "data.frame")) {
      # get column names from proposed site, and existing horizons
      ns <- names(value)
      nh <- names(object@horizons)
      
      ## remove ID column from names(horizons)
      ID.idx <- match(idname(object), nh)
      
      # check to make sure there is no overlap in proposed site + hz variable names
      if(any(ns %in% nh[-ID.idx]))
        stop('duplicate names in new site / existing horizon data not allowed', call.=FALSE)
      
      # existing site data (may be absent == 0-row data.frame)
      s <- site(object)
      
      # join to existing data: by default it will only be idname(object)
      
      ## an appropriate ID must exist in 'value' AND @site for this to work
      # LEFT-join in - assumes that appropriate IDs exist in both @site and 'value'
      # we are suppressing the 'Joining by:' output from join()
      suppressMessages(site.new <- join(s, value, type='left'))
      
      # sanity check: site + new data should have same number of rows as original
      if(nrow(s) != nrow(site.new)) {
      	message(paste('original data (', nrow(s), ' rows) new data (', nrow(site.new), ' rows)', sep=''))
        stop('invalid join condition, site data not changed', call.=FALSE)
      }
            
      # look good, proceed
      object@site <- site.new
	  }
  	
    ## TODO: finer reporting on what the problem might be
    # check to make sure the the number of rows in @site is the same as length(object)
    if(length(object) != nrow(site(object))){
    	print(paste('pedons (', length(object), ') rows of site data (', nrow(site(object)), ')', sep=''))
    	stop('invalid site data, non-unique values present in horizon data?', call.=FALSE)
    }
    
    # done
    return(object)
  }
)

# update an SPC object:
# add site data
# remove named columns from horizons
# return new SPC object
.createSiteFromHorizon <- function(object, mf){
  # create a numeric index for named site columns, as we will remove them
  # from the horizon data
  names_attr <- names(mf)
  idx <- match(names_attr, names(horizons(object)))
  # remove the index to the ID columnm, as we do not want to remove this from
  # the horizon data !
  idx <- idx[-match(idname(object), names_attr)]
	
  # this will break when multiple horizons in the same pedon have different site data!
  # this seems to work fine in all cases, as we keep the ID column
  # and it ensures that the result is in the same order as the IDs
  new_site_data <- ddply(mf, idname(object),
      .fun=function(x) {
	      unique(x[, names_attr])
      }
  )

  # if site data is already present in the object, we don't want to erase it
  site_data <- join(site(object), new_site_data, by=idname(object))

  # remove the named site data from horizon_data
  object@horizons <- horizons(object)[, -idx]
	
  # replace existing site data
  object@site <- site_data

  # done
  return(object)
}


##
## horizon data replacement
##
## horizons<- setter method
##
if (!isGeneric('horizons<-'))
  setGeneric('horizons<-', function(object, value) standardGeneric('horizons<-'))

setReplaceMethod("horizons", "SoilProfileCollection",
  function(object, value) {
  # testing the class of the horizon data to add to the object
  if (!inherits(value, "data.frame"))
	  stop("value must be a data.frame", call.=FALSE)
  
  ## 
  ## not sure if this test is important... as sometimes we want to delete horizons
  ##
  # testing the number of rows of the horizon data
  # if (nrow(value) != nrow(object))
	  # stop("inconsistent number of rows")

  # basic test of ids:
  if(!idname(object) %in% names(value)) # is there a matching ID column in the replacement?
  	stop("there is no matching ID column in replacement", call.=FALSE)

  if(length(setdiff(unique(as.character(value[[idname(object)]])), profile_id(object))) > 0)
  	stop("there are IDs in the replacement that do not exist in the original data", call.=FALSE)

  # replacement: order by IDs, then top horizon boundary
  hz_top_depths <- horizonDepths(object)[1]
  object@horizons <- value[order(value[[idname(object)]], value[[hz_top_depths]]), ]

  # done
  return(object)
  }
)

##
## intit diagnotic horizon data
##
## NOTE: these data are likely to be free-form, may not exist for every profile, and are usually 1:many
##
if (!isGeneric('diagnostic_hz<-'))
  setGeneric('diagnostic_hz<-', function(object, value) standardGeneric('diagnostic_hz<-'))

setReplaceMethod("diagnostic_hz", "SoilProfileCollection",
  function(object, value) {
  
  # get the initial data
  d <- diagnostic_hz(object)
  
  # get column and ID names
  nm <- names(value)
  idn <- idname(object)
  pIDs <- profile_id(object)
  
  # testing the class of the new data
  if (!inherits(value, "data.frame"))
    stop("diagnostic horizon data must be a data.frame", call.=FALSE)
	
  # test for the special case where internally-used functions 
  # are copying over data from one object to another, and diagnostic_hz(obj) is a 0-row data.frame
  # short-circut, and return original object
  if(nrow(d) == 0 & nrow(value) == 0)
  	return(object)
  
  # test to make sure that our common ID is present in the new data
  if(! idn %in% nm)
  	stop(paste("diagnostic horizon data are missing a common ID:", idn), call.=FALSE)
  
  # test to make sure that at least one of the IDS in candidate data are present within SPC
  if(all( ! unique(value[[idn]]) %in% pIDs) )
  	stop('candidate diagnostic horizon data have no matching IDs in target object!', call.=FALSE)
  
  # warn user if some of the IDs in the candidate data are missing
  if(any( ! unique(value[[idn]]) %in% pIDs) ) {
  	warning('some records in candidate diagnostic horizon data have no matching IDs in target object', call.=FALSE)
    # print(value[value$peiid == unique(value[[idn]])[which(! unique(value[[idn]]) %in% pIDs)], ])
  }
  
  # if data are already present, warn the user
  if(nrow(d) > 0)
  	warning('overwriting existing diagnostic horizon data!', call.=FALSE)
  
  # copy data over
  object@diagnostic <- value
  
  # done
  return(object)
  }
)
