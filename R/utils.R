## 
## misc functions used by soilDB
##

## TODO: keep track of funky records in the soilDB.env

## TODO: consider toggling paralithic contact to FALSE when lithic contact is TRUE
# convert diagnostic horizon info into wide-formatted, boolean table
.diagHzLongtoWide <- function(d, feature = 'featkind', id = 'peiid') {
	
	# get unique vector of diagnostic hz
	d.unique <- na.omit(unique(as.character(d[[feature]])))
	
	# init list for storing initial FALSE for each ID / diag kind
	l <- vector(mode='list')
	
	# add unique id
	l[[id]] <- unique(d[[id]])
	
	# make a vector of FALSE, matching the length of unique ID
	f <- rep(FALSE, times=length(l[[id]]))
	
	# iterate over diagnostic hz kind
	for(i in d.unique) {
		# fill this list element with FALSE
		l[[i]] <- f
		# lookup those ID with this feature
		matching.id <- d[[id]][which(d[[feature]] == i)]
		# toggle FALSE-->TRUE for these pedons
		l[[i]][which(l[[id]] %in% matching.id)] <- TRUE
	}
	
	# convert to DF
	return(as.data.frame(l))
		
}


## TODO: this may need some review
## convert horizon designation pieces info into wide-formatted, boolean table
.hzSuffixLongtoWide <- function(d) {
	
	# get unique vector of all hzdesgnsuffix
	d.unique <- na.omit(unique(d$desgnsuffix))
	
	# init list for storing initial FALSE for each phiid / desgnsuffix
	l <- vector(mode='list')
	
	# add unique phiid
	l[['phiid']] <- unique(d$phiid)
	
	# make a vector of FALSE, matching the length of unique phiid
	f <- rep(FALSE, times=length(l[['phiid']]))
	
	# iterate over hzdesgnsuffix
	for(i in d.unique) {
		# fill this list element with FALSE
		l[[i]] <- f
		# lookup those phiid with this horizon suffix
		matching.phiid <- d$phiid[which(d$desgnsuffix == i)]
		# toggle FALSE-->TRUE for these horizons
		l[[i]][which(l[['phiid']] %in% matching.phiid)] <- TRUE
	}
	
	# convert to DF
	return(as.data.frame(l))
		
}


## TODO: this may need some review
## try and pick the best possible taxhistory record
.pickBestTaxHistory <- function(d) {
	
	# add a method field (a character)
	d$selection_method <- NA_character_
	
	# short-circuit: 1 row
	if(nrow(d) < 2) {
	  d$selection_method <- 'single record'
	  return(d)
	}
	
	## TODO: this must be a POSIXct / Date class object, if not results will be incorrect
	# try to get the most recent
	d.order <- order(d$classdate, decreasing=TRUE)
	
	# if there are multiple (unique) dates, return the most recent
	if(length(unique(d$classdate)) > 1) {
		d$selection_method <- 'most recent'
		return(d[d.order[1], ])
	}
	
	# otherwise, return the record with the least number of missing cells
	# if there are the same number of missing cells, the first record is returned
	n.na <- apply(d, 1, function(i) length(which(is.na(i))))
	best.record <- which.min(n.na)
	
	d$selection_method <- 'least missing data'
	return(d[best.record, ])
}


## TODO: this may need some review
## try and pick the best possible ecosite record
.pickBestEcosite <- function(d) {
	
	# add a method field
	d$es_selection_method <- NA_character_
	
	# try to get the most recent:
	d.order <- order(d$ecositecorrdate, decreasing=TRUE)
	
	# if there are multiple (unique) dates, return the most recent
	if(length(unique(d$ecositecorrdate)) > 1) {
		d$es_selection_method <- 'most recent'
		return(d[d.order[1], ])
	}
	
	# otherwise, return the record with the least number of missing cells
	# if there are the same number of missing cells, the first record is returned
	n.na <- apply(d, 1, function(i) length(which(is.na(i))))
	best.record <- which.min(n.na)
	
	d$es_selection_method <- 'least missing data'
	return(d[best.record, ])
}

## TODO: this may need some review
## try and pick the best possible ecosite record
# .pickBestOtherVeg <- function(d) {
#   
#   # add a method field
#   d$es_selection_method <- NA
#   
#   # try to get the most recent:
#   d.order <- order(d$ecositecorrdate, decreasing=TRUE)
#   
#   # if there are multiple (unique) dates, return the most recent
#   if(length(unique(d$ecositecorrdate)) > 1) {
#     d$es_selection_method <- 'most recent'
#     return(d[d.order[1], ])
#   }
#   
#   # otherwise, return the record with the least number of missing cells
#   # if there are the same number of missing cells, the first record is returned
#   n.na <- apply(d, 1, function(i) length(which(is.na(i))))
#   best.record <- which.min(n.na)
#   
#   d$es_selection_method <- 'least missing data'
#   return(d[best.record, ])
# }

## https://github.com/ncss-tech/soilDB/issues/84
## TODO: https://github.com/ncss-tech/soilDB/issues/47
## 2015-11-30: short-circuits could use some work, consider pre-marking mistakes in calling function
# attempt to format "landform" records into a single string
# note: there are several assumptions made about the data, 
# see "short-circuits" used when there are funky data
.formatLandformString <- function(i.gm, uid = NULL, name.sep='|') {

  # get the current group of rows by unique ID (either passed by caller or calculated)
  if (is.null(uid) | length(uid) == 0)
    soiliid <- unique(i.gm$peiid) # backwards compatible with hardcoded "peiid"
  else 
    soiliid <- uid
  
  if (is.null(soiliid))
    return(NULL)
  
  if (length(soiliid) > 1)
    stop('data are from multiple pedon records')
  
  # sanity check: this functionican only be applied to data from a single pedon
  if (length(soiliid) > 1)
    stop('data are from multiple pedon records')
  
  # subset geomorph data to landforms, landscape and microfeature
  i.ls <- i.gm[which(i.gm$geomftname == 'landscape'), ]
  i.mf <- i.gm[which(i.gm$geomftname == 'microfeature'), ]
  i.gm <- i.gm[which(i.gm$geomftname == 'landform'), ] 
  
  # subset landform data to RV (or NULL) hillslope position?
  # i.gm <- i.gm[which(i.gm$cosurfmorphhpprv | is.na(i.gm$cosurfmorphhpprv)), ]
  
  # allow for NA's
  if (nrow(i.gm) == 0) {
    return(data.frame(peiid = soiliid,
                      landform_string = NA_character_,
                      landscape_string = NA_character_,
                      microfeature_string = NA_character_,
                      hillslopeprof_string = NA_character_,
                      geompos_string = NA_character_,
                      slopeshape_string = NA_character_,
                      microrelief_string = NA_character_,
                      stringsAsFactors = FALSE))
  }
  
  # landscape
  landsc.string <- paste0(unique(i.ls$geomfname), collapse = name.sep)
  
  # microfeature
  mf.string <- paste0(unique(i.mf$geomfname[!is.na(i.mf$geomfname)]), collapse = name.sep) 
  
  # 2d hillslope position
  hspp <- unique(paste0(i.gm$hillslopeprof, ifelse(i.gm$cosurfmorphhpprv, "*",""))[!is.na(i.gm$hillslopeprof)])
  surf2d.string <- paste0(hspp, collapse = name.sep)
  
  # 3d geomorphic description
  surf3d.string <- paste0(unique(paste0(c(i.gm$geomposmntn[!is.na(i.gm$geomposmntn)], 
                                        i.gm$geomposhill[!is.na(i.gm$geomposhill)], 
                                        i.gm$geompostrce[!is.na(i.gm$geompostrce)], 
                                        i.gm$geomposflats[!is.na(i.gm$geomposflats)])), collapse = name.sep), collapse = name.sep)
  
  # microrelief
  surfmr.string <- paste0(unique(i.gm$geomicrorelief[!is.na(i.gm$geomicrorelief)]), collapse = name.sep)
  
  # surface shape
  surfss <- paste0(i.gm$shapeacross[!is.na(i.gm$shapeacross)], "/", i.gm$shapedown[!is.na(i.gm$shapedown)])
  surfss.string <- paste0(unique(surfss[nchar(surfss) > 1]), collapse = name.sep)
  
  landsc.string[landsc.string == ""] <- NA
  mf.string[mf.string == ""] <- NA
  surf2d.string[surf2d.string == ""] <- NA
  surf3d.string[surf3d.string == ""] <- NA
  surfmr.string[surfmr.string == ""] <- NA
  surfss.string[surfss.string == ""] <- NA
  
  # short-circuit: if any geomfeatid are NA, then we don't know the order
  # string together as-is, in row-order
  if(any(is.na(i.gm$geomfeatid))) {
    
    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      message(paste0('Using row-order. NA in geomfeatid:', soiliid))
    
    ft.string <- paste(unique(i.gm$geomfname), collapse = name.sep)
    return(data.frame(
      peiid = soiliid,
      landform_string = ft.string,
      landscape_string = landsc.string,
      microfeature_string = mf.string,
      hillslopeprof_string = surf2d.string,
      geompos_string = surf3d.string,
      slopeshape_string = surfss.string,
      geomicrorelief_string = surfmr.string,
      stringsAsFactors = FALSE
    ))
  }
  
  # short-circuit: if any feature exists on itself, then use row-order
  # string together as-is, in row-order
  if(any(na.omit(c(i.gm$geomfeatid == i.gm$existsonfeat), FALSE))) {
    
    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      message(paste0('Using row-order. Error in exists-on logic:', soiliid))
    
    ft.string <- paste(unique(i.gm$geomfname), collapse=name.sep)
    return(data.frame(
      peiid = soiliid,
      landform_string = ft.string,
      landscape_string = landsc.string,
      microfeature_string = mf.string,
      hillslopeprof_string = surf2d.string,
      geompos_string = surf3d.string,
      slopeshape_string = surfss.string,
      microrelief_string = surfmr.string,
      stringsAsFactors = FALSE
    ))
  }
  
  # get an index to the top-most and bottom-most features
  # only 1 row should match these criteria
  top.feature <- which(! i.gm$geomfeatid %in% i.gm$existsonfeat)
  bottom.feature <- which(! i.gm$existsonfeat %in% i.gm$geomfeatid)
  
  # short-circut: incorrect data-population, usually duplicate entries with IDs that make no sense: use row-order
  if(length(top.feature) == 0 & length(bottom.feature) == 0) {
    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      warning(paste0('Using row-order. Error in exists-on logic: ', soiliid), call.=FALSE)
    
    ft.string <- paste(unique(i.gm$geomfname), collapse=name.sep)
    return(data.frame(
      peiid = soiliid,
      landform_string = ft.string,
      landscape_string = landsc.string,
      microfeature_string = mf.string,
      hillslopeprof_string = surf2d.string,
      geompos_string = surf3d.string,
      slopeshape_string = surfss.string,
      microrelief_string = surfmr.string,
      stringsAsFactors = FALSE
    ))
  }
   
  ## short-circuit: only 1 row, and exists-on logic is wrong, use row-order
  if(nrow(i.gm) == 1 & length(top.feature) == length(bottom.feature)) {
    
    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      warning(paste0('Using row-order. Single row / error in exists-on logic: ', soiliid), call.=FALSE)
    
    ft.string <- paste(unique(i.gm$geomfname), collapse=name.sep)
    return(data.frame(
      peiid = soiliid,
      landform_string = ft.string,
      landscape_string = landsc.string,
      microfeature_string = mf.string,
      hillslopeprof_string = surf2d.string,
      geompos_string = surf3d.string,
      slopeshape_string = surfss.string,
      microrelief_string = surfmr.string,
      stringsAsFactors = FALSE
    ))
  }
  
  # short-circuit: if the exists-on logic is wrong, use row-order
  if(length(top.feature) > 1 | length(bottom.feature) > 1) {
    
    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      warning(paste0('Using row-order. Incorrect exists-on specification: ', soiliid), call.=FALSE)
    
    ft.string <- paste(unique(i.gm$geomfname), collapse=name.sep)
    return(data.frame(
      peiid = soiliid,
      landform_string = ft.string,
      landscape_string = landsc.string,
      microfeature_string = mf.string,
      hillslopeprof_string = surf2d.string,
      geompos_string = surf3d.string,
      slopeshape_string = surfss.string,
      microrelief_string = surfmr.string,
      stringsAsFactors = FALSE
    ))
  }
  
  # init a vector to store feature names
  ft.vect <- vector(mode = 'character', length = length(unique(i.gm[, 'geomfname'])))
  # the first feature is the top-most feature
  this.feature.idx <- top.feature
  
  # loop over features, until the bottom-most feature
  i <- 1
  while(i <= length(ft.vect)){
    # get the current feature
    f.i <- i.gm$geomfname[this.feature.idx]
    
    if(length(f.i) == 0) {
      print(this.feature.idx)
      print(i.gm)
    }
      
    
    # assign to vector of labels
    ft.vect[i] <- f.i
    
    # jump to the next feature
    this.feature.idx <- which(i.gm$geomfeatid == i.gm$existsonfeat[this.feature.idx])
    i <- i + 1
  }
  
  # paste into single string
  ft.string <- paste(unique(ft.vect), collapse = name.sep)
  
  # done!
  return(data.frame(
    peiid = soiliid,
    landform_string = ft.string,
    landscape_string = landsc.string,
    microfeature_string = mf.string,
    hillslopeprof_string = surf2d.string,
    geompos_string = surf3d.string,
    slopeshape_string = surfss.string,
    microrelief_string = surfmr.string,
    stringsAsFactors = FALSE
  ))
}


## https://github.com/ncss-tech/soilDB/issues/84
# attempt to flatten site parent material data into 2 strings
.formatParentMaterialString <- function(i.pm, uid = NULL, name.sep='|') {
  
  # get the current group of rows by unique ID (either passed by caller or calculated)
  if (is.null(uid))
    u.siteiid <- unique(i.pm$siteiid)
  else 
    u.siteiid <- uid
  
  if (is.null(u.siteiid))
    return(NULL)
  
  # sanity check: this function can only be applied to data from a single site
  if (length(u.siteiid) > 1)
    stop('data are from multiple site records')
  
  # subset sitepm data to remove any with NA for pmkind
  i.pm <- i.pm[which(!is.na(i.pm$pmkind)), ]
  
  # if there is no data, then return NULL
  if (nrow(i.pm) == 0) {
    return(data.frame(siteiid = u.siteiid,
                      pmkind = NA_character_[length(u.siteiid)],
                      pmorigin = NA_character_[length(u.siteiid)],
                      stringsAsFactors = FALSE))
  }
  
  # short-circuit: if any pmorder are NA, then we don't know the order
  # string together as-is, in row-order
  if (any(is.na(i.pm$pmorder))) {
    # optional information on which sites have issues
    if(getOption('soilDB.verbose', default=FALSE))
      warning(paste0('Using row-order. NA in pmorder:', u.siteiid), call.=FALSE)
  } else {
    # there are no NAs in pmorder --> sort according to pmorder
    i.pm <- i.pm[order(i.pm$pmorder), ]
  }
  
  # composite strings and return
  str.kind <- paste(i.pm$pmkind, collapse=name.sep)
  str.origin <- paste(unique(i.pm$pmorigin), collapse=name.sep)
  
  return(data.frame(siteiid=u.siteiid, pmkind=str.kind, pmorigin=str.origin, stringsAsFactors=FALSE))
}


## TODO add .formatcoLandscapeString
# g <- split(geo, geo$coiid)
# 
# gg <- lapply(g, function(i) {
#   
#   idx <- which(i$geomftname == 'landscape')
#   id <- i$coiid[1]
#   
#   res <- data.frame(
#     coiid = id,
#     geomfname = paste(i$geomfname[idx], collapse = '/'),
#     stringsAsFactors = FALSE
#   )
#   
#   return(res)
# })


## https://github.com/ncss-tech/soilDB/issues/84
# 2017-03-13: attempt to format COMPONENT "landform" records into a single string
# note: there are several assumptions made about the data, 
# see "short-circuits" used when there are funky data
.formatcoLandformString <- function(i.gm, name.sep='|') {
  
  # hacks to make R CMD check --as-cran happy:
  u.peiid <- NULL
  
  # get the current 
  u.coiid <- unique(i.gm$coiid)
  
  if(length(u.coiid) == 0)
    return(data.frame(coiid=NA_integer_, landform_string=NA, stringsAsFactors=FALSE)[0,])
  
  # sanity check: this function can only be applied to data from a single component
  if(length(u.coiid) > 1)
    stop('data are from multiple component records')
  
  # subset geomorph data to landforms
  i.gm <- i.gm[which(i.gm$geomftname == 'landform'), ]
  
  # allow for NA's
  if(nrow(i.gm) == 0)
    return(data.frame(coiid=u.coiid, landform_string=NA, stringsAsFactors=FALSE))
  
  # short-circuit: if any geomfeatid are NA, then we don't know the order
  # string together as-is, in row-order
  if(any(is.na(i.gm$geomfeatid))) {
    
    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      message(paste0('Using row-order. NA in geomfeatid: ', u.peiid))
    
    ft.string <- paste(unique(i.gm$geomfname), collapse=name.sep)
    return(data.frame(coiid=u.coiid, landform_string=ft.string, stringsAsFactors=FALSE))
  }
  
  # short-circuit: if any feature exists on itself, then use row-order
  # string together as-is, in row-order
  if(any(na.omit(c(i.gm$geomfeatid == i.gm$existsonfeat), FALSE))) {
    
    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      message(paste0('Using row-order. Error in exists-on logic: ', u.coiid))
    
    ft.string <- paste(unique(i.gm$geomfname), collapse=name.sep)
    return(data.frame(coiid=u.coiid, landform_string=ft.string, stringsAsFactors=FALSE))
  }
  
  # get an index to the top-most and bottom-most features
  # only 1 row should match these criteria
  top.feature <- which(! i.gm$geomfeatid %in% i.gm$existsonfeat)
  bottom.feature <- which(! i.gm$existsonfeat %in% i.gm$geomfeatid)
  
  ## short-circuit: only 1 row, and exists-on logic is wrong, use row-order
  if(nrow(i.gm) == 1 & length(top.feature) == length(bottom.feature)) {
    
    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      warning(paste0('Using row-order. Single row / error in exists-on logic: ', u.coiid), call.=FALSE)
    
    ft.string <- paste(unique(i.gm$geomfname), collapse=name.sep)
    return(data.frame(coiid=u.coiid, landform_string=ft.string, stringsAsFactors=FALSE))
  }
  
  # short-circuit: if the exists-on logic is wrong, use row-order
  if(length(top.feature) > 1 | length(bottom.feature) > 1) {
    
    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      warning(paste0('Using row-order. Incorrect exists-on specification: ', u.coiid), call.=FALSE)
    
    ft.string <- paste(unique(i.gm$geomfname), collapse=name.sep)
    return(data.frame(coiid=u.coiid, landform_string=ft.string, stringsAsFactors=FALSE))
  }
  
  # short circuit: if there is circularity in the exists-on logic, use row-order
  # example coiid: 2119838
  if(length(top.feature) < 1 | length(bottom.feature) < 1) {
    
    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      warning(paste0('Using row-order. Incorrect exists-on specification: ', u.coiid), call.=FALSE)
    
    ft.string <- paste(unique(i.gm$geomfname), collapse=name.sep)
    return(data.frame(coiid=u.coiid, landform_string=ft.string, stringsAsFactors=FALSE))
  }
  
  # init a vector to store feature names
  ft.vect <- vector(mode='character', length=nrow(i.gm))
  # the first feature is the top-most feature
  this.feature.idx <- top.feature
  
  # loop over features, until the bottom-most feature
  i <- 1
  while(i <= nrow(i.gm)){
    # get the current feature
    f.i <- i.gm$geomfname[this.feature.idx]
    
    # likely an error condition, print some debugging info
    if(length(f.i) == 0) {
      print(this.feature.idx)
      print(i.gm)
    }
    
    
    # assign to vector of labels
    ft.vect[i] <- f.i
    
    # jump to the next feature
    this.feature.idx <- which(i.gm$geomfeatid == i.gm$existsonfeat[this.feature.idx])
    i <- i + 1
  }
  
  # paste into single string
  ft.string <- paste(ft.vect, collapse=name.sep)
  
  # done!
  return(data.frame(coiid=u.coiid, landform_string=ft.string, stringsAsFactors=FALSE))
}


## https://github.com/ncss-tech/soilDB/issues/84
# attempt to flatten component parent material data into 2 strings
.formatcoParentMaterialString <- function(i.pm, name.sep='|') {
  
  .Deprecated(".formatParentMaterialString")
  
  # get the current site
  u.coiid <- unique(i.pm$coiid)
  
  if(length(u.coiid) == 0)
    return(data.frame(coiid=NA_integer_, pmkind=NA, pmorigin=NA, stringsAsFactors=FALSE)[0,])
  
  # sanity check: this function can only be applied to data from a single site
  if(length(u.coiid) > 1)
    stop('data are from multiple site records')
  
  # subset sitepm data to remove any with NA for pm_kind
  i.pm <- i.pm[which(!is.na(i.pm$pmkind)), ]
  
  # if there is no data, then return a DF formatted as if there were data
  if(nrow(i.pm) == 0)
    return(data.frame(coiid=u.coiid, pmkind=NA, pmorigin=NA, stringsAsFactors=FALSE))
  
  # short-circuit: if any pmorder are NA, then we don't know the order
  # string together as-is, in row-order
  if(any(is.na(i.pm$pmorder))) {
    # optional information on which sites have issues
    if(getOption('soilDB.verbose', default=FALSE))
      warning(paste0('Using row-order. NA in pmorder:', u.coiid), call.=FALSE)
  }
  else{
    # there are no NAs in pmorder --> sort according to pmorder
    i.pm <- i.pm[order(i.pm$pmorder), ]
  }
  
  # composite strings and return
  str.kind <- paste(i.pm$pmkind, collapse=name.sep)
  str.origin <- paste(unique(i.pm$pmorigin), collapse=name.sep)
  
  return(data.frame(coiid=u.coiid, pmkind=str.kind, pmorigin=str.origin, stringsAsFactors=FALSE))
}


# attempt to flatten multiple ecosite entries into 1 string
.formatEcositeString <- function(i.esd, name.sep='|') {
  # get the current site
  u.coiid <- unique(i.esd$coiid)
  
  if(length(u.coiid) == 0)
    return(data.frame(coiid=NA_integer_, ecosite_id=NA, ecosite_name=NA, stringsAsFactors=FALSE)[0,])
  
  # sanity check: this function can only be applied to data from a single component
  if(length(u.coiid) > 1)
    stop('data are from multiple component records')
  
  # subset othervegcl data to remove any with NA for othervegclass
  i.esd <- i.esd[which(!is.na(i.esd$ecositeid)), ]
  
  # if there is no data, then return a DF formatted as if there were data
  if(nrow(i.esd) == 0)
    return(data.frame(coiid=u.coiid, ecosite_id=NA, ecosite_name=NA, stringsAsFactors=FALSE))
  
  # short-circuit: if any otherveg are NA, then we don't know the order
  # string together as-is, in row-order
  # if(any(is.na(i.ov$pmorder))) {
  #   # optional information on which sites have issues
  #   if(getOption('soilDB.verbose', default=FALSE))
  #     warning(paste0('Using row-order. NA in pmorder:', u.coiid), call.=FALSE)
  # }
  # else{
  #   # there are no NAs in pmorder --> sort according to pmorder
  #   i.ov <- i.ov[order(i.ov$pmorder), ]
  # }
  
  # composite strings and return
  str.ecoid <- paste(i.esd$ecositeid, collapse=name.sep)
  str.econm <- paste(unique(i.esd$ecositenm), collapse=name.sep)
  
  return(data.frame(coiid=u.coiid, ecosite_id=str.ecoid, ecosite_name=str.econm, stringsAsFactors=FALSE))
}



# attempt to flatten multiple other veg class entries into 1 string
.formatOtherVegString <- function(i.ov, name.sep='|') {
  # get the current site
  u.coiid <- unique(i.ov$coiid)
  
  if(length(u.coiid) == 0)
    return(data.frame(coiid=NA_integer_, othervegid=NA, othervegclass=NA, stringsAsFactors=FALSE)[0,])
  
  # sanity check: this function can only be applied to data from a single component
  if(length(u.coiid) > 1)
    stop('data are from multiple component records')
  
  # subset othervegcl data to remove any with NA for othervegclass
  i.ov <- i.ov[which(!is.na(i.ov$ovegclid)), ]
  
  # if there is no data, then return a DF formatted as if there were data
  if(nrow(i.ov) == 0)
    return(data.frame(coiid=u.coiid, othervegid=NA, othervegclass=NA, stringsAsFactors=FALSE))
  
  # short-circuit: if any otherveg are NA, then we don't know the order
  # string together as-is, in row-order
  # if(any(is.na(i.ov$pmorder))) {
  #   # optional information on which sites have issues
  #   if(getOption('soilDB.verbose', default=FALSE))
  #     warning(paste0('Using row-order. NA in pmorder:', u.coiid), call.=FALSE)
  # }
  # else{
  #   # there are no NAs in pmorder --> sort according to pmorder
  #   i.ov <- i.ov[order(i.ov$pmorder), ]
  # }
  
  # composite strings and return
  str.ovegid <- paste(i.ov$ovegclid, collapse=name.sep)
  str.ovegclnm <- paste(unique(i.ov$ovegclname), collapse=name.sep)
  
  return(data.frame(coiid=u.coiid, othervegid=str.ovegid, othervegclass=str.ovegclnm, stringsAsFactors=FALSE))
}





# function to estimate the thickness of horizons and diagnostic features
.test_thk <- function(x) {
  names(x) <- gsub("^hz|^feat", "", names(x))
  std <- with(x, data.frame(
    l  = depb_l - dept_h,
    rv = depb_r - dept_r,
    h  = depb_h - dept_l
  ))
  
  idx <- std < 0
  idx[is.na(idx)] <- FALSE
  
  if (any(idx)) {
    std_new <- std
    std_flip <- with(x, data.frame(
      l = depb_l - dept_l,
      rv = depb_r - dept_r,
      h = depb_h - dept_h
    ))
    std_new[idx] <- std_flip[idx]
  }
  
  if (exists("std_new")) {
    idx2 <- which(std_new$l > std$rv)
    std_new$l[idx2] <- std$r[idx2]
    idx3 <- which(std$h < std$rv)
    std_new$h[idx2] <- std$r[idx2]
  } else std_new <- std
  
  return(std_new)
}



# impute "not populated" into freqcl and "201" into dept_r & depb_r if !is.na(freqcl)
.cosoilmoist_prep <- function(df, impute, stringsAsFactors) {
  
  # cache original column names
  orig_names <- names(df)
  
  # relabel names
  names(df) <- gsub("^soimoist", "", names(df))
  old_names <- "stat"
  new_names <- "status"
  names(df)[names(df) %in% old_names] <- new_names
  
  
  # refactoring frequency levels, not sure why these aren't sorted naturally
  flod_lev <- levels(df$flodfreqcl)
  pond_lev <- levels(df$pondfreqcl)
  idx      <- c(1, 5, 2, 7, 3, 4, 6)
  df <- within(df, {
    flodfreqcl = levels(flodfreqcl)[as.integer(flodfreqcl)]
    flodfreqcl = factor(flodfreqcl, levels = flod_lev[idx])
    pondfreqcl = levels(pondfreqcl)[as.integer(pondfreqcl)]
    pondfreqcl = factor(pondfreqcl, levels = pond_lev[idx])
  })
  
  
  # impute NA freqcl values, default = "not populated"
  if (impute == TRUE) {
    
    missing <- "Not populated"
    lev_flodfreqcl <- c(missing, levels(df$flodfreqcl))
    lev_pondfreqcl <- c(missing, levels(df$pondfreqcl))
    lev_status <- c(missing, levels(df$status))
    
    df <- within(df, {
      # replace NULL RV depths with 201 cm if pondfreqcl or flodqcl is not NULL
      dept_r[is.na(dept_r) & (!is.na(pondfreqcl) | !is.na(flodfreqcl))] = 201
      depb_r[is.na(depb_r) & (!is.na(pondfreqcl) | !is.na(flodfreqcl))] = 201
      
      # replace NULL L and H depths with the RV
      dept_l = ifelse(is.na(dept_l), dept_r, dept_l)
      dept_h = ifelse(is.na(dept_h), dept_r, dept_h)
      
      depb_l = ifelse(is.na(depb_l), depb_r, depb_l)
      depb_h = ifelse(is.na(depb_h), depb_r, depb_h)
      
      # replace NULL freqcl with "Not_Populated"
      status = factor(levels(status)[status], levels = lev_status)
      flodfreqcl = factor(levels(flodfreqcl)[flodfreqcl], levels = lev_flodfreqcl)
      pondfreqcl = factor(levels(pondfreqcl)[pondfreqcl], levels = lev_flodfreqcl)
      
      status[is.na(status)] <- missing
      flodfreqcl[is.na(flodfreqcl)] <- missing
      pondfreqcl[is.na(pondfreqcl)] <- missing
    })
  }
  
  # convert factors to strings
  idx <- unlist(lapply(df, is.factor))
  if (stringsAsFactors == FALSE & any(idx)) {
    df[idx] <- lapply(df[idx], as.character)
  }
  
  return(df)
}


## https://github.com/ncss-tech/soilDB/issues/84
# Prep of the component parent material
# flatten multiple records into 1 cokey
.copm_prep <- function(df, db = NULL) {
  
  if (db == "SDA") {
    
    # flatten
    idx <- duplicated(df$cokey)
    
    if (any(idx) & nrow(df) > 0) {
      dups_idx <- df$cokey %in% df[idx, "cokey"]
      dups     <- df[dups_idx, ]
      nodups   <- df[!dups_idx, ]
      
      # hack to make CRAN check happy
      pmorigin = NA; pmkind = NA;
      
      dups_clean <- {
        transform(dups, 
                  idx_pmo = !is.na(pmorigin),
                  idx_pmk = !is.na(pmkind)
                  ) ->.;
        split(., .$cokey, drop = TRUE) ->.
        lapply(., function(x) { data.frame(
          x[1, c("cokey", "pmgroupname")],
          pmkind   = paste(x[x$idx_pmk, "pmkind"  ][order(x[x$idx_pmk, "pmorder"])],   collapse = " over "),
          pmorigin = paste(x[x$idx_pmo, "pmorigin"][order(x[x$idx_pmo, "pmorder"])], collapse = " over "),
          stringsAsFactors = FALSE
          )}) ->.
        do.call("rbind", .) ->.;
      }
      nodups <- nodups[! names(df) %in% c("copmgrpkey", "pmorder")]
      
      df <- rbind(nodups, dups_clean)  
      df <- df[order(df$cokey), ]
      row.names(df) <- 1:nrow(df)
      } else df <- df[! names(df) %in% c("copmgrpkey", "pmorder")]
    
    
    # replace "" with NA
    vars <- c("pmorigin", "pmkind")
    idx <- unlist(lapply(df, is.character))
    idx <- names(df) %in% vars & idx
    df[, idx] <- lapply(df[, idx], function(x) ifelse(x == "", NA, x))
    }
  
  return(df)
  }


## https://github.com/ncss-tech/soilDB/issues/84
# Prep of the component geomorphic description
# flatten multiple records into 1 cokey
.cogmd_prep <- function(df, db = NULL) {
  
  # rename LIMS columns and sort comma separated lists
  if (db == "LIMS") {
    # rename columns
    vars <- c("pmkind_grp", "pmorigin_grp", "gc_mntn", "gc_hill", "gc_trce", "gc_flats", "hs_hillslopeprof", "ss_shapeacross", "ss_shapedown")
    new_names <- c("pmkind", "pmorigin", "mntn", "hill", "trce", "flats", "hillslopeprof", "shapeacross", "shapedown")
    idx <- which(names(df) %in% vars)
    names(df)[idx] <- new_names
    
    # hack to make CRAN check happy
    mntn = NULL; hill = NULL; trce = NULL; flats = NULL; hillslopeprof = NULL;
    
    ## TODO: consider using is(x, 'character')
    df <- within(df, {
      if (class(mntn) == "character") {
        mntn  = sapply(strsplit(mntn, ", "),  function(x) paste(sort(unlist(x)), collapse = ", "))
        }
      if (class(hill) == "character") {
        hill  = sapply(strsplit(hill, ", "),  function(x) paste(sort(unlist(x)), collapse = ", "))
        }
      if (class(trce) == "character") {
        trce  = sapply(strsplit(trce, ", "),  function(x) paste(sort(unlist(x)), collapse = ", "))
        }
      if (class(flats) == "character") {
        flats = sapply(strsplit(flats, ", "), function(x) paste(sort(unlist(x)), collapse = ", "))
        }
      if (class(hillslopeprof) == "character") {
        hillslopeprof = sapply(strsplit(hillslopeprof, ", "), function(x) paste(sort(unlist(x)), collapse = ", "))
        }
      })
    }
  
  
  # flatten the SDA results to 1 cokey
  if (db == "SDA") {
    
    # flatten
    idx <- duplicated(df$cokey)
    
    if (any(idx) & nrow(df) > 0) {
      dups_idx <- df$cokey %in% df[idx, "cokey"]
      dups     <- df[dups_idx, ]
      nodups   <- df[!dups_idx, ]
      
      dups_clean <- {
        split(dups, dups$cokey, drop = TRUE) ->.
        lapply(., function(x) { data.frame(
          cokey = x$cokey[1],
          landscape     = paste(unique(x$landscape),           collapse = " and "),
          landform      = paste(unique(x$landform),            collapse = " on  "),
          mntn          = paste(sort(unique(x$mntn)),          collapse = ", "   ),
          hill          = paste(sort(unique(x$hill)),          collapse = ", "   ),
          trce          = paste(sort(unique(x$trce)),          collapse = ", "   ),
          flats         = paste(sort(unique(x$flats)),         collapse = ", "   ),
          shapeacross   = paste(sort(unique(x$shapeacross)),   collapse = ", "   ),
          shapedown     = paste(sort(unique(x$shapedown)),     collapse = ", "   ),
          hillslopeprof = paste(sort(unique(x$hillslopeprof)), collapse = ", "),
          stringsAsFactors = TRUE
        )}) ->.
        do.call("rbind", .) ->.
      }
      nodups <- nodups[! names(nodups) %in% c("geomfeatid", "existsonfeat")]
      
      df <- rbind(nodups, dups_clean)  
      df <- df[order(df$cokey), ]
      row.names(df) <- 1:nrow(df)
      } else df <- df[! names(df) %in% c("geomfeatid", "existsonfeat")]
    }
  
  vars <- c("landscape", "landform", "mntn", "hill", "trce", "flats", "hillslopeprof")
  idx <- unlist(lapply(df, is.character))
  idx <- names(df) %in% vars & idx
  df[, idx] <- lapply(df[, idx], function(x) ifelse(x %in% c("", "NA"), NA, x))
  
  # hack to make CRAN check happy
  mntn = NA; hill = NA; trce = NA; flats = NA; shapeacross = NA; shapedown = NA;
  
  # combine geompos and shapes
  if (nrow(df) > 0) {
    df <- within(df, {
      geompos = NA
      geompos = paste(mntn, hill, trce, flats, sep = ", ")
      geompos = gsub("NA", "", geompos)
      geompos = gsub("^, |^, , |^, , , |, $|, , $|, , , $", "", geompos)
      geompos = gsub(", , ", ", ", geompos)
      geompos[geompos == ""] = NA
      
      ssa = NA # slope shape across
      ssd = NA # slope shape down
      slopeshape = NA

      ssa = gsub("Concave", "C", shapeacross)
      ssa = gsub("Linear",  "L", ssa)
      ssa = gsub("Convex",  "V", ssa)

      ssd = gsub("Concave", "C", shapedown)
      ssd = gsub("Linear",  "L", ssd)
      ssd = gsub("Convex",  "V", ssd)

      slopeshape = paste0(ssd, ssa, sep = "")
      slopeshape[slopeshape %in% c("NANA", "")] = NA
      })
    df[c("ssa", "ssd")] <- NULL
  } else df <- cbind(df, geompos = as.character(NULL))
  
  ss_vars <- c("CC", "CV", "CL", "LC", "LL", "LV", "VL", "VC", "VV")
  if (all(df$slopeshape[!is.na(df$slopeshape)] %in% ss_vars)) {
    df$slopeshape <- factor(df$slopeshape, levels = ss_vars)
    df$slopeshape <- droplevels(df$slopeshape)
  }
  
  hs_vars <- c("Toeslope", "Footslope", "Backslope", "Shoulder", "Summit")
  if (all(df$hillslopeprof[!is.na(df$hillslopeprof)] %in% hs_vars)) {
    df$hillslopeprof <- factor(df$hillslopeprof, levels = hs_vars)
    df$hillslopeprof <- droplevels(df$hillslopeprof)
  }
  
  hill_vars <- c("Base Slope", "Head Slope", "Side Slope", "Free Face", "Nose Slope", "Crest", "Interfluve")
  if (all(df$hill[!is.na(df$hill)] %in% hill_vars)) {
    df$hill <- factor(df$hill, levels = hill_vars)
    df$hill <- droplevels(df$hill)
  }
  
  flats_vars <- c("Dip", "Talf", "Rise")
  if (all(df$flats[!is.na(df$flats)] %in% flats_vars)) {
    df$flats <- factor(df$flats, levels = flats_vars)
    df$flats <- droplevels(df$flats)
  }
  
  trce_vars <- c("Tread", "Riser")
  if (all(df$trce[!is.na(df$trce)] %in% trce_vars)) {
    df$trce <- factor(df$trce, levels = trce_vars)
    df$trce <- droplevels(df$trce)
  }

  return(df)
  }
