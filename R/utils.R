## 
## misc functions used by soilDB
##

## TODO: keep track of funky records in the soilDB.env

## TODO: consider toggling paralithic contact to FALSE when lithic contact is TRUE
# convert diagnostic horizon info into wide-formatted, boolean table
.diagHzLongtoWide <- function(d) {
	
	# get unique vector of diagnostic hz
	d.unique <- na.omit(unique(d$diag_kind))
	
	# init list for storing initial FALSE for each peiid / diag kind
	l <- vector(mode='list')
	
	# add unique peiid
	l[['peiid']] <- unique(d$peiid)
	
	# make a vector of FALSE, matching the length of unique peiid
	f <- rep(FALSE, times=length(l[['peiid']]))
	
	# iterate over diagnostic hz kind
	for(i in d.unique) {
		# fill this list element with FALSE
		l[[i]] <- f
		# lookup those peiid with this feature
		matching.peiid <- d$peiid[which(d$diag_kind == i)]
		# toggle FALSE-->TRUE for these pedons
		l[[i]][which(l[['peiid']] %in% matching.peiid)] <- TRUE
	}
	
	# convert to DF
	return(as.data.frame(l))
		
}


## TODO: this may need some review
## try and pick the best possible taxhistory record
.pickBestTaxHistory <- function(d) {
	
	# add a method field
	d$selection_method <- NA
	
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
	d$es_selection_method <- NA
	
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

## 2015-11-30: short-circuts could use some work, consider pre-marking mistakes in calling function
# attempt to format "landform" records into a single string
# note: there are several assumptions made about the data, 
# see "short-circuits" used when there are funky data
.formatLandformString <- function(i.gm, name.sep='|') {
  # get the current 
  u.peiid <- unique(i.gm$peiid)
    
  # sanity check: this function can only be applied to data from a single pedon
  if(length(u.peiid) > 1)
    stop('data are from multiple pedon records')
  
  # subset geomorph data to landforms
  i.gm <- i.gm[which(i.gm$geomftname == 'landform'), ]
  
  # allow for NA's
  if(nrow(i.gm) == 0)
    return(data.frame(peiid=u.peiid, landform.string=NA, stringsAsFactors=FALSE))
  
  # short-circuit: if any geomfeatid are NA, then we don't know the order
  # string together as-is, in row-order
  if(any(is.na(i.gm$geomfeatid))) {
    
    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      message(paste0('Using row-order. NA in geomfeatid:', u.peiid))
    
    ft.string <- paste(i.gm$geomfname, collapse=name.sep)
    return(data.frame(peiid=u.peiid, landform.string=ft.string, stringsAsFactors=FALSE))
  }
  
  # short-circuit: if any feature exists on itself, then use row-order
  # string together as-is, in row-order
  if(any(na.omit(c(i.gm$geomfeatid == i.gm$existsonfeat), FALSE))) {
    
    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      message(paste0('Using row-order. Error in exists-on logic:', u.peiid))
    
    ft.string <- paste(i.gm$geomfname, collapse=name.sep)
    return(data.frame(peiid=u.peiid, landform.string=ft.string, stringsAsFactors=FALSE))
  }
  
  # get an index to the top-most and bottom-most features
  # only 1 row should match these criteria
  top.feature <- which(! i.gm$geomfeatid %in% i.gm$existsonfeat)
  bottom.feature <- which(! i.gm$existsonfeat %in% i.gm$geomfeatid)
  
  # short-circut: incorrect data-population, usually duplicate entries with IDs that make no sense: use row-order
  if(length(top.feature) == 0 & length(bottom.feature) == 0) {
    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      warning(paste0('Using row-order. Error in exists-on logic: ', u.peiid), call.=FALSE)
    
    ft.string <- paste(i.gm$geomfname, collapse=name.sep)
    return(data.frame(peiid=u.peiid, landform.string=ft.string, stringsAsFactors=FALSE))
  }
   
  ## short-circuit: only 1 row, and exists-on logic is wrong, use row-order
  if(nrow(i.gm) == 1 & length(top.feature) == length(bottom.feature)) {
    
    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      warning(paste0('Using row-order. Single row / error in exists-on logic: ', u.peiid), call.=FALSE)
    
    ft.string <- paste(i.gm$geomfname, collapse=name.sep)
    return(data.frame(peiid=u.peiid, landform.string=ft.string, stringsAsFactors=FALSE))
  }
  
  # short-circuit: if the exists-on logic is wrong, use row-order
  if(length(top.feature) > 1 | length(bottom.feature) > 1) {
    
    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      warning(paste0('Using row-order. Incorrect exists-on specification: ', u.peiid), call.=FALSE)
    
    ft.string <- paste(i.gm$geomfname, collapse=name.sep)
    return(data.frame(peiid=u.peiid, landform.string=ft.string, stringsAsFactors=FALSE))
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
  return(data.frame(peiid=u.peiid, landform.string=ft.string, stringsAsFactors=FALSE))
}


# attempt to flatten site parent material data into 2 strings
.formatParentMaterialString <- function(i.pm, name.sep='|') {
  # get the current site
  u.siteiid <- unique(i.pm$siteiid)
  
  # sanity check: this function can only be applied to data from a single site
  if(length(u.siteiid) > 1)
    stop('data are from multiple site records')
  
  # subset sitepm data to remove any with NA for pm_kind
  i.pm <- i.pm[which(!is.na(i.pm$pm_kind)), ]
  
  # if there is no data, then return a DF formatted as if there were data
  if(nrow(i.pm) == 0)
    return(data.frame(siteiid=u.siteiid, pmkind=NA, pmorigin=NA, stringsAsFactors=FALSE))
  
  # short-circuit: if any pmorder are NA, then we don't know the order
  # string together as-is, in row-order
  if(any(is.na(i.pm$pmorder))) {
    # optional information on which sites have issues
    if(getOption('soilDB.verbose', default=FALSE))
      warning(paste0('Using row-order. NA in pmorder:', u.siteiid), call.=FALSE)
  }
  else{
    # there are no NAs in pmorder --> sort according to pmorder
    i.pm <- i.pm[order(i.pm$pmorder), ]
  }
  
  # composite strings and return
  str.kind <- paste(i.pm$pm_kind, collapse=name.sep)
  str.origin <- paste(unique(i.pm$pm_origin), collapse=name.sep)
  
  return(data.frame(siteiid=u.siteiid, pmkind=str.kind, pmorigin=str.origin, stringsAsFactors=FALSE))
}


# 2017-03-13: attempt to format COMPONENT "landform" records into a single string
# note: there are several assumptions made about the data, 
# see "short-circuits" used when there are funky data
.formatcoLandformString <- function(i.gm, name.sep='|') {
  
  # hacks to make R CMD check --as-cran happy:
  u.peiid <- NULL
  
  # get the current 
  u.coiid <- unique(i.gm$coiid)
  
  # sanity check: this function can only be applied to data from a single pedon
  if(length(u.coiid) > 1)
    stop('data are from multiple pedon records')
  
  # subset geomorph data to landforms
  i.gm <- i.gm[which(i.gm$geomftname == 'landform'), ]
  
  # allow for NA's
  if(nrow(i.gm) == 0)
    return(data.frame(coiid=u.coiid, landform.string=NA, stringsAsFactors=FALSE))
  
  # short-circuit: if any geomfeatid are NA, then we don't know the order
  # string together as-is, in row-order
  if(any(is.na(i.gm$geomfeatid))) {
    
    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      message(paste0('Using row-order. NA in geomfeatid:', u.peiid))
    
    ft.string <- paste(i.gm$geomfname, collapse=name.sep)
    return(data.frame(coiid=u.coiid, landform.string=ft.string, stringsAsFactors=FALSE))
  }
  
  # short-circuit: if any feature exists on itself, then use row-order
  # string together as-is, in row-order
  if(any(na.omit(c(i.gm$geomfeatid == i.gm$existsonfeat), FALSE))) {
    
    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      message(paste0('Using row-order. Error in exists-on logic:', u.coiid))
    
    ft.string <- paste(i.gm$geomfname, collapse=name.sep)
    return(data.frame(coiid=u.coiid, landform.string=ft.string, stringsAsFactors=FALSE))
  }
  
  # get an index to the top-most and bottom-most features
  # only 1 row should match these criteria
  top.feature <- which(! i.gm$geomfeatid %in% i.gm$existsonfeat)
  bottom.feature <- which(! i.gm$existsonfeat %in% i.gm$geomfeatid)
  
  ## short-circuit: only 1 row, and exists-on logic is wrong, use row-order
  if(nrow(i.gm) == 1 & length(top.feature) == length(bottom.feature)) {
    
    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      warning(paste0('Using row-order. Single row / error in exists-on logic:', u.coiid), call.=FALSE)
    
    ft.string <- paste(i.gm$geomfname, collapse=name.sep)
    return(data.frame(coiid=u.coiid, landform.string=ft.string, stringsAsFactors=FALSE))
  }
  
  # short-circuit: if the exists-on logic is wrong, use row-order
  if(length(top.feature) > 1 | length(bottom.feature) > 1) {
    
    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      warning(paste0('Using row-order. Incorrect exists-on specification:', u.coiid), call.=FALSE)
    
    ft.string <- paste(i.gm$geomfname, collapse=name.sep)
    return(data.frame(coiid=u.coiid, landform.string=ft.string, stringsAsFactors=FALSE))
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
  return(data.frame(coiid=u.coiid, landform.string=ft.string, stringsAsFactors=FALSE))
}


# attempt to flatten component parent material data into 2 strings
.formatcoParentMaterialString <- function(i.pm, name.sep='|') {
  # get the current site
  u.coiid <- unique(i.pm$coiid)
  
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

# fix column classes, for some reason all the data is getting imported as characters
.fix_class <- function(x) {
  if (class(x) == "character" & any(!is.na(as.numeric(x)))) {as.numeric(x)} else x
  }
