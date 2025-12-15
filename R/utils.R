##
## misc functions used by soilDB
##

## simplfied base R implementation of glue::glue()
# x: character vector with simple braced expressions to replace (NOT evaluate)
# env: environment where expression values are defined (default: `parent.frame()`)
.gluelite <- function(x, env = parent.frame()) {
  as.character(unlist(sapply(x, function(y) {
    vars <- regmatches(y, gregexpr("\\{[^{}]+\\}", y))[[1]]
    uvars <- unique(vars) 
    vals <- lapply(uvars, function(var) unique(get(gsub("[{}]", "", var), env, inherits = TRUE))) 
    unique(apply(expand.grid(vals, stringsAsFactors = FALSE), 1, function(z) 
      Reduce(function(y, var) sub(var, z[match(var, uvars)], y, fixed = TRUE), vars, y)))
  }, simplify = FALSE)))
}

#' Generate SDA SQL Comment Header
#'
#' This function generates a standardized SQL comment header string to be
#' prepended to SDA queries.
#'
#' @param function_name Character. The name of the high-level `soilDB` function
#'   generating the query.
#' @param package_version Character. The current version of the `soilDB`
#'   package.
#'
#' @return A character string containing the SQL comment header.
#' @noRd
#' @keywords internal
#'
#' @examples
#' generate_SDA_comment_header("soilDB::get_SDA_property")
.SDA_comment_header <- function(function_name,
                                package_version = as.character(packageVersion('soilDB'))) {
  
  if (!is.character(function_name) ||
      length(function_name) != 1 ||
      nchar(function_name) == 0) {
    stop("`function_name` must be a non-empty character string.")
  }
  
  if (!is.character(package_version) ||
      length(package_version) != 1 ||
      nchar(package_version) == 0) {
    stop("`package_version` must be a non-empty character string.")
  }
  
  # Construct the SQL comment header
  sprintf(
    "/** SDA Query application='soilDB' rule='%s' version='%s' **/",
    function_name,
    package_version
  )
}

# check if terra namespace can be loaded and used to initialize objects
.terra_can_initialize_spatial_object <- function() {
  requireNamespace("terra", quietly = TRUE) && tryCatch({
    terra::rast(matrix(0), crs = "OGC:CRS84")
    terra::vect("POINT (0 0)", crs = "OGC:CRS84")
    TRUE
  }, warning = FALSE, error = FALSE)
}

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

## try and pick the best possible taxhistory record
.pickBestTaxHistory <- function(d) {

	# add a method field (a character)
	d$selection_method <- NA_character_

	# short-circuit: 1 row
	if(nrow(d) < 2) {
	  d$selection_method <- 'single record'
	  return(d)
	}

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

## try and pick the best possible ecosite record
.pickBestEcosite <- function(d, es_classifier = NULL) {

  if (!is.null(es_classifier)) {
    d <- d[which(d$siteecositehistory.classifier %in% es_classifier), ]
  }
  
	# add a method field
	d$es_selection_method <- NA_character_

	# try to get the most recent:
	d.order <- order(d$ecositecorrdate, decreasing = TRUE)
	
	# if there are multiple (unique) dates, return the most recent
	if (length(unique(d$ecositecorrdate)) > 1) {
		d$es_selection_method <- 'most recent'
		d$recwlupdated <- NULL
		return(d[d.order[1], ])
	}
	
	# sort order is not stable when no correlation dates are populated (use record date)
	if (all(is.na(d$ecositecorrdate))) {
	  d.order <- order(d$recwlupdated, decreasing=TRUE)
	  d <- d[d.order,]
	}

	# otherwise, return the record with the least number of missing cells
	# if there are the same number of missing cells, the first record is returned
	n.na <- apply(d, 1, function(i) length(which(is.na(i))))
	best.record <- which.min(n.na)

	d$es_selection_method <- 'least missing data'
	d$recwlupdated <- NULL
	return(d[best.record, ])
}

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
                      geomicrorelief_string = NA_character_,
                      stringsAsFactors = FALSE))
  }

  # landscape
  landsc.string <- paste0(unique(trimws(paste(i.ls$geomfmod[!is.na(i.ls$geomfmod)], i.ls$geomfname))), collapse = name.sep)

  # microfeature
  mf.string <- paste0(unique(trimws(paste(i.mf$geomfmod[!is.na(i.mf$geomfmod)], i.mf$geomfname[!is.na(i.mf$geomfname)]))), collapse = name.sep)

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
  if(anyNA(i.gm$geomfeatid)) {

    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      message(paste0('Using row-order. NA in geomfeatid:', soiliid))

    ft.string <- paste(unique(trimws(paste(i.gm$geomfmod[!is.na(i.gm$geomfmod)], i.gm$geomfname))), collapse = name.sep)
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
      geomicrorelief_string = surfmr.string,
      stringsAsFactors = FALSE
    ))
  }

  # get an index to the top-most and bottom-most features
  # only 1 row should match these criteria
  top.feature <- which(!i.gm$geomfeatid %in% i.gm$existsonfeat)
  bottom.feature <- which(!i.gm$existsonfeat %in% i.gm$geomfeatid)

  # short-circut: incorrect data-population, usually duplicate entries with IDs that make no sense: use row-order
  if(length(top.feature) == 0 & length(bottom.feature) == 0) {
    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      warning('Using row-order. Error in exists-on logic: ', soiliid)

    ft.string <- paste(unique(i.gm$geomfname), collapse=name.sep)
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

  ## short-circuit: only 1 row, and exists-on logic is wrong, use row-order
  if(nrow(i.gm) == 1 & length(top.feature) == length(bottom.feature)) {

    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      warning('Using row-order. Single row / error in exists-on logic: ', soiliid)

    ft.string <- paste(unique(i.gm$geomfname), collapse=name.sep)
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

  # short-circuit: if the exists-on logic is wrong, use row-order
  if(length(top.feature) > 1 | length(bottom.feature) > 1) {

    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      warning('Using row-order. Incorrect exists-on specification: ', soiliid)

    ft.string <- paste(unique(i.gm$geomfname), collapse=name.sep)
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
    geomicrorelief_string = surfmr.string,
    stringsAsFactors = FALSE
  ))
}


## https://github.com/ncss-tech/soilDB/issues/84
# attempt to flatten site parent material data into 2 strings
.formatParentMaterialString <- function(i.pm, uid = NULL, name.sep = '|') {

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
  if (anyNA(i.pm$pmorder)) {
    # optional information on which sites have issues
    if(getOption('soilDB.verbose', default = FALSE))
      warning('Using row-order. NA in pmorder:', u.siteiid)
  } else {
    # there are no NAs in pmorder --> sort according to pmorder
    i.pm <- i.pm[order(i.pm$pmorder), ]
  }

  # composite strings and return
  str.kind <- paste(i.pm$pmkind, collapse = name.sep)
  str.origin <- paste(unique(i.pm$pmorigin), collapse = name.sep)

  return(data.frame(siteiid = u.siteiid, pmkind = str.kind, pmorigin = str.origin, stringsAsFactors = FALSE))
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
  if(anyNA(i.gm$geomfeatid)) {

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
      warning('Using row-order. Single row / error in exists-on logic: ', u.coiid)

    ft.string <- paste(unique(i.gm$geomfname), collapse=name.sep)
    return(data.frame(coiid=u.coiid, landform_string=ft.string, stringsAsFactors=FALSE))
  }

  # short-circuit: if the exists-on logic is wrong, use row-order
  if(length(top.feature) > 1 | length(bottom.feature) > 1) {

    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      warning('Using row-order. Incorrect exists-on specification: ', u.coiid)

    ft.string <- paste(unique(i.gm$geomfname), collapse=name.sep)
    return(data.frame(coiid=u.coiid, landform_string=ft.string, stringsAsFactors=FALSE))
  }

  # short circuit: if there is circularity in the exists-on logic, use row-order
  # example coiid: 2119838
  if(length(top.feature) < 1 | length(bottom.feature) < 1) {

    # optional information on which pedons have issues
    if(getOption('soilDB.verbose', default=FALSE))
      warning('Using row-order. Incorrect exists-on specification: ', u.coiid)

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
  if(anyNA(i.pm$pmorder)) {
    # optional information on which sites have issues
    if(getOption('soilDB.verbose', default=FALSE))
      warning('Using row-order. NA in pmorder:', u.coiid)
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
    return(data.frame(coiid = NA_integer_, ecositeid = NA, ecositenm = NA, stringsAsFactors = FALSE)[0, ])

  # sanity check: this function can only be applied to data from a single component
  if (length(u.coiid) > 1)
    stop('data are from multiple component records')

  # subset othervegcl data to remove any with NA for othervegclass
  i.esd <- i.esd[which(!is.na(i.esd$ecositeid)), ]

  # if there is no data, then return a DF formatted as if there were data
  if (nrow(i.esd) == 0)
    return(data.frame(coiid = u.coiid, ecositeid = NA, ecositenm = NA, stringsAsFactors = FALSE))

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

  return(data.frame(coiid=u.coiid, ecositeid=str.ecoid, ecositenm=str.econm, stringsAsFactors=FALSE))
}

# attempt to flatten multiple other veg class entries into 1 string
.formatOtherVegString <- function(i.ov, name.sep='|', id.name = "coiid") {
  # get the current site
  u.iid <- unique(i.ov[[id.name]])
  n.iid <- length(u.iid)
  emptydf <- data.frame(coiid = u.iid, 
                        ovegclid = NA[seq_len(n.iid)], 
                        ovegclname = NA[seq_len(n.iid)], 
                        stringsAsFactors = FALSE)
  colnames(emptydf)[1] <- id.name
  
  if (n.iid == 0) 
    return(emptydf)

  # sanity check: this function can only be applied to data from a single component
  if (n.iid > 1)
    stop('data are from multiple parent records, this function can only be applied to data from a single component')

  # subset othervegcl data to remove any with NA for othervegclass
  i.ov <- i.ov[which(!is.na(i.ov$ovegclid)), ]

  # if there is no data, then return a DF formatted as if there were data
  if (nrow(i.ov) == 0)
    return(emptydf)
  
  # composite strings and return
  str.ovegclid <- paste(i.ov$ovegclid, collapse = name.sep)
  str.ovegclname <- paste(unique(i.ov$ovegclname), collapse = name.sep)
  
  res <- data.frame(
    coiid = u.iid,
    ovegclid = str.ovegclid,
    ovegclname = str.ovegclname,
    stringsAsFactors = FALSE
  )
  colnames(res)[1] <- id.name
  return(res)
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
.cosoilmoist_prep <- function(df, impute) {

  # cache original column names
  orig_names <- names(df)

  # relabel names
  # names(df) <- gsub("^soimoist", "", names(df))
  # old_names <- "stat"
  # new_names <- "status"
  # names(df)[names(df) %in% old_names] <- new_names

  # setting frequency levels and order
  
  # NOTE: the next block of code require factor levels be set, regardless of package options
  # NOTE: the SDA domains for flooding and ponding have different levels, and "Common" is obsolete

  # .:. ordering implied in get_NASIS_column_metadata result from NASIS ChoiceSequence 
  flod_lev <- factor(df$flodfreqcl, levels = get_NASIS_column_metadata("flodfreqcl")$ChoiceLabel)
  pond_lev <- factor(df$pondfreqcl, levels = get_NASIS_column_metadata("pondfreqcl")$ChoiceLabel)
  mois_lev <- factor(df$soimoiststat, levels = get_NASIS_column_metadata("soimoiststat")$ChoiceLabel)
  
  # impute NA freqcl values, default = "not populated"
  if (impute) {

    missing <- "Not populated"
    lev_flodfreqcl <- c(missing, levels(flod_lev))
    lev_pondfreqcl <- c(missing, levels(pond_lev))
    lev_status <- c(missing, levels(mois_lev))

    # replace NULL RV depths with 201 cm if pondfreqcl or flodqcl is not NULL
    df$soimoistdept_r[is.na(df$soimoistdept_r) & (!is.na(df$pondfreqcl) | !is.na(df$flodfreqcl))] <- 201
    df$soimoistdepb_r[is.na(df$soimoistdepb_r) & (!is.na(df$pondfreqcl) | !is.na(df$flodfreqcl))] <- 201

    # replace NULL L and H depths with the RV
    df$soimoistdept_l <- ifelse(is.na(df$soimoistdept_l), df$soimoistdept_r, df$soimoistdept_l)
    df$soimoistdept_h <- ifelse(is.na(df$soimoistdept_h), df$soimoistdept_r, df$soimoistdept_h)
    df$soimoistdepb_l <- ifelse(is.na(df$soimoistdepb_l), df$soimoistdepb_r, df$soimoistdepb_l)
    df$soimoistdepb_h <- ifelse(is.na(df$soimoistdepb_h), df$soimoistdepb_r, df$soimoistdepb_h)

    # relevel factors with "Not populated" as first level
    df$soimoiststat <- factor(as.character(df$soimoiststat), levels = lev_status)
    df$flodfreqcl <- factor(as.character(df$flodfreqcl), levels = lev_flodfreqcl)
    df$pondfreqcl <- factor(as.character(df$pondfreqcl), levels = lev_flodfreqcl)

    # replace NULL moist state and frequency class with "Not populated"
    df$soimoiststat[is.na(df$soimoiststat)] <- missing
    df$flodfreqcl[is.na(df$flodfreqcl)] <- missing
    df$pondfreqcl[is.na(df$pondfreqcl)] <- missing
  }

  # convert factors to strings
  if (!NASISDomainsAsFactor()) {
    idx <- unlist(lapply(df, is.factor))
    if (any(idx)) {
      df[idx] <- lapply(df[idx], as.character)
    }
  }

  # note get_cosoilmoist_f
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
      pmorigin <- NA; pmkind <- NA;

      dups_clean <- {
        .<- transform(dups,
                  idx_pmo = !is.na(pmorigin),
                  idx_pmk = !is.na(pmkind)
                  );
        .<- split(., .$cokey, drop = TRUE)
        .<- lapply(., function(x) { data.frame(
          x[1, c("cokey", "pmgroupname")],
          pmkind   = paste(x[x$idx_pmk, "pmkind"  ][order(x[x$idx_pmk, "pmorder"])],   collapse = " over "),
          pmorigin = paste(x[x$idx_pmo, "pmorigin"][order(x[x$idx_pmo, "pmorder"])], collapse = " over "),
          stringsAsFactors = FALSE
          )})
        .<- do.call("rbind", .);
      }
      nodups <- nodups[! names(df) %in% c("copmgrpkey", "pmorder")]

      df <- rbind(nodups, dups_clean)
      df <- df[order(df$cokey), ]
      row.names(df) <- seq_len(nrow(df))
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
    mntn <- NULL; hill <- NULL; trce <- NULL; flats <- NULL; hillslopeprof <- NULL;

    df <- within(df, {
      if (is(mntn, "character")) {
        mntn <- sapply(strsplit(mntn, ", "),  function(x) paste(sort(unlist(x)), collapse = ", "))
        }
      if (is(hill, "character")) {
        hill <- sapply(strsplit(hill, ", "),  function(x) paste(sort(unlist(x)), collapse = ", "))
        }
      if (is(trce, "character")) {
        trce <- sapply(strsplit(trce, ", "),  function(x) paste(sort(unlist(x)), collapse = ", "))
        }
      if (is(flats, "character")) {
        flats <- sapply(strsplit(flats, ", "), function(x) paste(sort(unlist(x)), collapse = ", "))
        }
      if (is(hillslopeprof, "character")) {
        hillslopeprof <- sapply(strsplit(hillslopeprof, ", "), function(x) paste(sort(unlist(x)), collapse = ", "))
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
        . <- split(dups, dups$cokey, drop = TRUE)
        . <- lapply(., function(x) {
          data.frame(
            cokey = x$cokey[1],
            landscape     = paste(unique(x$landscape), collapse = " and "),
            landform      = paste(unique(x$landform), collapse = " on  "),
            mntn          = paste(sort(unique(x$mntn)), collapse = ", "),
            hill          = paste(sort(unique(x$hill)), collapse = ", "),
            trce          = paste(sort(unique(x$trce)), collapse = ", "),
            flats         = paste(sort(unique(x$flats)), collapse = ", "),
            shapeacross   = paste(sort(unique(x$shapeacross)), collapse = ", "),
            shapedown     = paste(sort(unique(x$shapedown)), collapse = ", "),
            hillslopeprof = paste(sort(unique(
              x$hillslopeprof
            )), collapse = ", ")
            # stringsAsFactors = TRUE # 2025-12-05: confirmed this has no effect on result
          )
        })
        . <- do.call("rbind", .)
      }
      nodups <- nodups[! names(nodups) %in% c("geomfeatid", "existsonfeat")]
      
      # NOTE: this rbind wipes out factors
      df <- rbind(nodups, dups_clean)
      df <- df[order(df$cokey), ]
      row.names(df) <- seq_len(nrow(df))
      } else df <- df[! names(df) %in% c("geomfeatid", "existsonfeat")]
    }

  vars <- c("landscape", "landform", "mntn", "hill", "trce", "flats", "hillslopeprof")
  idx <- unlist(lapply(df, is.character))
  idx <- names(df) %in% vars & idx
  df[, idx] <- lapply(df[, idx], function(x) ifelse(x %in% c("", "NA"), NA, x))

  mntn <- NA; hill <- NA; trce <- NA; flats <- NA; shapeacross <- NA; shapedown <- NA;

  ## construct factor levels
  
  # combine geompos and shapes
  if (nrow(df) > 0) {
    df <- within(df, {
      geompos <- NA
      geompos <- paste(mntn, hill, trce, flats, sep = ", ")
      geompos <- gsub("NA", "", geompos)
      geompos <- gsub("^, |^, , |^, , , |, $|, , $|, , , $", "", geompos)
      geompos <- gsub(", , ", ", ", geompos)
      geompos[geompos == ""] = NA

      ssa <- NA # slope shape across
      ssd <- NA # slope shape down
      slopeshape <- NA

      ssa <- gsub("Concave", "C", shapeacross)
      ssa <- gsub("Linear",  "L", ssa)
      ssa <- gsub("Convex",  "V", ssa)

      ssd <- gsub("Concave", "C", shapedown)
      ssd <- gsub("Linear",  "L", ssd)
      ssd <- gsub("Convex",  "V", ssd)

      slopeshape <- paste0(ssd, ssa, sep = "")
      slopeshape[slopeshape %in% c("NANA", "")] = NA
      })
    df[c("ssa", "ssd")] <- NULL
  } else {
    df <- cbind(df, geompos = as.character(NULL))
  }
  
  # custom combined slope shape variable (no corresponding single domain in NASIS)
  ss_vars <- c("CC", "CV", "CL", "LC", "LL", "LV", "VL", "VC", "VV")
  if (all(df$slopeshape[!is.na(df$slopeshape)] %in% ss_vars)) {
    df$slopeshape <- factor(df$slopeshape, levels = ss_vars)
    df$slopeshape <- droplevels(df$slopeshape)
  }

  hs_vars <- levels(NASISChoiceList(data.frame(hillslopeprof = ""), choice = "ChoiceLabel"))
  if (all(df$hillslopeprof[!is.na(df$hillslopeprof)] %in% hs_vars)) {
    df$hillslopeprof <- factor(df$hillslopeprof, levels = hs_vars)
    df$hillslopeprof <- droplevels(df$hillslopeprof)
  }

  hill_vars <- levels(NASISChoiceList(data.frame(geomposhill = ""), choice = "ChoiceLabel"))
  if (all(df$hill[!is.na(df$hill)] %in% hill_vars)) {
    df$hill <- factor(df$hill, levels = hill_vars)
    df$hill <- droplevels(df$hill)
  }

  flats_vars <- levels(NASISChoiceList(data.frame(geomposflats = ""), choice = "ChoiceLabel"))
  if (all(df$flats[!is.na(df$flats)] %in% flats_vars)) {
    df$flats <- factor(df$flats, levels = flats_vars)
    df$flats <- droplevels(df$flats)
  }

  trce_vars <- levels(NASISChoiceList(data.frame(geompostrce = ""), choice = "ChoiceLabel"))
  if (all(df$trce[!is.na(df$trce)] %in% trce_vars)) {
    df$trce <- factor(df$trce, levels = trce_vars)
    df$trce <- droplevels(df$trce)
  }
  
  return(df)
}


.copm_prep2 <- function(x, key = NULL) {
  
  idx_key   <- grep(key, names(x))
  idx_pmkey <- grep("pmgrpkey", names(x))
  nm_pmkey  <- names(x)[idx_pmkey]
  names(x)[c(idx_key, idx_pmkey)] <- c("key", "pmgrpkey")
  
  vars <- c("key", "pmgrpkey", "pmorder", "pmkind")
  
  # pmk1 <- pmk
  # pmk1 <- pmk1[with(pmk1, order(key, pmgrpkey, pmorder)), ]
  # pmk1 <- aggregate(pmkind ~ key, data = pmk1, FUN = function(x) paste0(x, collapse = " over "))
  # 
  # test <- {
  #   strsplit(pmk1$pmkind, " over ") ->.;
  #   lapply(., function(x) {
  #     x[i = cumsum(rle(x)$lengths)] ->.;
  #     paste(., collapse = " over ")
  #   }) ->.;
  #   unlist(.)
  # }
  # pmk1$pmkind <- test
  
  
  pm    <- data.table::as.data.table(x); rm(x)
  pm    <- pm[order(pm$key, pm$pmgrpkey, pm$pmorder)]
  
  pm$id_k <- paste(pm$key, pm$pmkind)
  pm$id_o <- paste(pm$key, pm$pmorigin)
  
  
  # pmkind
  # remove duplicate pmkind by cokey
  pm_k <- {
    vars <- c("key", "pmkind", "id_k")
    # ..vars = NULL
    pm_k <- pm[!is.na(pm$pmkind), vars, with = FALSE]
    idx <- cumsum(rle(pm_k$id_k)$lengths)
    pm_k[idx, ]
  }
  . <- NULL
  pmkind <- NULL
  pm_k <- pm_k[, .(pmkind   = paste0(pmkind,   collapse = " over ")), by = .(key)]
  
  
  # pmorigin
  pmorigin <- NULL
  pm_o <- {
    vars <- c("key", "pmorigin", "id_o")
    # ..vars = NULL
    pm_o <- pm[!is.na(pm$pmorigin), vars, with = FALSE]
    idx  <- cumsum(rle(pm_o$id_o)$lengths)
    pm_o[idx, ]
  }
  pm_o <- pm_o[, .(pmorigin = paste0(pmorigin, collapse = " over ")), by = .(key)]
  
  
  # merge
  pm <- as.data.frame(merge(pm_k, pm_o, by = "key", all = TRUE, sort = FALSE))
  
  names(pm)[1] <- c(key) 
  
  return(pm)
}



.cogmd_prep2 <- function(data, key = "cokey") {
  
  idx_key   <- grep(key, names(data))
  names(data)[c(idx_key)] <- c("key")
  
  
  # find sites with overlapping landforms ----
  n_bot <- NULL
  n_mis_geomfeatid <- NULL
  geomfeatid <- NULL
  .N <- NULL
  N <- NULL
  
  test  <- data.table::as.data.table(data)[
    , .(
      # n_bot = sum(! existsonfeat %in% geomfeatid, na.rm = TRUE),
      n_bot = sum(! match(existsonfeat, geomfeatid, nomatch = 0, incomparables = NA_integer_) > 0, na.rm = TRUE),
      n_geomfeatid  = sum(!is.na(geomfeatid)),
      n_existonfeat = sum(!is.na(existsonfeat)),
      .N,
      n_mis_geomfeatid = sum(is.na(geomfeatid))
    ), 
    by = key
  ]
  data <- merge(test, data, by = "key", all.y = TRUE)
  
  
  # determine row direction ----
  # ordered
  data <- within(data, {
    existsonfeat <- ifelse(geomfeatid == existsonfeat, NA, existsonfeat)
    existsonfeat <- ifelse(n_bot == N,                 NA, existsonfeat)
    
    row_dir <- ifelse(geomfeatid <  existsonfeat, "top2bot", "bot2top")
    row_dir <- ifelse(
      geomfeatid == existsonfeat + 1 | geomfeatid == existsonfeat - 1,
      row_dir,
      "chaos"
    )
    row_dir <- ifelse(n_bot == N | is.na(existsonfeat), "missing", row_dir)
    row_dir <- factor(row_dir, levels = c("top2bot", "bot2top", "chaos", "missing"))
  })
  
  
  # find chaos within a component ----
  tb <- as.data.frame.matrix(with(data, table(key, row_dir))) 
  chaos <- cbind(within(tb, {
      tot <- rowSums(cbind(top2bot > 0, bot2top > 0, chaos > 0))
      co_dir <- ifelse(tot > 1, "chaos", "ordered")
    }), key = row.names(tb))
  data <- merge(data, chaos, by = "key", all.x = TRUE, sort = FALSE)
  data <- within(data, {
    co_dir <- ifelse(N == n_bot | N == n_mis_geomfeatid | N == missing, "missing", co_dir)
  })
  
  
  # replace NA row direction, where the component direction == "ordered" ----
  vars <- c("top2bot", "bot2top", "chaos")
  # ..vars = NULL
  idx  <- which(data$row_dir == "missing" & data$co_dir == "ordered")
  ordered_mis  <- names(data[, vars, with = FALSE])[max.col(data[idx, vars, with = FALSE])]
  data[idx, "row_dir"] <- ordered_mis
  
  # subset(data, key == "22230267")
  
  
  # subset and sort different ordering conventions ----
  # top2bot & NA
  top2bot <- {
    .<- subset(data, row_dir  == "top2bot" & co_dir == "ordered");
    .[order(.$key,   .$geomfeatid,   .$existsonfeat), ]
  }
  # bot2top
  bot2top <- {
    .<- subset(data, row_dir == "bot2top" & co_dir == "ordered");
    .[order(.$key, - .$geomfeatid, - .$existsonfeat), ]
  }
  # chaos and missing ordered
  chaos2 <- {
    .<- subset(data, co_dir %in% c("chaos", "missing") | row_dir == "chaos");
    .[order(.$key,   .$geomfeatid,   .$existsonfeat), ]
  }
  
  
  # recombine
  data <- rbind(top2bot, bot2top, chaos2)
  rm(top2bot, bot2top, chaos2)
  
  
  # find N tops ----
  # test  <- data.table::as.data.table(data)[
  #   , .(
  #     # n_bot = sum(! existsonfeat %in% geomfeatid, na.rm = TRUE),
  #     n_bot = sum(! match(existsonfeat, geomfeatid, nomatch = 0, incomparables = NA_integer_) > 0, na.rm = TRUE),
  #     
  #     .N,
  #     n_mis_geomfeatid = sum(is.na(geomfeatid))
  #   ), 
  #   by = key
  # ]
  # data <- merge(test, data, by = "key", all.y = TRUE)
  
  
  # flatten duplicated ids ----
  # create unique key
  data$key2 <- with(data, paste(key, geomfeatid, existsonfeat))
  
  ## find duplicates ----
  idx <- which(duplicated(data$key2))
  
  if (length(idx) > 0) {
    tb  <- table(data$key2)
    dups <- which(data$key2 %in% names(tb)[tb > 1])
    
    if (length(dups) > 0) {
    nodups <- {
      len <- {.<- rle(data$key2[dups]); .$lengths}
      len <- cumsum(len) - len + 1
    }
    nodups <- dups[nodups]
    } else nodups <- seq_len(nrow(data))
    
    # subset duplicates
    vars <- c("key2", "landform", "mntn", "hill","trce", "flats", "shapeacross", "shapedown", "slopeshape", "hillslopeprof")
    # ..vars = NULL
    data_sub <- data[dups, vars, with = FALSE]
  
    # flatten duplicates
    data_sub <- data.table::as.data.table(.flatten_gmd(as.data.frame(data_sub), key = "key2"))
    
    # replace duplicates
    data[nodups, vars] <- data_sub
    
    # remove duplicates
    data <- data[-idx, ]
  }
  data$key2 <- NULL
  
  # subset different conventions ----
  data_comb <- subset(data, n_bot > 1 & co_dir != "missing")
  data_mis  <- subset(data, n_bot > 1 & co_dir == "missing")
  data_simp <- subset(data, n_bot < 2)
  
  
  vars <- c("key", "landform", "mntn", "hill","trce", "flats", "shapeacross", "shapedown", "slopeshape", "hillslopeprof")
  # ..vars = NULL
  data_mis <- data.table::as.data.table(.flatten_gmd(as.data.frame(data_mis[, vars, with = FALSE]), sep = " and "))
  data_simp <- data.table::as.data.table(.flatten_gmd(as.data.frame(data_simp[, vars, with = FALSE]), sep = " on "))
  
  
  # iterate over sites with unsorted overlapping landforms ----
  if (nrow(data_comb) > 0) {
  data_comb_l <- split(data_comb, data_comb$key)
  data_comb_l <- lapply(data_comb_l, function(x) {
    
    # replace landscape existsonfeat with NA
    x$existsonfeat <- sapply(x$existsonfeat, function(y) {
      ifelse(any(y == x$geomfeatid), y, NA)
    })
    
    
    # find bottom landform
    bot <- subset(x, is.na(existsonfeat))
    
    
    # iterate over bottoms
    len <- nrow(x)
    sep <- ifelse(x$co_dir == "ordered", " on ", " and ")
    
    bot <- split(bot, bot$geomfeatid)
    x_sorted <- lapply(bot, function(y) {
      rank <- integer(len)
      rank[1] <- y$geomfeatid[1]
      for (i in 1:len) {
        idx <- x$geomfeatid[which(x$existsonfeat == rank[i])]
        if (length(idx) > 0) rank[i + 1] = idx
      }
      rank <- rank[which(rank > 0)]
      rank <- which(x$geomfeatid %in% rank)
      vars <- c("key", "landform", "mntn", "hill","trce", "flats", "shapeacross", "shapedown", "slopeshape", "hillslopeprof")
      # ..vars = NULL
      x2 <- x[rank, vars, with = FALSE]
      suppressMessages(y <- .flatten_gmd(x2, sep = sep, SORT = FALSE)) 
      return(y)
    })
    x_sorted <- do.call("rbind", x_sorted)
    
    return(x_sorted)
  })
  data_comb_l3 <- do.call("rbind", data_comb_l)
  data_comb <- data.table::as.data.table(.flatten_gmd(as.data.frame(data_comb_l3), key = "key") )
  } else data_comb <- data_comb[, vars, with = FALSE]
  
  data <- as.data.frame(rbind(data_simp, data_mis, data_comb))
  names(data)[names(data) == "key"] <- key
  
  
  # # uncode
  # data("metadata", package = "soilDB")
  # vars <- c("mntn", "hill", "trce", "flats")
  # idx <- names(data) %in% vars
  # names(data)[idx] <- paste0("geompos", vars)
  # idx <- names(data) %in% metadata$ColumnPhysicalName
  # data
  
  
  return(data)
}


# vars <- c("geomfeatid", "existsonfeat")
# idx <- unlist(sapply(1:nrow(test), function(i) {
#    unname(unlist(test[i, vars, drop = TRUE]))
#   },
#   simplify = FALSE
# ))
# idx <- idx[!duplicated(idx) & !is.na(idx)]


.format_slopeshape <- function(dat) {
  
  shapeacross <- NA
  shapedown <- NA
  
  dat <- within(dat, {
    ssa <- NA # slope shape across
    ssd <- NA # slope shape down
    slopeshape <- NA
    
    ssa <- gsub("Concave", "C", shapeacross)
    ssa <- gsub("Linear",  "L", ssa)
    ssa <- gsub("Convex",  "V", ssa)
    
    ssd <- gsub("Concave", "C", shapedown)
    ssd <- gsub("Linear",  "L", ssd)
    ssd <- gsub("Convex",  "V", ssd)
    
    slopeshape <- paste0(ssd, ssa, sep = "")
    slopeshape[slopeshape %in% c("NANA", "")] = NA
  })
  dat[c("ssa", "ssd")] <- NULL
  
  # ss_vars <- c("CC", "CV", "CL", "LC", "LL", "LV", "VL", "VC", "VV")
  # if (all(dat$slopeshape[!is.na(dat$slopeshape)] %in% ss_vars)) {
  #   dat$slopeshape <- factor(dat$slopeshape, levels = ss_vars)
  #   dat$slopeshape <- droplevels(dat$slopeshape)
  # }
  return(dat)
}


.flatten_gmd <- function(data, key = "key", table = NULL, sep = " and ", SORT = TRUE) {
  
  idx_key   <- grep(key, names(data))
  if (length(idx_key) != 1L) stop("the key/id argument does not match any of the column names in the data.frame")
  names(data)[c(idx_key)] <- c("key")
  
  
  tb <- table(data$key)
  idx <- names(tb[tb > 1])
  idx <- data$key %in% idx
  
  test <- sum(idx, na.rm = TRUE)
  if (test > 0) {
    message(test, " ", key, " values were found in the ", table, " table that contain multiple entries, the resulting values will be flattened/combined into 1 record per ", key, " and separated with 'and'")
    
    data_sub <- data.table::as.data.table(data[idx, ])
    .SD <- NULL
    data_sub <- as.data.frame(data_sub[
      ,
      lapply(.SD, function(x) {
        if (SORT) {paste0(sort(unique(x[!is.na(x)])), collapse = sep)
        } else    {paste0(     unique(x[!is.na(x)]),  collapse = sep)}
      }),
      by = key
    ])
    
    data <- rbind(data[!idx, ], data_sub)
    
  }
  
  # replace "" values with NA
  idx <- seq_len(ncol(data))
  data[idx] <- lapply(data, function(x) ifelse(x == "", NA, x))
  
  
  names(data)[idx_key] <- key
  
  return(data)
}


