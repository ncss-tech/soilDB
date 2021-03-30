# horizon checking may be too strict



#' Fetch commonly used site/horizon data from a PedonPC v.5 database.
#'
#' Fetch commonly used site/horizon data from a version 5.x PedonPC database,
#' return as a SoilProfileCollection object.
#'
#' This function currently works only on Windows.
#'
#' @aliases fetchPedonPC getHzErrorsPedonPC
#' @param dsn The path to a PedonPC version 6.x database
#' @return a SoilProfileCollection class object
#' @note This function attempts to do most of the boilerplate work when
#' extracting site/horizon data from a PedonPC or local NASIS database. Pedons
#' that have errors in their horizonation are excluded from the returned
#' object, however, their IDs are printed on the console. See
#' \code{\link{getHzErrorsPedonPC}} for a simple approach to identifying pedons
#' with problematic horizonation. Records from the 'taxhistory' table are
#' selected based on 1) most recent record, or 2) record with the least amount
#' of missing data.
#' @author D. E. Beaudette and J. M. Skovlin
#' @seealso \code{\link{get_hz_data_from_pedon_db}}
#' @keywords manip
#' @export fetchPedonPC
fetchPedonPC <- function(dsn) {

  # not in parity with NASIS functions
  warning("Loading data from PedonPC will return slightly different data structures than fetchNASIS().", call. = FALSE)

	# load data in pieces
	site_data <- get_site_data_from_pedon_db(dsn)
	hz_data <- get_hz_data_from_pedon_db(dsn)
	color_data <- get_colors_from_pedon_db(dsn)
	extended_data <- get_extended_data_from_pedon_db(dsn)

	# join pieces
	# horizon + hz color: all horizons
	h <- join(hz_data, color_data, by='phiid', type='left')

	# convert colors... in the presence of missing color data
	if(nrow(h) > 0) {
	  h$soil_color <- NA
  	idx <- complete.cases(h$m_r)
  	h$soil_color[idx] <- with(h[idx, ], rgb(m_r, m_g, m_b)) # moist colors
  }
	# replace horizons with hz + fragment summary
	h <- join(h, extended_data$frag_summary, by='phiid', type='left')

	# fix some common problems
	# replace missing lower boundaries
	message('replacing missing lower horizon boundaries ...')
	missing.lower.depth.idx <- which(!is.na(h$hzdept) & is.na(h$hzdepb))
	h$hzdepb[missing.lower.depth.idx] <- h$hzdept[missing.lower.depth.idx] + 1

	# test for bad horizonation... flag, and remove
	cat('finding horizonation errors ...\n')
	h.test <- aqp::checkHzDepthLogic(h, c('hzdept', 'hzdepb'), idname = 'peiid', fast = TRUE)

	# which are the good (valid) ones?
	good.pedon.ids <- as.character(h.test$peiid[which(h.test$valid)])
	bad.pedon.ids <- as.character(h.test$pedon_id[which(!h.test$valid)])

	# keep the good ones
	h <- h[which(h$peiid %in% good.pedon.ids), ]
	
  if (nrow(h) == 0)
    stop("no horizon data in PedonPC database", call. = FALSE)
	
	# upgrade to SoilProfilecollection
	depths(h) <- peiid ~ hzdept + hzdepb

	## TODO: this is slow
	# move pedon_id into @site, this will be used to join full table of site data
	site(h) <- ~ pedon_id

	## TODO: this will fail in the presence of duplicates
	# add site data to object
	site_data$pedon_id <- NULL # remove 'pedon_id' column from site_data
	site(h) <- site_data # left-join via peiid

	# load diagnostic horizons into @diagnostic
	diagnostic_hz(h) <- extended_data$diagnostic

	# add diagnostic boolean data into @site
	site(h) <- extended_data$diagHzBoolean

	### TODO: consider moving this into the extended data function ###
	# load best-guess optimal records from taxhistory
	# method is added to the new field called 'selection_method'

	.BY <- NULL
	.SD <- NULL
	# .pickBestTaxHistory <- soilDB:::.pickBestTaxHistory
	# .pickBestEcosite <- soilDB:::.pickBestEcosite
	ed.tax <- data.table::as.data.table(extended_data$taxhistory)
	best.tax.data <- ed.tax[, .pickBestTaxHistory(.SD),
	                        by = list(peiid = ed.tax$peiid)]
	site(h) <- as.data.frame(best.tax.data)

  # join-in landform string
	ed.lf <- data.table::as.data.table(extended_data$geomorph)
	lf <- ed.lf[, .formatLandformString(.SD, uid = .BY$peiid, name.sep = '|'),
	            by = list(peiid = ed.lf$peiid)]

	if (ncol(lf) > 1)
	  site(h) <- as.data.frame(lf[,c("peiid","landform_string")])

  # set PedonPC/NASIS-specific horizon identifier
  hzidname(h) <- 'phiid'

	# 7. save and mention bad pedons
	assign('bad.pedon.ids', value=bad.pedon.ids, envir=soilDB.env)
	if(length(bad.pedon.ids) > 0)
		message("horizon errors detected, use `get('bad.pedon.ids', envir=soilDB.env)` for a list of pedon IDs")

	# done
	return(h)
}
