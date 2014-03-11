# updated to NASIS 6.3

# convenience function for loading most commonly used information from local NASIS database
fetchNASISLabData <- function() {
	
	# test connection
	if(! 'nasis_local' %in% names(odbcDataSources()))
			stop('Local NASIS ODBC connection has not been setup. Please see the `setup_ODBC_local_NASIS.pdf` document included with this package.')
	
	# 1. load data in pieces
	s <- get_labpedon_data_from_NASIS_db()
	h <- get_lablayer_data_from_NASIS_db()
	
	# convert layers from list to dataframe
	h <- as.data.frame(h)
		
	# fix some common problems
	# replace missing lower boundaries
	missing.lower.depth.idx <- which(!is.na(h$hzdept) & is.na(h$hzdepb))
  if(length(missing.lower.depth.idx) > 0) {
    message(paste('replacing missing lower horizon depths with top depth + 1cm ... [', length(missing.lower.depth.idx), ' horizons]', sep=''))
    h$hzdepb[missing.lower.depth.idx] <- h$hzdept[missing.lower.depth.idx] + 1
  }
	
	
	# test for bad horizonation... flag, and remove
	message('finding horizonation errors ...')
	h.test <- ddply(h, 'labpeiid', test_hz_logic, topcol='hzdept', bottomcol='hzdepb', strict=TRUE)
	
	# which are the good (valid) ones?
	good.ids <- as.character(h.test$peiid[which(h.test$hz_logic_pass)])
	bad.ids <- as.character(h.test$peiid[which(!h.test$hz_logic_pass)])
  bad.pedon.ids <- site_data$pedon_id[which(site_data$peiid %in% bad.ids)]
	
	# keep the good ones
	h <- h[which(h$peiid %in% good.ids), ]
	
	# upgrade to SoilProfilecollection
	depths(h) <- labpeiid ~ hzdept + hzdepb
	
	## TODO: this is slow
	# move pedon_id into @site
	site(h) <- ~ pedon_id
	
	## TODO: this will fail in the presence of duplicates
	# add site data to object
	site_data$pedon_id <- NULL # remove 'pedon_id' column from site_data
	site(h) <- site_data # left-join via peiid
	
	
	# add surface frag summary
	site(h) <- extended_data$surf_frag_summary
		
	# 7. save and mention bad pedons
	assign('bad.pedon.ids', value=bad.pedon.ids, envir=soilDB.env)
	if(length(bad.pedon.ids) > 0)
		message("horizon errors detected, use `get('bad.pedon.ids', envir=soilDB.env)` for a list of pedon IDs")
	
	# done
	return(h)
}
