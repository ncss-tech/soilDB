# updated to NASIS 6.3

# convenience function for loading most commonly used information from local NASIS database
fetchNASISLabData <- function() {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
	# test connection
	if(! 'nasis_local' %in% names(RODBC::odbcDataSources()))
			stop('Local NASIS ODBC connection has not been setup. Please see the `setup_ODBC_local_NASIS.pdf` document included with this package.')
	
	# 1. load data in pieces, results are DF objects
	s <- get_labpedon_data_from_NASIS_db()
	h <- get_lablayer_data_from_NASIS_db()
	
  # test to see if the selected set is loaded
  if (nrow(h) == 0 | nrow(s) == 0) message('your selected set is missing either the lab or site table, please load and try again :)')
		
	# fix some common problems
	# replace missing lower boundaries
	missing.lower.depth.idx <- which(!is.na(h$hzdept) & is.na(h$hzdepb))
  if(length(missing.lower.depth.idx) > 0) {
    message(paste('replacing missing lower horizon depths with top depth + 1cm ... [', length(missing.lower.depth.idx), ' horizons]', sep=''))
    h$hzdepb[missing.lower.depth.idx] <- h$hzdept[missing.lower.depth.idx] + 1
  }
	
	
  ## TODO: what to do with multiple samples / hz?
	# test for bad horizonation... flag
	message('finding horizonation errors ...')
	h.test <- ddply(h, 'labpeiid', test_hz_logic, topcol='hzdept', bottomcol='hzdepb', strict=TRUE)
	
	# which are the good (valid) ones?
	good.ids <- as.character(h.test$labpeiid[which(h.test$hz_logic_pass)])
	bad.ids <- as.character(h.test$labpeiid[which(!h.test$hz_logic_pass)])
  bad.pedon.ids <- s$upedonid[which(s$labpeiid %in% bad.ids)]
	
  # subset ?
  
	
	# upgrade to SoilProfilecollection
	depths(h) <- labpeiid ~ hzdept + hzdepb
	
	
	## TODO: this will fail in the presence of duplicates
	# add site data to object
	site(h) <- s # left-join via labpeiid
	
  
	# 7. save and mention bad pedons
	assign('bad.labpedon.ids', value=bad.pedon.ids, envir=soilDB.env)
	if(length(bad.pedon.ids) > 0)
		message("horizon errors detected, use `get('bad.labpedon.ids', envir=soilDB.env)` for a list of pedon IDs")
	
	# done
	return(h)
}
