# updated to NASIS 6.2

# convenience function for loading most commonly used information from local NASIS database
fetchNASIS <- function(rmHzErrors=TRUE) {
	
  # must have RODBC installed
  if(!require(RODBC))
    stop('please install the `RODBC` package', call.=FALSE)
  
	# test connection
	if(! 'nasis_local' %in% names(odbcDataSources()))
			stop('Local NASIS ODBC connection has not been setup. Please see the `setup_ODBC_local_NASIS.pdf` document included with this package.')
	
	# 1. load data in pieces
	site_data <- get_site_data_from_NASIS_db()
	hz_data <- get_hz_data_from_NASIS_db()
	color_data <- get_colors_from_NASIS_db()
	extended_data <- get_extended_data_from_NASIS_db()
	
	# join horizon + hz color: all horizons
	h <- join(hz_data, color_data, by='phiid', type='left')
	
	# fix some common problems
	# replace missing lower boundaries
	missing.lower.depth.idx <- which(!is.na(h$hzdept) & is.na(h$hzdepb))
  if(length(missing.lower.depth.idx) > 0) {
    message(paste('replacing missing lower horizon depths with top depth + 1cm ... [', length(missing.lower.depth.idx), ' horizons]', sep=''))
    h$hzdepb[missing.lower.depth.idx] <- h$hzdept[missing.lower.depth.idx] + 1
  }
	
	
	# convert colors... in the presence of missing color data
	h$soil_color <- NA
	idx <- complete.cases(h$m_r)
	h$soil_color[idx] <- with(h[idx, ], rgb(m_r, m_g, m_b)) # moist colors
	
	# join hz + fragment summary
	h <- join(h, extended_data$frag_summary, by='phiid', type='left')
	
	# optionally test for bad horizonation... flag, and remove
  if(rmHzErrors) {
    message('finding horizonation errors ...')
    h.test <- ddply(h, 'peiid', test_hz_logic, topcol='hzdept', bottomcol='hzdepb', strict=TRUE)
    
    # which are the good (valid) ones?
    good.ids <- as.character(h.test$peiid[which(h.test$hz_logic_pass)])
    bad.ids <- as.character(h.test$peiid[which(!h.test$hz_logic_pass)])
    bad.pedon.ids <- site_data$pedon_id[which(site_data$peiid %in% bad.ids)]
    
    # keep the good ones
    h <- h[which(h$peiid %in% good.ids), ]
    
    # keep track of those pedons with horizonation errors
    assign('bad.pedon.ids', value=bad.pedon.ids, envir=soilDB.env)
    if(length(bad.pedon.ids) > 0)
      message("horizon errors detected, use `get('bad.pedon.ids', envir=soilDB.env)` for a list of pedon IDs")
  }
	
	
	# upgrade to SoilProfilecollection
	depths(h) <- peiid ~ hzdept + hzdepb
	
	## TODO: this is slow
	# move pedon_id into @site
	site(h) <- ~ pedon_id
	
	## TODO: this will fail in the presence of duplicates
	# add site data to object
	site_data$pedon_id <- NULL # remove 'pedon_id' column from site_data
	site(h) <- site_data # left-join via peiid
	
	
	### TODO: consider moving this into the extended data function ###
	# load best-guess optimal records from taxhistory
	# method is added to the new field called 'selection_method'
	best.tax.data <- ddply(extended_data$taxhistory, 'peiid', .pickBestTaxHistory)
	site(h) <- best.tax.data
	
	# add diagnostic boolean data into @site
	site(h) <- extended_data$diagHzBoolean
	
	# add surface frag summary
	site(h) <- extended_data$surf_frag_summary
	
	# load diagnostic horizons into @diagnostic:
	diagnostic_hz(h) <- extended_data$diagnostic
	
  # join-in landform string
  lf <- ddply(extended_data$geomorph, 'peiid', .formatLandformString, name.sep='|')
  if(nrow(lf) > 0)
    site(h) <- lf
  
  # join-in parent material strings
  pm <- ddply(extended_data$pm, 'siteiid', .formatParentMaterialString, name.sep='|')
  if(nrow(pm) > 0)
    site(h) <- pm
  
	# done
	return(h)
}
