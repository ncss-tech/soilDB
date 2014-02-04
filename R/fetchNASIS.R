# updated to NASIS 6.2

# convenience function for loading most commonly used information from local NASIS database
fetchNASIS <- function() {
	
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
	message('replacing missing lower horizon boundaries ...')
	missing.lower.depth.idx <- which(!is.na(h$hzdept) & is.na(h$hzdepb))
	h$hzdepb[missing.lower.depth.idx] <- h$hzdept[missing.lower.depth.idx]
	
## as of NASIS 6.3 we will use peiid as our ID
## remove this code
# 	# in the presence of duplicate pedons, replace pedon_id with pedon_id-peiid
# 	# test for duplicate pedons
# 	if(exists('dup.pedon.ids', envir=soilDB.env)) {
# 		message('appending peiid to duplicate pedon IDs...')
# 		
# 		# extract from error reporting environment
# 		dupe.pedons <- get('dup.pedon.ids', envir=soilDB.env)	
# 		# convert into row-index
# 		pedon.ids.to.fix.idx <- which(h$pedon_id %in% dupe.pedons)
# 		
# 		# fix offending pedon IDs by concatenating pedon_id with peiid
# 		h$pedon_id[pedon.ids.to.fix.idx] <- paste(h$pedon_id[pedon.ids.to.fix.idx], h$peiid[pedon.ids.to.fix.idx], sep='-')
# 	}
	
	# convert colors... in the presence of missing color data
	h$soil_color <- NA
	idx <- complete.cases(h$m_r)
	h$soil_color[idx] <- with(h[idx, ], rgb(m_r, m_g, m_b)) # moist colors
	
	
	# join hz + fragment summary
	h <- join(h, extended_data$frag_summary, by='phiid', type='left')
	
	# test for bad horizonation... flag, and remove
	message('finding horizonation errors ...')
	h.test <- ddply(h, 'peiid', test_hz_logic, topcol='hzdept', bottomcol='hzdepb', strict=TRUE)
	
	# which are the good (valid) ones?
	good.pedon.ids <- as.character(h.test$peiid[which(h.test$hz_logic_pass)])
	bad.pedon.ids <- as.character(h.test$peiid[which(!h.test$hz_logic_pass)])
	
	# keep the good ones
	h <- h[which(h$peiid %in% good.pedon.ids), ]
	
	# upgrade to SoilProfilecollection
	depths(h) <- peiid ~ hzdept + hzdepb
	
	## TODO: this is slow
	# move pedon_id into @site, this will be used to join full table of site data
	site(h) <- ~ pedon_id
	
	## TODO: this will fail in the presence of duplicates
	# add site data to object
	site_data$pedon_id <- NULL # remove 'pedon_id' column from site_data
	site(h) <- site_data # left-join via peiid
	
	
	### TODO: consider moving this into the extended data function ###
	# load best-guess optimal records from taxhistory
	# method is added to the new field called 'selection_method'
	best.tax.data <- ddply(extended_data$taxhistory, 'peiid', pickBestTaxHistory)
	site(h) <- best.tax.data
	
	# add diagnostic boolean data into @site
	site(h) <- extended_data$diagHzBoolean
	
	# add surface frag summary
	site(h) <- extended_data$surf_frag_summary
	
	## TODO: we don't really need pedon_id in @diagnostic
	# load diagnostic horizons into @diagnostic: note that this requires one additional join 
	# and implicitly filters diagnostic hz by our subset of f
# 	diagnostic_hz(h) <- join(site(h)[, c('pedon_id','peiid')], extended_data$diagnostic, by='peiid', type='left')
	diagnostic_hz(h) <- extended_data$diagnostic
		
	# 7. save and mention bad pedons
	assign('bad.pedon.ids', value=bad.pedon.ids, envir=soilDB.env)
	if(length(bad.pedon.ids) > 0)
		message("horizon errors detected, use `get('bad.pedon.ids', envir=soilDB.env)` for a list of pedon IDs")
	
	# done
	return(h)
}
