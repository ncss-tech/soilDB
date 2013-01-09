# updated to NASIS 6.2 -- needs testing
# horizon checking may be too strict

# convenience function for loading most commonly used information from local NASIS database
fetchNASIS <- function() {
	
	# 0. test connection
	if(! 'nasis_local' %in% names(odbcDataSources()))
			stop('Local NASIS ODBC connection has not been setup. Please see the `setup_ODBC_local_NASIS.pdf` document included with this package.')
	
	# 1. load data in pieces
	site_data <- get_site_data_from_NASIS_db()
	hz_data <- get_hz_data_from_NASIS_db()
	color_data <- get_colors_from_NASIS_db()
	extended_data <- get_extended_data_from_NASIS_db()
	
	# test for multiple pedons / site
	# note that this is also checked and reported on in get_site_data_from_NASIS_db()
	multiple.pedons.per.site <- names(which(table(site_data$pedon_id) > 1))
	
	# 2. join pieces
	# horizon + hz color: all horizons
	h <- join(hz_data, color_data, by='phiid', type='left')
	
	## TODO: this creates 2 columns of 'pedon_id', temp work-around is to remove it from site data
	# (hz + color) + site: only those with horizon data
	f <- join(h, site_data[, -which(names(site_data) == 'pedon_id')], by='peiid', type='inner')
	
	# 3. fix some common problems
	# replace missing lower boundaries
	message('replacing missing lower boundaries ...')
	f$hzdepb[!is.na(f$hzdept) & is.na(f$hzdepb)] <- f$hzdept[!is.na(f$hzdept) & is.na(f$hzdepb)]
	
	## TODO: is this really a good idea?
	# when multiple pedons / site, replace pedon_id with pedon_id-peiid
	pedon.ids.to.fix.idx <- which(f$pedon_id %in% multiple.pedons.per.site)
	if(length(pedon.ids.to.fix.idx) > 0)
		message('appending peiid to duplicate pedon IDs...')
	
	# fix offending pedon IDs by concatenating pedon_id with peiid
	f$pedon_id[pedon.ids.to.fix.idx] <- paste(f$pedon_id[pedon.ids.to.fix.idx], f$peiid[pedon.ids.to.fix.idx], sep='-')
	
	## TODO: this can be made more efficient
	# test for bad horizonation... flag, and remove
	message('finding horizonation errors ...')
	f.test <- ddply(f, 'pedon_id', test_hz_logic, topcol='hzdept', bottomcol='hzdepb', strict=TRUE)
	
	# which are the good (valid) ones?
	good.pedon.ids <- as.character(f.test$pedon_id[which(f.test$hz_logic_pass)])
	bad.pedon.ids <- as.character(f.test$pedon_id[which(f.test$hz_logic_pass == FALSE)])
	
	# keep the good ones
	f <- subset(f, pedon_id %in% good.pedon.ids)
	
	# 4. upgrade to SoilProfilecollection
	depths(f) <- pedon_id ~ hzdept + hzdepb
	
	# move site data into @site
	site(f) <- ~ peiid + site_id + siteiid + hillslope_pos + x + y + datum + elev_field + slope_field + aspect_field + plantassocnm + bedrckdepth + bedrock_kind + bedrock_hardness + describer + psctopdepth + pscbotdepth + obs_date + pedon_purpose + pedon_type + pedlabsampnum
	
	# 5. convert colors... in the presence of missing color data
	f$soil_color <- rep(NA, times=nrow(horizons(f)))
	idx <- complete.cases(f$m_r)
	f$soil_color[idx] <- with(horizons(f)[idx, ], rgb(m_r, m_g, m_b)) # moist colors
	
	# 6. merge-in extended data:
	# replace horizons with hz + fragment summary
	horizons(f) <- join(horizons(f), extended_data$frag_summary, by='phiid', type='left')
	
	# add diagnostic boolean data into @site
	site(f) <- extended_data$diagHzBoolean
	
	# add surface frag summary
	site(f) <- extended_data$surf_frag_summary
	
	# load diagnostic horizons into @diagnostic: note that this requires one additional join 
	# and implicitly filters diagnostic hz by our subset of f
	diagnostic_hz(f) <- join(site(f)[, c('pedon_id','peiid')], extended_data$diagnostic, by='peiid', type='left')
	
	# 6.1 load best-guess optimal records from taxhistory
	# method is added to the new field called 'selection_method'
	best.tax.data <- ddply(extended_data$taxhistory, 'peiid', pickBestTaxHistory)
	site(f) <- best.tax.data
	
	# 7. mention bad pedons
	if(length(bad.pedon.ids) > 0)
		message(paste('horizon errors in:', paste(bad.pedon.ids, collapse=',')))
	
	# done
	return(f)
}
