# horizon checking may be too strict

fetchPedonPC <- function(dsn) {
	
	# 1. load data in pieces
	site_data <- get_site_data_from_pedon_db(dsn)
	hz_data <- get_hz_data_from_pedon_db(dsn)
	color_data <- get_colors_from_pedon_db(dsn)
	extended_data <- get_extended_data_from_pedon_db(dsn)
	
	# 2. join pieces
	# horizon + hz color: all horizons
	h <- join(hz_data, color_data, by='phiid', type='left')
	
#	## TODO: this creates 2 columns of 'pedon_id', temp work-around is to remove it from site data
#	# (hz + color) + site: only those with horizon data
#	#f <- join(h, site_data[, -which(names(site_data) == 'pedon_id')], by='peiid', type='inner')
	
	
	# 3. fix some common problems
	# replace missing lower boundaries
	message('replacing missing lower horizon boundaries ...')
	missing.lower.depth.idx <- which(!is.na(h$hzdept) & is.na(h$hzdepb))
	h$hzdepb[missing.lower.depth.idx] <- h$hzdept[missing.lower.depth.idx]

		
	# test for bad horizonation... flag, and remove
	cat('finding horizonation errors ...\n')
	h.test <- ddply(h, 'peiid', test_hz_logic, topcol='hzdept', bottomcol='hzdepb', strict=TRUE)
	
	# which are the good (valid) ones?
	good.pedon.ids <- as.character(f.test$peiid[which(h.test$hz_logic_pass)])
	bad.pedon.ids <- as.character(f.test$pedon_id[which(!h.test$hz_logic_pass)])
	
	# keep the good ones
	h <- h[which(h$peiid %in% good.pedon.ids), ]
	
	# 4. upgrade to SoilProfilecollection
	depths(h) <- peiid ~ hzdept + hzdepb

	## TODO: this is slow
	# move pedon_id into @site, this will be used to join full table of site data
	site(h) <- ~ pedon_id
	
	## TODO: this will fail in the presence of duplicates
	# add site data to object
	site_data$pedon_id <- NULL # remove 'pedon_id' column from site_data
	site(h) <- site_data # left-join via peiid
	
#	# move site data into @site
#	#site(f) <- ~ peiid + site_id + siteiid + hillslope_pos + x + y + datum + elev_field + slope_field + aspect_field + plantassocnm + bedrckdepth + bedrock_kind + bedrock_hardness + describer + psctopdepth + pscbotdepth + obs_date + pedon_purpose + pedon_type + pedlabsampnum
	
	# 5. convert colors... in the presence of missing color data
	h$soil_color <- NA
	idx <- complete.cases(h$m_r)
	h$soil_color[idx] <- with(h[idx, ], rgb(m_r, m_g, m_b)) # moist colors
	
	# 6. merge-in extended data:
	# replace horizons with hz + fragment summary
	h <- join(h, extended_data$frag_summary, by='phiid', type='left')
		
	## TODO: we don't really need pedon_id in @diagnostic
	# load diagnostic horizons into @diagnostic: note that this requires one additional join
	diagnostic_hz(h) <- join(site(h)[, c('pedon_id','peiid')], extended_data$diagnostic, by='peiid', type='left')
	diagnostic_hz(h) <- extended_data$diagnostic

	# add diagnostic boolean data into @site
	site(h) <- extended_data$diagHzBoolean

	### TODO: consider moving this into the extended data function ###
	# load best-guess optimal records from taxhistory
	# method is added to the new field called 'selection_method'
	best.tax.data <- ddply(extended_data$taxhistory, 'peiid', pickBestTaxHistory)
	site(h) <- best.tax.data
	
	# 7. save and mention bad pedons
	assign('bad.pedon.ids', value=bad.pedon.ids, envir=soilDB.env)
	if(length(bad.pedon.ids) > 0)
		message("horizon errors detected, use `get('bad.pedon.ids', envir=soilDB.env)` for a list of pedon IDs")
		
	# done
	return(h)
}
