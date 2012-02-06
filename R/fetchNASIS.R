fetchNASIS <- function() {
	
	# 1. load data in pieces
	site_data <- get_site_data_from_NASIS_db()
	hz_data <- get_hz_data_from_NASIS_db()
	color_data <- get_colors_from_NASIS_db()
	
	# 2. join pieces
	# horizon + hz color: all horizons
	h <- join(hz_data, color_data, by='phiid', type='left')
	
	## TODO: this creates 2 columns of 'pedon_id', temp work-around is to remove it from site data
	# (hz + color) + site: only those with horizon data
	f <- join(h, site_data[, -which(names(site_data) == 'pedon_id')], by='peiid', type='inner')
	
	
	# 3. fix some common problems
	# replace missing lower boundaries
	cat('replacing missing lower boundaries ...\n')
	f$hzdepb[!is.na(f$hzdept) & is.na(f$hzdepb)] <- f$hzdept[!is.na(f$hzdept) & is.na(f$hzdepb)]
	
	# test for bad horizonation... flag, and remove
	cat('finding horizonation errors ...\n')
	f.test <- ddply(f, .(pedon_id), test_hz_logic, topcol='hzdept', bottomcol='hzdepb', strict=TRUE)
	
	# which are the good (valid) ones?
	good.pedon.ids <- as.character(f.test$pedon_id[which(f.test$hz_logic_pass)])
	bad.pedon.ids <- as.character(f.test$pedon_id[which(f.test$hz_logic_pass == FALSE)])
	
	# keep the good ones
	f <- subset(f, pedon_id %in% good.pedon.ids)
	
	# 4. upgrade to SoilProfilecollection
	depths(f) <- pedon_id ~ hzdept + hzdepb
	
	# move site data into @site
	site(f) <- ~ peiid + site_id + siteiid + sampled_as + correlated_as + hillslope_pos + x + y + datum + elev + slope + aspect + plantassocnm + bedrckdepth + bedrock_kind + describer + psctopdepth + pscbotdepth + part_size_class + tax_subgroup + obs_date + pedon_purpose + pedon_type + pedlabsampnum
	
	# 5. convert colors... in the presence of missing color data
	f$soil_color <- rep(NA, times=nrow(horizons(f)))
	idx <- complete.cases(f$m_r)
	f$soil_color[idx] <- with(horizons(f)[idx, ], rgb(m_r, m_g, m_b)) # moist colors
	
	# 6. mention bad pedons
	if(length(bad.pedon.ids) > 0)
		cat(paste('horizon errors in:', paste(bad.pedon.ids, collapse=','), '\n'))
	
	# done
	return(f)
}
