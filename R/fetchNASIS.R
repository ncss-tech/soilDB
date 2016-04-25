# updated to NASIS 6.2

# convenience function for loading most commonly used information from local NASIS database
fetchNASIS <- function(rmHzErrors=TRUE, nullFragsAreZero=TRUE, soilColorState='moist') {
	
	# test connection
	if(! 'nasis_local' %in% names(RODBC::odbcDataSources()))
			stop('Local NASIS ODBC connection has not been setup. Please see `https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/setup_local_nasis.html?root=aqp`.')
	
  # sanity check
  if(! soilColorState %in% c('dry', 'moist'))
    stop('soilColorState must be either `dry` or `moist`', call. = FALSE)
  
	# 1. load data in pieces
	suppressMessages(site_data <- get_site_data_from_NASIS_db())
	hz_data <- get_hz_data_from_NASIS_db()
	color_data <- get_colors_from_NASIS_db()
	extended_data <- get_extended_data_from_NASIS_db(nullFragsAreZero)
	
	# test to see if the selected set is loaded
	if (nrow(hz_data) == 0 | all(unlist(lapply(extended_data, nrow)) == 0)) message('your selected set is missing either the pedon or site table, please load and try again')
  
	## this is the "total fragment volume" per NASIS calculation
  # optionally convert NA fragvol to 0
  if(nullFragsAreZero) {
    hz_data$total_frags_pct_cal <- ifelse(is.na(hz_data$total_frags_pct_cal), 0, hz_data$total_frags_pct_cal)
  }
  
  
	# join horizon + hz color: all horizons
	h <- join(hz_data, color_data, by='phiid', type='left')
	
	# fix some common problems
	# replace missing lower boundaries
	missing.lower.depth.idx <- which(!is.na(h$hzdept) & is.na(h$hzdepb))
  if(length(missing.lower.depth.idx) > 0) {
    message(paste('replacing missing lower horizon depths with top depth + 1cm ... [', length(missing.lower.depth.idx), ' horizons]', sep=''))
    h$hzdepb[missing.lower.depth.idx] <- h$hzdept[missing.lower.depth.idx] + 1
  }
	
	
	## copy pre-computed colors into a convenience field for plotting
	# moist colors
	if(soilColorState == 'moist')
	  h$soil_color <- h$moist_soil_color
	
	# dry colors
	if(soilColorState == 'dry')
	  h$soil_color <- h$dry_soil_color
	
	
	## join hz + fragment summary
	h <- join(h, extended_data$frag_summary, by='phiid', type='left')
	
	# optionally test for bad horizonation... flag, and remove
  if(rmHzErrors) {
    h.test <- ddply(h, 'peiid', test_hz_logic, topcol='hzdept', bottomcol='hzdepb', strict=TRUE)
    
    # which are the good (valid) ones?
    good.ids <- as.character(h.test$peiid[which(h.test$hz_logic_pass)])
    bad.ids <- as.character(h.test$peiid[which(!h.test$hz_logic_pass)])
    bad.horizons<- h[which(!h.test$hz_logic_pass), c(1:4,6,7)]
    bad.pedon.ids <- site_data$pedon_id[which(site_data$peiid %in% bad.ids)]
    
    # keep the good ones
    h <- h[which(h$peiid %in% good.ids), ]
    
    # keep track of those pedons with horizonation errors
    if(length(bad.pedon.ids) > 0) {
      assign('bad.pedon.ids', value=bad.pedon.ids, envir=soilDB.env)
      assign("bad.horizons", value = data.frame(bad.horizons), envir = soilDB.env)
    }
      
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

	# load best-guess optimal records from ecositehistory
	# method is added to the new field called 'es_selection_method'
	best.ecosite.data <- ddply(extended_data$ecositehistory, 'siteiid', .pickBestEcosite)
	site(h) <- best.ecosite.data
	
	# add diagnostic boolean data into @site
	site(h) <- extended_data$diagHzBoolean
	
	
	## TODO: convert this to simplifyFragmentData
	# add surface frag summary
  sfs <- extended_data$surf_frag_summary
  # optionally convert NA fragvol to 0
  if(nullFragsAreZero) {
    sfs <- as.data.frame(
      cbind(sfs[, 1, drop=FALSE], 
            lapply(sfs[, -1], function(i) ifelse(is.na(i), 0, i))
            ), stringsAsFactors=FALSE)
  }
  
  # add surf. frag summary to @site
	site(h) <- sfs
	
	# load diagnostic horizons into @diagnostic:
  # supress warnings, usually associated with site/pedon data that have been removed due to lack of horizon data
  suppressWarnings(diagnostic_hz(h) <- extended_data$diagnostic)
  
  # join-in landform string
  ## 2015-11-30: short-circuts could use some work, consider pre-marking mistakes before parsing
  lf <- ddply(extended_data$geomorph, 'peiid', .formatLandformString, name.sep=' & ')
  if(nrow(lf) > 0)
    site(h) <- lf
  
  # join-in parent material strings
  pm <- ddply(extended_data$pm, 'siteiid', .formatParentMaterialString, name.sep=' & ')
  if(nrow(pm) > 0)
    site(h) <- pm
  
  # print any messages on possible data quality problems:
	if(exists('sites.missing.pedons', envir=soilDB.env))
	  message("-> QC: sites without pedons: use `get('sites.missing.pedons', envir=soilDB.env)` for related usersiteid values")
  
	if(exists('dup.pedon.ids', envir=soilDB.env))
	  message("-> QC: duplicate pedons: use `get('dup.pedon.ids', envir=soilDB.env)` for related peiid values")
  
  if(exists('bad.pedon.ids', envir=soilDB.env))
	  message("-> QC: horizon errors detected, use `get('bad.pedon.ids', envir=soilDB.env)` for related userpedonid values or `get('bad.horizons', envir=soilDB.env)` for related horizon designations")
  
	# done
	return(h)
}
