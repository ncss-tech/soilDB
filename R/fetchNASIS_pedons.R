

# get NASIS site/pedon/horizon/diagnostic feature data
fetchNASIS_pedons <- function(SS=TRUE, rmHzErrors=TRUE, nullFragsAreZero=TRUE, soilColorState='moist', lab=FALSE, stringsAsFactors = default.stringsAsFactors()) {
  
  # test connection
  if(! 'nasis_local' %in% names(RODBC::odbcDataSources()))
    stop('Local NASIS ODBC connection has not been setup. Please see `http://ncss-tech.github.io/AQP/soilDB/setup_local_nasis.html`.')
  
  # sanity check
  if(! soilColorState %in% c('dry', 'moist'))
    stop('soilColorState must be either `dry` or `moist`', call. = FALSE)
  
  ## load data in pieces
  # these fail gracefully when no data in local DB | selected set
  site_data  <- get_site_data_from_NASIS_db(SS=SS, stringsAsFactors = stringsAsFactors)
  hz_data    <- get_hz_data_from_NASIS_db(SS=SS, stringsAsFactors = stringsAsFactors)
  color_data <- get_colors_from_NASIS_db(SS=SS)
  
  ## ensure there are enough data to create an SPC object
  if (nrow(hz_data) == 0) {
    stop('No site/pedons objects in local NASIS DB or selected set.', call. = FALSE)
  }
  
  ## TODO: improve efficiency
  # data that don't flatten well
  extended_data <- get_extended_data_from_NASIS_db(SS=SS, nullFragsAreZero=nullFragsAreZero, stringsAsFactors = stringsAsFactors)
  
  ## https://github.com/ncss-tech/soilDB/issues/44
  # optionally load phlabresults table
  if (lab) {
    phlabresults <- get_phlabresults_data_from_NASIS_db(SS=SS)
  }
  
  ## this is the "total fragment volume" per NASIS calculation
  # optionally convert NA fragvol to 0
  if(nullFragsAreZero) {
    hz_data$fragvoltot <- ifelse(is.na(hz_data$fragvoltot), 0, hz_data$fragvoltot)
  }
  
  # join horizon + hz color: all horizons
  h <- join(hz_data, color_data, by='phiid', type='left')
  
  ## fix some common problems
  
  # replace missing lower boundaries
  missing.lower.depth.idx <- which(!is.na(h$hzdept) & is.na(h$hzdepb))
  if(length(missing.lower.depth.idx) > 0) {
    message(paste('replacing missing lower horizon depths with top depth + 1cm ... [', length(missing.lower.depth.idx), ' horizons]', sep=''))
    
    # keep track of affected pedon IDs
    assign('missing.bottom.depths', value=unique(h$pedon_id[missing.lower.depth.idx]), envir=soilDB.env)
    
    # make edit
    h$hzdepb[missing.lower.depth.idx] <- h$hzdept[missing.lower.depth.idx] + 1
  }
  
  # top == bottom ? bottom <- bottom + 1
  top.eq.bottom.idx <- which(h$hzdept == h$hzdepb)
  if(length(top.eq.bottom.idx) > 0) {
    message(paste('top/bottom depths equal, adding 1cm to bottom depth ... [', length(top.eq.bottom.idx), ' horizons]', sep=''))
    
    # keep track of affected pedon IDs
    assign('top.bottom.equal', value=unique(h$pedon_id[	top.eq.bottom.idx]), envir=soilDB.env)
    
    # make the edit
    h$hzdepb[top.eq.bottom.idx] <- h$hzdepb[top.eq.bottom.idx] + 1
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
  
  # test for horizonation inconsistencies... flag, and optionally remove
  h.test <- ddply(h, 'peiid', test_hz_logic, topcol='hzdept', bottomcol='hzdepb', strict=TRUE)
  
  # which are the good (valid) ones?
  good.ids <- as.character(h.test$peiid[which(h.test$hz_logic_pass)])
  bad.ids <- as.character(h.test$peiid[which(!h.test$hz_logic_pass)])
  bad.horizons<- h[which(!h.test$hz_logic_pass), c(1:4,6,7)]
  bad.pedon.ids <- site_data$pedon_id[which(site_data$peiid %in% bad.ids)]
  
  # optionally filter pedons WITH NO horizonation inconsistencies
  if(rmHzErrors)
    h <- h[which(h$peiid %in% good.ids), ]
  
  # keep track of those pedons with horizonation errors
  if(length(bad.pedon.ids) > 0) {
    assign('bad.pedon.ids', value=bad.pedon.ids, envir=soilDB.env)
    assign("bad.horizons", value = data.frame(bad.horizons), envir = soilDB.env)
  }
  
  ## join hz + phlabresults
  if (lab) {
    h <- join(h, phlabresults, by = "phiid", type = "left")
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
  # assumes that classdate is a datetime class object!
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
  # supress warnings: diagnostic_hz() <- is noisy when not all profiles have diagnostic hz data
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
  
  if(exists('missing.bottom.depths', envir=soilDB.env))
    message("-> QC: pedons missing bottom hz depths: use `get('missing.bottom.depths', envir=soilDB.env)` for related pedon IDs")
  
  if(exists('top.bottom.equal', envir=soilDB.env))
    message("-> QC: equal hz top and bottom depths: use `get('top.bottom.equal', envir=soilDB.env)` for related pedon IDs")
  
  # done
  return(h)
}
