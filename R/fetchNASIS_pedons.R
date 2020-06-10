

# get NASIS site/pedon/horizon/diagnostic feature data
.fetchNASIS_pedons <- function(SS=TRUE, rmHzErrors=TRUE, nullFragsAreZero=TRUE, soilColorState='moist', lab=FALSE, stringsAsFactors = default.stringsAsFactors()) {
  
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
  
  # data that cannot be effectively flattened in SQL
  extended_data <- get_extended_data_from_NASIS_db(SS=SS, nullFragsAreZero=nullFragsAreZero, stringsAsFactors = stringsAsFactors)
  
  ## join horizon + hz color: all horizons
  h <- join(hz_data, color_data, by='phiid', type='left')
  
  # check for empty fragment summary and nullFragsAreZero
  if(nullFragsAreZero & all(is.na(unique(extended_data$frag_summary$phiid))))
    extended_data$frag_summary <- cbind(phiid = unique(h$phiid), extended_data$frag_summary[,-1])
  
  ## join hz + fragment summary
  h <- join(h, extended_data$frag_summary, by='phiid', type='left')
  
  # check for empty artifact summary and nullFragsAreZero
  if(nullFragsAreZero & all(is.na(unique(extended_data$art_summary$phiid))))
    extended_data$art_summary <- cbind(phiid = unique(h$phiid), extended_data$art_summary[,-1])
  
  # join hz + artifact summary
  h <- join(h, extended_data$art_summary, by='phiid', type='left')
  
  ## fix some common problems
  
  # replace missing lower boundaries
  missing.lower.depth.idx <- which(!is.na(h$hzdept) & is.na(h$hzdepb))    
  
  # keep track of affected pedon IDs (if none, this will have zero length)
  assign('missing.bottom.depths', value=unique(h$pedon_id[missing.lower.depth.idx]), envir=soilDB.env)
  
  if(length(missing.lower.depth.idx) > 0) {
    message(paste('replacing missing lower horizon depths with top depth + 1cm ... [', length(missing.lower.depth.idx), ' horizons]', sep=''))
    
    # make edit
    h$hzdepb[missing.lower.depth.idx] <- h$hzdept[missing.lower.depth.idx] + 1
  }
  
  # top == bottom ? bottom <- bottom + 1
  top.eq.bottom.idx <- which(h$hzdept == h$hzdepb)
  
  # keep track of affected pedon IDs (if none, this will have zero length)
  assign('top.bottom.equal', value=unique(h$pedon_id[	top.eq.bottom.idx]), envir=soilDB.env)
  
  if(length(top.eq.bottom.idx) > 0) {
    message(paste('top/bottom depths equal, adding 1cm to bottom depth ... [', length(top.eq.bottom.idx), ' horizons]', sep=''))
    
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
  
  
  ## test for horizonation inconsistencies... flag, and optionally remove
  # ~ 1.3 seconds / ~ 4k pedons
  h.test <- ddply(h, 'peiid', function(d) {
    res <- aqp::hzDepthTests(top=d[['hzdept']], bottom=d[['hzdepb']])
    return(data.frame(hz_logic_pass=all(!res)))
  })
  
  # which are the good (valid) ones?
  good.ids <- as.character(h.test$peiid[which(h.test$hz_logic_pass)])
  bad.ids <- as.character(h.test$peiid[which(!h.test$hz_logic_pass)])
  bad.horizons<- h[which(!h.test$hz_logic_pass), c(1:4,6,7)]
  bad.pedon.ids <- site_data$pedon_id[which(site_data$peiid %in% bad.ids)]
  
  # optionally filter pedons WITH NO horizonation inconsistencies
  if(rmHzErrors)
    h <- h[which(h$peiid %in% good.ids), ]
  
  # keep track of those pedons with horizonation errors
  assign('bad.pedon.ids', value=bad.pedon.ids, envir=soilDB.env)
  assign("bad.horizons", value = data.frame(bad.horizons), envir = soilDB.env)
  
  ## optionally convert NA fragvol to 0
  if(nullFragsAreZero) {
    # this is the "total fragment volume" per NASIS calculation
    h$fragvoltot <- ifelse(is.na(h$fragvoltot), 0, h$fragvoltot)
    
    # this is computed by soilDB::simplifyFragmentData()
    h$total_frags_pct <- ifelse(is.na(h$total_frags_pct), 0, h$total_frags_pct)
    
    # this is computed by soilDB::simplifyFragmentData()
    # no para-frags
    h$total_frags_pct_nopf <- ifelse(is.na(h$total_frags_pct_nopf), 0, h$total_frags_pct_nopf)
    
    # this is computed by soilDB::simplifyArtifactData()
    h$total_art_pct <- ifelse(is.na(h$total_art_pct), 0, h$total_art_pct)
  }
  
  # convert pedon and horizon unique ID to character
  h$peiid <- as.character(h$peiid)
  h$phiid <- as.character(h$phiid)
  
  # upgrade to SoilProfilecollection
  depths(h) <- peiid ~ hzdept + hzdepb
  
  # move pedon_id into @site
  # 1 second for ~ 4k pedons
  site(h) <- ~ pedon_id
  
  ## TODO: this will fail in the presence of duplicates
  # add site data to object
  # remove 'pedon_id' column from site_data
  site_data$pedon_id <- NULL
  
  # left-join via peiid
  # < 0.1 second for ~ 4k pedons
  site(h) <- site_data
  
  
  ### TODO: consider moving this into the extended data function ###
  # load best-guess optimal records from taxhistory
  # method is added to the new field called 'selection_method'
  # assumes that classdate is a datetime class object!
  # ddply: 26 seconds for ~ 4k pedons
  # best.tax.data <- ddply(extended_data$taxhistory, 'peiid', .pickBestTaxHistory)
  # 
  # 2019-01-31: converting to base functions
  ed.tax <- split(extended_data$taxhistory, extended_data$taxhistory$peiid)
  ed.tax.flat <- lapply(ed.tax, .pickBestTaxHistory)
  best.tax.data <- do.call('rbind', ed.tax.flat)
  site(h) <- best.tax.data
  
  # load best-guess optimal records from ecositehistory
  # method is added to the new field called 'es_selection_method'
  # 1.4 seconds for ~ 4k pedons
  best.ecosite.data <- ddply(extended_data$ecositehistory, 'siteiid', .pickBestEcosite)
  site(h) <- best.ecosite.data
  
  ## TODO: NA in diagnostic boolean columns are related to pedons with no diagnostic features
  ## https://github.com/ncss-tech/soilDB/issues/59
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
  
  # add restrictions to SPC
  # required new setter in aqp SPC object (AGB added 2019/12/23)
  suppressWarnings(restrictions(h) <- extended_data$restriction)
  
  # join-in landform string
  ## 2015-11-30: short-circuts could use some work, consider pre-marking mistakes before parsing
  # ddply: 3 seconds for ~ 4k pedons
  # lf <- ddply(extended_data$geomorph, 'peiid', .formatLandformString, name.sep=' & ')
  #
  # 2019-01-31: converting to base functions
  ed.lf <- split(extended_data$geomorph, extended_data$geomorph$peiid)
  ed.lf.flat <- lapply(ed.lf, .formatLandformString, name.sep=' & ')
  lf <- do.call('rbind', ed.lf.flat)
  if(nrow(lf) > 0)
    site(h) <- lf
  
  # join-in parent material strings
  # ddply: 4 seconds for ~ 4k pedons
  pm <- ddply(extended_data$pm, 'siteiid', .formatParentMaterialString, name.sep=' & ')
  if(nrow(pm) > 0)
    site(h) <- pm
  
  # set metadata
  m <- metadata(h)
  m$origin <- 'NASIS pedons'
  metadata(h) <- m
  
  # print any messages on possible data quality problems:
  if(exists('sites.missing.pedons', envir=soilDB.env))
    if(length(get('sites.missing.pedons', envir=soilDB.env)) > 0)
      message("-> QC: sites without pedons: use `get('sites.missing.pedons', envir=soilDB.env)` for related usersiteid values")
  
  if(exists('dup.pedon.ids', envir=soilDB.env))
    if(length(get('dup.pedon.ids', envir=soilDB.env)) > 0)
      message("-> QC: duplicate pedons: use `get('dup.pedon.ids', envir=soilDB.env)` for related peiid values")
  
  # set NASIS-specific horizon identifier
  tryCatch(hzidname(h) <- 'phiid', error = function(e) {
    if(grepl(e$message, pattern="not unique$")) {
       if(!rmHzErrors) {
        # if rmHzErrors = FALSE, keep unique integer assigned ID to all records automatically
        message("-> QC: duplicate horizons are present with rmHzErrors=FALSE! defaulting to `hzID` as unique horizon ID.")
       } else {
         stop(e)
       }
    }
  })  
  
  # set hz designation and texture fields -- NB: chose to use calculated texture -- more versatile
  # functions designed to use hztexclname() should handle presence of in-lieu, modifiers, etc.
  hzdesgnname(h) <- "hzname"
  hztexclname(h) <- "texture"
  
  if(exists('bad.pedon.ids', envir=soilDB.env))
    if(length(get('bad.pedon.ids', envir=soilDB.env)) > 0)
      message("-> QC: horizon errors detected, use `get('bad.pedon.ids', envir=soilDB.env)` for related userpedonid values or `get('bad.horizons', envir=soilDB.env)` for related horizon designations")
  
  if(exists('missing.bottom.depths', envir=soilDB.env))
    if(length(get('missing.bottom.depths', envir=soilDB.env)) > 0)
      message("-> QC: pedons missing bottom hz depths: use `get('missing.bottom.depths', envir=soilDB.env)` for related pedon IDs")
  
  if(exists('top.bottom.equal', envir=soilDB.env))
    if(length(get('top.bottom.equal', envir=soilDB.env)) > 0)
      message("-> QC: equal hz top and bottom depths: use `get('top.bottom.equal', envir=soilDB.env)` for related pedon IDs")
  
  ## https://github.com/ncss-tech/soilDB/issues/44
  # optionally load phlabresults table
  if (lab) {
    phlabresults <- .get_phlabresults_data_from_NASIS_db(SS=SS)
    horizons(h) <- phlabresults
    #h <- join(h, phlabresults, by = "phiid", type = "left")
  }
  
  # done
  return(h)
}
