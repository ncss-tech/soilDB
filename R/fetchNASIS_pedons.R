

# get NASIS site/pedon/horizon/diagnostic feature data
.fetchNASIS_pedons <- function(SS = TRUE,
                               fill = FALSE,
                               rmHzErrors = FALSE,
                               nullFragsAreZero = TRUE,
                               soilColorState = 'moist',
                               mixColors = FALSE,
                               lab = FALSE,
                               dsn = NULL
) {
  
  # check if NASIS local DB instance/ODBC data source is available
  .soilDB_test_NASIS_connection(dsn = dsn)
  
  if (!requireNamespace("aqp")) {
    stop("package 'aqp' is required", call. = FALSE)
  }
  
  # sanity check
  if (!soilColorState %in% c('dry', 'moist'))
    stop('soilColorState must be either `dry` or `moist`', call. = FALSE)

  ## load data in pieces
  # these fail gracefully when no data in local DB | selected set
  site_data  <- get_site_data_from_NASIS_db(SS = SS, dsn = dsn)
  hz_data    <- get_hz_data_from_NASIS_db(SS = SS, fill = fill, dsn = dsn)
  color_data <- get_colors_from_NASIS_db(SS = SS, method = ifelse(isTRUE(mixColors), "mixed", "dominant"), dsn = dsn)

  ## ensure there are enough data to create an SPC object
  ds <- ifelse(SS, "NASIS selected set", "NASIS local database")
  if (nrow(site_data) == 0) {
    stop('No site/site observation records in ', ds, call. = FALSE)
  }
  if (nrow(hz_data) == 0) {
    stop('No horizon records in ', ds,'. Use `fill = TRUE` to include pedons without horizons.', call. = FALSE)
  }
  
  ## https://github.com/ncss-tech/soilDB/issues/44
  # optionally load phlabresults table
  if (lab) {
    phlabresults <- .get_phlabresults_data_from_NASIS_db(SS = SS, dsn = dsn)
    
    # TODO: perform phlabresults aggregation method(s) here
    
    hz_data <- merge(hz_data,
                     phlabresults,
                     by = c("peiid", "phiid"),
                     all = TRUE,
                     sort = FALSE)
  }
  
  # data that cannot be effectively flattened in SQL

  extended_data <- get_extended_data_from_NASIS_db(SS = SS,
                                                   nullFragsAreZero = nullFragsAreZero,
                                                   dsn = dsn)

  ## fix some common problems

  # replace missing lower boundaries
  missing.lower.depth.idx <- which(!is.na(hz_data$hzdept) & is.na(hz_data$hzdepb))

  # keep track of affected pedon IDs (if none, this will have zero length)
  assign('missing.bottom.depths', value = unique(hz_data$upedonid[missing.lower.depth.idx]), envir = get_soilDB_env())

  if (length(missing.lower.depth.idx) > 0) {
    message(paste0('replacing missing lower horizon depths with top depth + 1cm ... [', length(missing.lower.depth.idx), ' horizons]'))

    # make edit
    hz_data$hzdepb[missing.lower.depth.idx] <- hz_data$hzdept[missing.lower.depth.idx] + 1
  }

  # top == bottom ? bottom <- bottom + 1
  top.eq.bottom.idx <- which(hz_data$hzdept == hz_data$hzdepb)

  # keep track of affected pedon IDs (if none, this will have zero length)
  assign('top.bottom.equal', value = unique(hz_data$upedonid[top.eq.bottom.idx]), envir = get_soilDB_env())

  if (length(top.eq.bottom.idx) > 0) {
    message(paste0('top/bottom depths equal, adding 1cm to bottom depth ... [', length(top.eq.bottom.idx), ' horizons]'))

    # make the edit
    hz_data$hzdepb[top.eq.bottom.idx] <- hz_data$hzdepb[top.eq.bottom.idx] + 1
  }

  filled.ids <- character(0)
  if (rmHzErrors) {

    # get overall validity (combination of 4 logic tests applied to each peiid)
    h.test <- aqp::checkHzDepthLogic(hz_data, c("hzdept","hzdepb"), "peiid", fast = TRUE)
    
    # fill=TRUE adds horizons with NA phiid will have NA depths -- will not pass logic check
    filled.idx <- which(is.na(hz_data$phiid))
    if(length(filled.idx) > 0) {
      filled.ids <- as.character(hz_data$peiid[filled.idx])
      #print(dput(filled.ids))
    }
    
    # which are the good (valid) ones?
    good.ids <- as.character(h.test$peiid[which(h.test$valid)])
    bad.ids <- as.character(h.test$peiid[which(!h.test$valid)])
    bad.horizons <- hz_data[hz_data$peiid %in% h.test$peiid[which(!h.test$valid)], 
                            c("peiid", "phiid", "upedonid", "hzname", "hzdept", "hzdepb")]
    bad.pedon.ids <- site_data$upedonid[which(site_data$peiid %in% bad.ids)]
    
    # handle fill=TRUE
    if(length(filled.ids) > 0) {
      good.ids <- unique(c(good.ids, filled.ids))
      bad.ids <- unique(bad.ids[!bad.ids %in% filled.ids])
    }
    
    # optionally filter pedons WITH NO horizonation inconsistencies
    if (rmHzErrors) {
      hz_data <- hz_data[which(hz_data$peiid %in% good.ids), ]
    }
    
    # keep track of those pedons with horizonation errors
    assign('bad.pedon.ids', value = bad.pedon.ids, envir = get_soilDB_env())
    assign("bad.horizons", value = data.frame(bad.horizons), envir = get_soilDB_env())
  }

  # convert pedon and horizon unique ID to character
  hz_data$peiid <- as.character(hz_data$peiid)
  hz_data$phiid <- as.character(hz_data$phiid)

  # upgrade to SoilProfilecollection
  aqp::depths(hz_data) <- peiid ~ hzdept + hzdepb

  # move upedonid into @site
  aqp::site(hz_data) <- ~ upedonid

  ## copy pre-computed colors into a convenience field for plotting
  # moist colors
  if (soilColorState == 'moist') {
    color_data$soil_color <- color_data$moist_soil_color
  }
  
  # dry colors
  if (soilColorState == 'dry') {
    color_data$soil_color <- color_data$dry_soil_color
  }
  
  aqp::horizons(hz_data) <- color_data

  # check for empty fragment summary and nullFragsAreZero
  if (nullFragsAreZero && 
      all(is.na(unique(extended_data$frag_summary$phiid))) && 
      nrow(extended_data$frag_summary) != 0) {
    extended_data$frag_summary <- cbind(phiid = unique(hz_data$phiid), extended_data$frag_summary[, -1])
  }
  
  ## join hz + fragment summary
  aqp::horizons(hz_data) <- extended_data$frag_summary

  # check for empty artifact summary and nullFragsAreZero
  if (nullFragsAreZero && 
      all(is.na(unique(extended_data$art_summary$phiid))) && 
      nrow(extended_data$art_summary) != 0) {
    extended_data$art_summary <- cbind(phiid = unique(hz_data$phiid), extended_data$art_summary[, -1])
  }
  
  # join hz + artifact summary
  aqp::horizons(hz_data) <- extended_data$art_summary

  # add site data to object
  # remove 'upedonid' column from site_data
  site_data$upedonid <- NULL
  
  # remove 'pedon_id' from horizon data
  hz_data$pedon_id <- NULL
  
  # TODO: duplicating surface fine gravel column with old name for backward compatibility
  site_data$surface_fgravel <- site_data$surface_fine_gravel
  
  # left-join via peiid
  # < 0.1 second for ~ 4k pedons
  aqp::site(hz_data) <- site_data

  # load best-guess optimal records from taxhistory
  # method is added to the new field called 'selection_method'
  # assumes that classdate is a datetime class object!
  # 2019-01-31: converting to base functions
  # 2020-02-17: converting to data.table

  # define data.table globals for R CMD CHECK
  .BY <- NULL
  .SD <- NULL

  ed.tax <- data.table::as.data.table(extended_data$taxhistory)
  best.tax.data <- ed.tax[, .pickBestTaxHistory(.SD),
                          by = list(peiid = ed.tax$peiid)]
  aqp::site(hz_data) <- as.data.frame(best.tax.data)

  # get "best" ecosite data (most recent correlation, or most complete if no date)
  aqp::site(hz_data) <- extended_data$ecositehistory

  ## NA in diagnostic boolean columns are related to pedons with no diagnostic features
  ## this is intentional, see: https://github.com/ncss-tech/soilDB/issues/59
  aqp::site(hz_data) <- extended_data$diagHzBoolean

  ## optionally convert NA fragvol to 0
  if (nullFragsAreZero) {
    # this is the "total fragment volume" per NASIS calculation
    hz_data$fragvoltot <- ifelse(is.na(hz_data$fragvoltot), 0, hz_data$fragvoltot)

    # this is computed by soilDB::simplifyFragmentData()
    hz_data$total_frags_pct <- ifelse(is.na(hz_data$total_frags_pct), 0, hz_data$total_frags_pct)

    # this is computed by soilDB::simplifyFragmentData()
    # no para-frags
    hz_data$total_frags_pct_nopf <- ifelse(is.na(hz_data$total_frags_pct_nopf), 0, hz_data$total_frags_pct_nopf)

    # this is computed by soilDB::simplifyArtifactData()
    hz_data$total_art_pct <- ifelse(is.na(hz_data$total_art_pct), 0, hz_data$total_art_pct)
  }

  ## 2021-11-05: converted surface frag summary to simplifyFragmentData() in get_site_data_from_NASIS_db()

  # load diagnostic horizons into @diagnostic:
  # supress warnings: diagnostic_hz() <- is noisy when not all profiles have diagnostic hz data
  suppressWarnings(aqp::diagnostic_hz(hz_data) <- extended_data$diagnostic)

  # add restrictions to SPC
  # required new setter in aqp SPC object (AGB added 2019/12/23)
  suppressWarnings(aqp::restrictions(hz_data) <- extended_data$restriction)

  # join-in landform string w/ ampersand as separator for hierarchy
  # .formatLandformString <- soilDB:::.formatLandformString
  # .formatParentMaterialString <- soilDB:::.formatParentMaterialString
  ed.lf <- data.table::as.data.table(extended_data$geomorph)
  lf <- ed.lf[, .formatLandformString(.SD, uid = .BY$peiid, name.sep = ' & '),
              by = list(peiid = ed.lf$peiid)]

  if (ncol(lf) > 1) {
    aqp::site(hz_data) <- as.data.frame(lf[, c("peiid", "landform_string", "landscape_string", "microfeature_string", "geomicrorelief_string")])
  }
  
  ed.pm <- data.table::as.data.table(extended_data$pm)
  pm <- ed.pm[, .formatParentMaterialString(.SD, uid = .BY$siteiid, name.sep = ' & '),
              by = list(siteiid = ed.pm$siteiid)]
  
  if (ncol(pm) > 2) {
    aqp::site(hz_data) <- as.data.frame(pm[, c("siteiid", "pmkind", "pmorigin")])
  }
  
  # set metadata
  m <- aqp::metadata(hz_data)
  m$origin <- 'NASIS pedons'
  m$created <- Sys.time()
  aqp::metadata(hz_data) <- m

  # print any messages on possible data quality problems:
  if (exists('sites.missing.pedons', envir = get_soilDB_env()) &&
      length(get('sites.missing.pedons', envir = get_soilDB_env())) > 0) {
    message(
      "-> QC: sites without pedons: \n\tUse `get('sites.missing.pedons', envir=get_soilDB_env())` for site record IDs (siteiid)"
    )
  }
  
  if (exists('dup.pedon.ids', envir = get_soilDB_env()) &&
      length(get('dup.pedon.ids', envir = get_soilDB_env())) > 0) {
    message(
      "-> QC: duplicate pedons: \n\tUse `get('dup.pedon.ids', envir=get_soilDB_env())` for pedon record IDs (peiid)"
    )
  }
  
  if (exists('rock.fragment.volume.gt100.phiid', envir = get_soilDB_env()) &&
      length(get('rock.fragment.volume.gt100.phiid', envir = get_soilDB_env())) > 0) {
    message(
      "-> QC: pedon horizons with rock fragment volume >=100%: \n\tUse `get('rock.fragment.volume.gt100.phiid', envir=get_soilDB_env())` for pedon horizon record IDs (phiid)"
    )
  }
  
  if (exists('artifact.volume.gt100.phiid', envir = get_soilDB_env()) &&
      length(get('artifact.volume.gt100.phiid', envir = get_soilDB_env())) > 0) {
    message(
      "-> QC: pedon horizons with artifact volume >=100%: \n\tUse `get('artifact.volume.gt100.phiid', envir=get_soilDB_env())` for pedon horizon record IDs (phiid)"
    )
  }
  
  if (exists('multisiteobs.surface', envir = get_soilDB_env()) &&
      length(get('multisiteobs.surface', envir = get_soilDB_env())) > 0) {
    message(
      "-> QC: surface fragment records from multiple site observations:\n\tUse `get('multisiteobs.surface', envir=get_soilDB_env())` for site (siteiid) and site observation (siteobsiid)"
    )
  }
  
  if (exists('surface.fragment.cover.gt100.siteobsiid', envir = get_soilDB_env()) &&
      length(get('surface.fragment.cover.gt100.siteobsiid', envir = get_soilDB_env())) > 0) {
    message(
      "-> QC: pedons with surface fragment cover >=100%: \n\tUse `get('surface.fragment.cover.gt100.siteobsiid', envir=get_soilDB_env())` for site observation record IDs (siteobsiid)"
    )
  }
  
  
  if (exists('multiple.labsampnum.per.phiid', envir = get_soilDB_env()) &&
      length(get('multiple.labsampnum.per.phiid', envir = get_soilDB_env())) > 0) {
    message(
      "-> QC: horizons with multiple lab samples: \n\tUse `get('multiple.labsampnum.per.phiid', envir=get_soilDB_env())` for pedon horizon record IDs (phiid)"
    )
  }
  
  # set NASIS component specific horizon identifier
  if (!fill && length(filled.ids) == 0) {
    res <- try(aqp::hzidname(hz_data) <- 'phiid')
    if (inherits(res, 'try-error')) {
      if (!rmHzErrors) {
        warning("cannot set `phiid` as unique pedon horizon key -- duplicate horizons present with rmHzErrors=FALSE")
      } else {
        warning("cannot set `phiid` as unique pedon horizon key -- defaulting to `hzID`")
      }
    }
  } else {
    warning("cannot set `phiid` as unique pedon horizon key - `NA` introduced by fill=TRUE", call.=FALSE)
  }
  
  # set hz designation and texture fields -- NB: chose to use calculated texture -- more versatile
  # functions designed to use hztexclname() should handle presence of in-lieu, modifiers, etc.
  aqp::hzdesgnname(hz_data) <- "hzname"
  aqp::hztexclname(hz_data) <- "texture"

  if (exists('bad.pedon.ids', envir = get_soilDB_env()) &&
      length(get('bad.pedon.ids', envir = get_soilDB_env())) > 0) {
    message("-> QC: horizon errors detected:\n\tUse `get('bad.pedon.ids', envir=get_soilDB_env())` for pedon record IDs (peiid)\n\tUse `get('bad.horizons', envir=get_soilDB_env())` for horizon designations")
  }
  
  if (exists('missing.bottom.depths', envir = get_soilDB_env()) &&
      length(get('missing.bottom.depths', envir = get_soilDB_env())) > 0) {
    message("-> QC: pedons missing bottom hz depths:\n\tUse `get('missing.bottom.depths', envir=get_soilDB_env())` for pedon record IDs (peiid)")
  }
  
  if (exists('top.bottom.equal', envir = get_soilDB_env()) &&
      length(get('top.bottom.equal', envir = get_soilDB_env())) > 0){
    message("-> QC: equal hz top and bottom depths:\n\tUse `get('top.bottom.equal', envir=get_soilDB_env())` for pedon record IDs (peiid)")
  }
  
  # done
  return(hz_data)
}
