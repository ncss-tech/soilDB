

# get NASIS site/pedon/horizon/diagnostic feature data
.fetchNASIS_pedons <- function(SS = TRUE,
                               rmHzErrors = TRUE,
                               nullFragsAreZero = TRUE,
                               soilColorState = 'moist',
                               lab = FALSE,
                               stringsAsFactors = default.stringsAsFactors(),
                               dsn = NULL
) {

  # test connection
  if (!local_NASIS_defined(dsn) & !inherits(dsn, 'DBIConnection'))
    stop('Local NASIS ODBC connection has not been set up. Please see `http://ncss-tech.github.io/AQP/soilDB/setup_local_nasis.html`.')

  # sanity check
  if (!soilColorState %in% c('dry', 'moist'))
    stop('soilColorState must be either `dry` or `moist`', call. = FALSE)

  ## load data in pieces
  # these fail gracefully when no data in local DB | selected set
  site_data  <- get_site_data_from_NASIS_db(SS = SS, stringsAsFactors = stringsAsFactors,
                                            dsn = dsn)
  hz_data    <- get_hz_data_from_NASIS_db(SS = SS, stringsAsFactors = stringsAsFactors,
                                          dsn = dsn)
  color_data <- get_colors_from_NASIS_db(SS = SS, dsn = dsn)

  ## ensure there are enough data to create an SPC object
  if (nrow(hz_data) == 0) {
    stop('No site/pedons objects in local NASIS DB or selected set.', call. = FALSE)
  }

  # data that cannot be effectively flattened in SQL

  extended_data <- get_extended_data_from_NASIS_db(SS = SS,
                                                   nullFragsAreZero = nullFragsAreZero,
                                                   stringsAsFactors = stringsAsFactors,
                                                   dsn = dsn)

  ## fix some common problems

  # replace missing lower boundaries
  missing.lower.depth.idx <- which(!is.na(hz_data$hzdept) & is.na(hz_data$hzdepb))

  # keep track of affected pedon IDs (if none, this will have zero length)
  assign('missing.bottom.depths', value = unique(hz_data$pedon_id[missing.lower.depth.idx]), envir = soilDB.env)

  if (length(missing.lower.depth.idx) > 0) {
    message(paste0('replacing missing lower horizon depths with top depth + 1cm ... [', length(missing.lower.depth.idx), ' horizons]'))

    # make edit
    hz_data$hzdepb[missing.lower.depth.idx] <- hz_data$hzdept[missing.lower.depth.idx] + 1
  }

  # top == bottom ? bottom <- bottom + 1
  top.eq.bottom.idx <- which(hz_data$hzdept == hz_data$hzdepb)

  # keep track of affected pedon IDs (if none, this will have zero length)
  assign('top.bottom.equal', value = unique(hz_data$pedon_id[	top.eq.bottom.idx]), envir = soilDB.env)

  if (length(top.eq.bottom.idx) > 0) {
    message(paste0('top/bottom depths equal, adding 1cm to bottom depth ... [', length(top.eq.bottom.idx), ' horizons]'))

    # make the edit
    hz_data$hzdepb[top.eq.bottom.idx] <- hz_data$hzdepb[top.eq.bottom.idx] + 1
  }

  #  aqp uses data.table for efficient logic checking
  if (rmHzErrors) {

    # get overall validity (combination of 4 logic tests applied to each peiid)
    h.test <- aqp::checkHzDepthLogic(hz_data, c("hzdept","hzdepb"), "peiid", fast = TRUE)

    # which are the good (valid) ones?
    good.ids <- as.character(h.test$peiid[which(h.test$valid)])
    bad.ids <- as.character(h.test$peiid[which(!h.test$valid)])
    bad.horizons <- hz_data[which(!h.test$valid), c("peiid", "phiid",
                                                    "pedon_id", "hzname",
                                                    "hzdept", "hzdepb")]
    bad.pedon.ids <- site_data$pedon_id[which(site_data$peiid %in% bad.ids)]

    # optionally filter pedons WITH NO horizonation inconsistencies
    if (rmHzErrors)
      hz_data <- hz_data[which(hz_data$peiid %in% good.ids), ]

    # keep track of those pedons with horizonation errors
    assign('bad.pedon.ids', value = bad.pedon.ids, envir = soilDB.env)
    assign("bad.horizons", value = data.frame(bad.horizons), envir = soilDB.env)
  }

  # convert pedon and horizon unique ID to character
  hz_data$peiid <- as.character(hz_data$peiid)
  hz_data$phiid <- as.character(hz_data$phiid)

  # upgrade to SoilProfilecollection
  depths(hz_data) <- peiid ~ hzdept + hzdepb

  # move pedon_id into @site
  site(hz_data) <- ~ pedon_id

  ## copy pre-computed colors into a convenience field for plotting
  # moist colors
  if(soilColorState == 'moist')
    color_data$soil_color <- color_data$moist_soil_color

  # dry colors
  if(soilColorState == 'dry')
    color_data$soil_color <- color_data$dry_soil_color

  horizons(hz_data) <- color_data

  # check for empty fragment summary and nullFragsAreZero
  if(nullFragsAreZero & all(is.na(unique(extended_data$frag_summary$phiid))))
    extended_data$frag_summary <- cbind(phiid = unique(hz_data$phiid), extended_data$frag_summary[,-1])

  ## join hz + fragment summary
  horizons(hz_data) <- extended_data$frag_summary

  # check for empty artifact summary and nullFragsAreZerod
  if(nullFragsAreZero & all(is.na(unique(extended_data$art_summary$phiid))))
    extended_data$art_summary <- cbind(phiid = unique(hz_data$phiid), extended_data$art_summary[,-1])

  # join hz + artifact summary
  horizons(hz_data) <- extended_data$art_summary

  ## TODO: this will fail in the presence of duplicates
  # add site data to object
  # remove 'pedon_id' column from site_data
  site_data$pedon_id <- NULL

  # left-join via peiid
  # < 0.1 second for ~ 4k pedons
  site(hz_data) <- site_data

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
  site(hz_data) <- as.data.frame(best.tax.data)


  # load best-guess optimal records from ecositehistory
  # method is added to the new field called 'es_selection_method'
  ed.es <- data.table::as.data.table(extended_data$ecositehistory)
  best.ecosite.data <- ed.es[, .pickBestEcosite(.SD),
                               by = list(siteiid = ed.es$siteiid)]
  site(hz_data) <- as.data.frame(best.ecosite.data)

  ## TODO: NA in diagnostic boolean columns are related to pedons with no diagnostic features
  ## https://github.com/ncss-tech/soilDB/issues/59
  # add diagnostic boolean data into @site
  site(hz_data) <- extended_data$diagHzBoolean

  ## optionally convert NA fragvol to 0
  if(nullFragsAreZero) {
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

  ## TODO: convert this to simplifyFragmentData
  # add surface frag summary
  sfs <- extended_data$surf_frag_summary

  # optionally convert NA fragvol to 0
  if (nullFragsAreZero) {
    sfs <- as.data.frame(
      cbind(sfs[, 1, drop = FALSE],
            lapply(sfs[, -1], function(i) ifelse(is.na(i), 0, i))
      ), stringsAsFactors = FALSE)
  }

  # add surf. frag summary to @site
  site(hz_data) <- sfs

  # load diagnostic horizons into @diagnostic:
  # supress warnings: diagnostic_hz() <- is noisy when not all profiles have diagnostic hz data
  suppressWarnings(diagnostic_hz(hz_data) <- extended_data$diagnostic)

  # add restrictions to SPC
  # required new setter in aqp SPC object (AGB added 2019/12/23)
  suppressWarnings(restrictions(hz_data) <- extended_data$restriction)

  # join-in landform string w/ ampersand as separator for hierarchy
  # .formatLandformString <- soilDB:::.formatLandformString
  # .formatParentMaterialString <- soilDB:::.formatParentMaterialString
  ed.lf <- data.table::as.data.table(extended_data$geomorph)
  lf <- ed.lf[, .formatLandformString(.SD, uid = .BY$peiid, name.sep = ' & '),
              by = list(peiid = ed.lf$peiid)]

  if (ncol(lf) > 1)
    site(hz_data) <- as.data.frame(lf[,c("peiid","landform_string")])

  ed.pm <- data.table::as.data.table(extended_data$pm)
  pm <- ed.pm[, .formatParentMaterialString(.SD, uid = .BY$siteiid, name.sep = ' & '),
                by = list(siteiid = ed.pm$siteiid)]

  if (ncol(pm) > 2)
    site(hz_data) <- as.data.frame(pm[,c("siteiid","pmkind","pmorigin")])

# set metadata
  m <- metadata(hz_data)
  m$origin <- 'NASIS pedons'
  metadata(hz_data) <- m

  # print any messages on possible data quality problems:
  if (exists('sites.missing.pedons', envir = soilDB.env))
    if (length(get('sites.missing.pedons', envir = soilDB.env)) > 0)
      message("-> QC: sites without pedons: see `get('sites.missing.pedons', envir=soilDB.env)`")

  if (exists('dup.pedon.ids', envir = soilDB.env))
    if (length(get('dup.pedon.ids', envir = soilDB.env)) > 0)
      message("-> QC: duplicate pedons: see `get('dup.pedon.ids', envir=soilDB.env)`")

  # set NASIS-specific horizon identifier
  tryCatch(hzidname(hz_data) <- 'phiid', error = function(e) {
    if (grepl(e$message, pattern = "not unique$")) {
       if (!rmHzErrors) {
        # if rmHzErrors = FALSE, keep unique integer assigned ID to all records automatically
        message("-> QC: duplicated horizons found! defaulting to `hzID` as unique horizon ID.")
       } else {
         stop(e)
       }
    }
  })

  # set hz designation and texture fields -- NB: chose to use calculated texture -- more versatile
  # functions designed to use hztexclname() should handle presence of in-lieu, modifiers, etc.
  hzdesgnname(hz_data) <- "hzname"
  hztexclname(hz_data) <- "texture"

  if (exists('bad.pedon.ids', envir = soilDB.env))
    if (length(get('bad.pedon.ids', envir = soilDB.env)) > 0)
      message("-> QC: horizon errors detected, use `get('bad.pedon.ids', envir=soilDB.env)` for related userpedonid values or `get('bad.horizons', envir=soilDB.env)` for related horizon designations")

  if (exists('missing.bottom.depths', envir = soilDB.env))
    if (length(get('missing.bottom.depths', envir = soilDB.env)) > 0)
      message("-> QC: pedons missing bottom hz depths: use `get('missing.bottom.depths', envir=soilDB.env)` for related pedon IDs")

  if (exists('top.bottom.equal', envir = soilDB.env))
    if (length(get('top.bottom.equal', envir = soilDB.env)) > 0)
      message("-> QC: equal hz top and bottom depths: use `get('top.bottom.equal', envir=soilDB.env)` for related pedon IDs")

  ## https://github.com/ncss-tech/soilDB/issues/44
  # optionally load phlabresults table
  if (lab) {
    phlabresults <- .get_phlabresults_data_from_NASIS_db(SS = SS)
    horizons(hz_data) <- phlabresults
  }

  # done
  return(hz_data)
}
