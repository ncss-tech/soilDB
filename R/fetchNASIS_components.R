## TODO: better documentation for "fill" argument
# https://github.com/ncss-tech/soilDB/issues/50
## TODO: this will not ID horizons with no depths
## TODO: better error checking / reporting is needed: coiid, dmu id, component name
.fetchNASIS_components <- function(SS = TRUE,
                                   rmHzErrors = FALSE,
                                   nullFragsAreZero = TRUE,
                                   fill = FALSE,
                                   stringsAsFactors = NULL,
                                   dsn = NULL,
                                   dropAdditional = TRUE,
                                   dropNotRepresentative = TRUE,
                                   duplicates = FALSE) {

  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }
  
  
  # check if NASIS local DB instance/ODBC data source is available
  .soilDB_test_NASIS_connection(dsn = dsn)
  
  if (!requireNamespace("aqp")) {
    stop("package 'aqp' is required", call. = FALSE)
  }
  
  # ensure that any old hz errors are cleared
  if (exists('component.hz.problems', envir = get_soilDB_env())) {
    assign('component.hz.problems', value = character(0), envir = get_soilDB_env())
  }
  
  # optionally legend and mapunit information are included if in local DB/selected set
  #   includes possible results for rep and non-rep DMUs and any mustatus
  if (duplicates) {
    f.corr       <- get_component_correlation_data_from_NASIS_db(SS = SS, dsn = dsn, dropAdditional = dropAdditional, dropNotRepresentative = dropNotRepresentative)
  }
  # load data in pieces
  f.comp       <- get_component_data_from_NASIS_db(SS = SS, dsn = dsn, nullFragsAreZero = nullFragsAreZero)
  f.chorizon   <- get_component_horizon_data_from_NASIS_db(SS = SS, fill = fill, dsn = dsn, nullFragsAreZero = nullFragsAreZero)
  f.copm       <- get_component_copm_data_from_NASIS_db(SS = SS, dsn = dsn)
  f.cogeomorph <- get_component_cogeomorph_data_from_NASIS_db2(SS = SS, dsn = dsn)
  f.otherveg   <- get_component_otherveg_data_from_NASIS_db(SS = SS, dsn = dsn)
  f.ecosite    <- get_component_esd_data_from_NASIS_db(SS = SS, dsn = dsn)
  f.diaghz     <- get_component_diaghz_from_NASIS_db(SS = SS, dsn = dsn)
  f.restrict   <- get_component_restrictions_from_NASIS_db(SS = SS, dsn = dsn)

  filled.ids <- character(0)

  # optionally test for bad horizonation... flag, and remove
  if (nrow(f.chorizon) > 0) {
    f.chorizon.test <- aqp::checkHzDepthLogic(f.chorizon, c('hzdept_r', 'hzdepb_r'), idname = 'coiid', fast = TRUE)

    # fill=TRUE adds horizons with NA chiid will have NA depths -- will not pass hzDepthTests
    # therefore, only way to use fill effectively was with rmHzErrors=FALSE
    # which runs the risk of duplication in the case of data entry errors or other many:1 issues in comp
    filled.idx <- which(is.na(f.chorizon$chiid))
    if (length(filled.idx) > 0) {
      filled.ids <- as.character(f.chorizon$coiid[filled.idx])
    }

    # which are the good (valid) ones?
    good.ids <- as.character(f.chorizon.test$coiid[which(f.chorizon.test$valid)])
    bad.ids <- as.character(f.chorizon.test$coiid[which(!f.chorizon.test$valid)])

    if (length(filled.ids) > 0) {
      good.ids <- unique(c(good.ids, filled.ids))
      bad.ids <- unique(bad.ids[!bad.ids %in% filled.ids])
    }

    if (rmHzErrors) {
      # keep the good ones
      f.chorizon <- f.chorizon[which(f.chorizon$coiid %in% good.ids), ]
    }
    
    # keep track of those components with horizonation errors
    #if(length(bad.ids) > 0) # AGB removed this line of code b/c it prevents update of 'component.hz.problems' on subsequent error-free calls
    assign('component.hz.problems', value = bad.ids, envir = get_soilDB_env())
  }

  # diagnostics and restrictions
  # 2021-11-30: subset to hide aqp warnings for <- methods
  f.diaghz2 <- f.diaghz[which(f.diaghz$coiid %in% f.chorizon$coiid),]
  f.restrict2 <- f.restrict[which(f.restrict$coiid %in% f.chorizon$coiid),]
  
  if (nrow(f.chorizon) > 0) {
    if (duplicates) {
      f.chorizon <- merge(f.chorizon, f.comp[,c("coiid","dmuiid")], by = "coiid", all.x = TRUE, all.y = TRUE, sort = FALSE)
      f.chorizon <- merge(f.corr[,c("dmuiid","muiid","lmapunitiid")], f.chorizon, all.y = TRUE, by = "dmuiid", sort = FALSE)
      f.chorizon$coiidcmb <- paste0(f.chorizon$lmapunitiid, ":", f.chorizon$muiid, ":", f.chorizon$dmuiid, ":", f.chorizon$coiid)
      
      f.diaghz2 <- merge(f.diaghz2, f.comp[,c("coiid","dmuiid")], by = "coiid", all.x = TRUE, all.y = TRUE, sort = FALSE)
      f.diaghz2 <- merge(f.corr[,c("dmuiid","muiid","lmapunitiid")], f.diaghz2, all.y = TRUE, by = "dmuiid", sort = FALSE)
      f.diaghz2$coiidcmb <- paste0(f.diaghz2$lmapunitiid, ":", f.diaghz2$muiid, ":", f.diaghz2$dmuiid, ":", f.diaghz2$coiid)
      
      f.restrict2 <- merge(f.restrict2, f.comp[,c("coiid","dmuiid")], by = "coiid", all.x = TRUE, all.y = TRUE, sort = FALSE)
      f.restrict2 <- merge(f.corr[,c("dmuiid","muiid","lmapunitiid")], f.restrict2, all.y = TRUE, by = "dmuiid", sort = FALSE)
      f.restrict2$coiidcmb <- paste0(f.restrict2$lmapunitiid, ":", f.restrict2$muiid, ":", f.restrict2$dmuiid, ":", f.restrict2$coiid)
    }
    
    if (duplicates) {
      # use combined coiid (lmapunitiid, muiid, dmuiid, coiid) under name coiidcmb
      aqp::depths(f.chorizon) <- coiidcmb ~ hzdept_r + hzdepb_r
      aqp::site(f.chorizon) <- ~ dmuiid + muiid + lmapunitiid + coiid
    } else {
      # upgrade to SoilProfilecollection
      aqp::depths(f.chorizon) <- coiid ~ hzdept_r + hzdepb_r
    }
    
  } else {
    ds <- ifelse(SS, "NASIS selected set", "NASIS local database")
    stop("No component/horizon records in ", ds, call. = FALSE)
  }

  # add site data to object
  aqp::site(f.chorizon) <- f.comp # left-join via coiid
  
  if (duplicates && !is.null(f.corr) && nrow(f.corr) > 0) {
    aqp::site(f.chorizon) <- f.corr # left-join via dmuiid, muiid, lmapunitiid
  }
  
  # add diagnostic features and restrictions to SPC
  aqp::diagnostic_hz(f.chorizon) <- f.diaghz2
  aqp::restrictions(f.chorizon) <- f.restrict2
  
  ## 2017-3-13: short-circuits need testing, consider pre-marking mistakes before parsing
  ## 2021-10-28: TODO: harmonize strategies for .formatXXXXString methods and ID variables
  .SD <- NULL
  .BY <- NULL
  
  # join-in copm strings
  pm <- data.table::data.table(f.copm)[, .formatParentMaterialString(.SD, uid = .BY$coiid, name.sep = ' & '), by = "coiid"]
  pm$siteiid <- NULL
  if (nrow(pm) > 0) {
    aqp::site(f.chorizon) <- pm
  }
  
  # join-in cogeomorph strings
  lf <- data.table::data.table(f.cogeomorph)[, .formatLandformString(.SD, uid = .BY$coiid, name.sep = ' & '), by = "coiid"]
  lf$peiid <- NULL
  if (nrow(lf) > 0) {
    aqp::site(f.chorizon) <- lf
  }
  
  .soilDB_warn_deprecated_aliases(c("ecositeid" = "ecosite_id",
                                    "ecositenm" = "ecosite_name",
                                    "ovegclid" = "othervegid",
                                    "ovegclname" = "othervegclass"))
  
  # join-in ecosite string
  es <- data.table::data.table(f.ecosite)[, .formatEcositeString(.SD, name.sep = ' & '), by = "coiid", .SDcols = colnames(f.ecosite)]
  es$coiid <- NULL
  es$ecosite_id <- es$ecositeid
  es$ecosite_name <- es$ecositenm
  if (nrow(es) > 0) {
    aqp::site(f.chorizon) <- es
  }
  
  # join-in othervegclass string
  ov <- data.table::data.table(f.otherveg)[, .formatOtherVegString(.SD, name.sep = ' & '), by = "coiid", .SDcols = colnames(f.otherveg)]
  ov$coiid <- NULL 
  ov$othervegid <- ov$ovegclid
  ov$othervegclass <- ov$ovegclname
  if (nrow(ov) > 0) {
    aqp::site(f.chorizon) <- ov
  }

  # print any messages on possible data quality problems:
  if (exists('component.hz.problems', envir = get_soilDB_env()) &&
      length(get("component.hz.problems", envir = get_soilDB_env())) > 0) {
    message(
      "-> QC: horizon errors detected:\n\tUse `get('component.hz.problems', envir=get_soilDB_env())` for component record IDs (coiid)"
    )
  }
  
  # set NASIS component specific horizon identifier
  if (!fill & length(filled.ids) == 0) {
    res <- try(aqp::hzidname(f.chorizon) <- 'chiid')
    if (inherits(res, 'try-error')) {
      if (!rmHzErrors) {
        warning("cannot set `chiid` as unique component horizon key -- duplicate horizons present with rmHzErrors=FALSE", call. = FALSE)
      } else {
        warning("cannot set `chiid` as unique component horizon key -- defaulting to `hzID`", call. = FALSE)
      }
    }
  } else {
    warning("cannot set `chiid` as unique component horizon key - `NA` introduced by fill=TRUE", call. = FALSE)
  }

  # set metadata
  m <- aqp::metadata(f.chorizon)
  m$origin <- 'NASIS components'
  m$created <- Sys.time()
  aqp::metadata(f.chorizon) <- m

  # set optional hz designation and texture slots
  aqp::hzdesgnname(f.chorizon) <- "hzname"
  aqp::hztexclname(f.chorizon) <- "texture"

  if (duplicates && dropNotRepresentative) {
    f.chorizon <- f.chorizon[which(!is.na(f.chorizon$repdmu)), ]
  }
  
  if (duplicates && dropAdditional) {
    f.chorizon <- f.chorizon[which(f.chorizon$mustatus != "additional"), ]
  }
  
  # done, return SPC
  return(f.chorizon)

}
