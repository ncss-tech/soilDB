## TODO: better documentation for "fill" argument
# https://github.com/ncss-tech/soilDB/issues/50
## TODO: this will not ID horizons with no depths
## TODO: better error checking / reporting is needed: coiid, dmu id, component name
.fetchNASIS_components <- function(SS = TRUE,
                                   rmHzErrors = TRUE,
                                   nullFragsAreZero = TRUE,
                                   fill = FALSE,
                                   stringsAsFactors = default.stringsAsFactors(),
                                   dsn = NULL) {


  # ensure that any old hz errors are cleared
  if(exists('component.hz.problems', envir=soilDB.env))
    assign('component.hz.problems', value=character(0), envir=soilDB.env)

  # load data in pieces
  f.comp       <- get_component_data_from_NASIS_db(SS = SS, stringsAsFactors = stringsAsFactors, dsn = dsn, nullFragsAreZero = nullFragsAreZero)
  f.chorizon   <- get_component_horizon_data_from_NASIS_db(SS = SS, fill = fill, dsn = dsn, nullFragsAreZero = nullFragsAreZero)
  f.copm       <- get_component_copm_data_from_NASIS_db(SS = SS, stringsAsFactors = stringsAsFactors, dsn = dsn)
  f.cogeomorph <- get_component_cogeomorph_data_from_NASIS_db2(SS = SS, dsn = dsn)
  f.otherveg   <- get_component_otherveg_data_from_NASIS_db(SS = SS, dsn = dsn)
  f.ecosite    <- get_component_esd_data_from_NASIS_db(SS = SS, stringsAsFactors = stringsAsFactors, dsn = dsn)
  f.diaghz     <- get_component_diaghz_from_NASIS_db(SS = SS, dsn = dsn)
  f.restrict   <- get_component_restrictions_from_NASIS_db(SS = SS, dsn = dsn)

  filled.ids <- character(0)

  # optionally test for bad horizonation... flag, and remove
  if(rmHzErrors & nrow(f.chorizon) > 0) {
    f.chorizon.test <- aqp::checkHzDepthLogic(f.chorizon, c('hzdept_r', 'hzdepb_r'), idname = 'coiid', fast = TRUE)

    # fill=TRUE adds horizons with NA chiid will have NA depths -- will not pass hzDepthTests
    # therefore, only way to use fill effectively was with rmHzErrors=FALSE
    # which runs the risk of duplication in the case of data entry errors or other many:1 issues in comp
    filled.idx <- which(is.na(f.chorizon$chiid))
    if(length(filled.idx) > 0) {
      filled.ids <- as.character(f.chorizon$coiid[filled.idx])
      #print(dput(filled.ids))
    }

    # which are the good (valid) ones?
    good.ids <- as.character(f.chorizon.test$coiid[which(f.chorizon.test$valid)])
    bad.ids <- as.character(f.chorizon.test$coiid[which(!f.chorizon.test$valid)])

    if(length(filled.ids) > 0) {
      good.ids <- unique(c(good.ids, filled.ids))
      bad.ids <- unique(bad.ids[!bad.ids %in% filled.ids])
    }

    # keep the good ones
    f.chorizon <- f.chorizon[which(f.chorizon$coiid %in% good.ids), ]

    # keep track of those components with horizonation errors
    #if(length(bad.ids) > 0) # AGB removed this line of code b/c it prevents update of 'component.hz.problems' on subsequent error-free calls
    assign('component.hz.problems', value=bad.ids, envir=soilDB.env)
  }

  if(nrow(f.chorizon) > 0) {
    # upgrade to SoilProfilecollection
    depths(f.chorizon) <- coiid ~ hzdept_r + hzdepb_r
  } else {
    stop("No horizon data in NASIS component query result.", call.=FALSE)
  }

  # add site data to object
  site(f.chorizon) <- f.comp # left-join via coiid

  ## 2017-3-13: short-circuits need testing, consider pre-marking mistakes before parsing
  ## 2021-10-28: TODO: harmonize strategies for .formatXXXXString methods and ID variables
  .SD <- NULL
  .BY <- NULL
  
  # join-in copm strings
  pm <- data.table::data.table(f.copm)[, .formatParentMaterialString(.SD, uid = .BY$coiid, name.sep=' & '), by = "coiid"]
  pm$siteiid <- NULL
  if (nrow(pm) > 0)
    site(f.chorizon) <- pm

  # join-in cogeomorph strings
  lf <- data.table::data.table(f.cogeomorph)[, .formatLandformString(.SD, uid = .BY$coiid, name.sep=' & '), by = "coiid"]
  lf$peiid <- NULL
  if (nrow(lf) > 0)
    site(f.chorizon) <- lf

  # join-in ecosite string
  es <- data.table::data.table(f.ecosite)[, .formatEcositeString(.SD, name.sep=' & '), by = "coiid", .SDcols = colnames(f.ecosite)]
  es$coiid <- NULL
  if (nrow(es) > 0)
    site(f.chorizon) <- es

  # join-in othervegclass string
  ov <- data.table::data.table(f.otherveg)[, .formatOtherVegString(.SD, name.sep=' & '), by = "coiid", .SDcols = colnames(f.otherveg)]
  ov$coiid <- NULL
  if (nrow(ov) > 0)
    site(f.chorizon) <- ov

  # 2021-11-30: subset to hide aqp warnings for <- methods
  
  # add diagnostic features to SPC
  diagnostic_hz(f.chorizon) <- f.diaghz[which(f.diaghz$coiid %in% f.chorizon$coiid),]

  # add restrictions to SPC
  # required new setter in aqp SPC object (AGB added 2019/12/23)
  restrictions(f.chorizon) <- f.restrict[which(f.restrict$coiid %in% f.chorizon$coiid),]

  # print any messages on possible data quality problems:
  if(exists('component.hz.problems', envir=soilDB.env))
    if(length(get("component.hz.problems", envir = soilDB.env)) > 0)
      message("-> QC: horizon errors detected:\n\tUse `get('component.hz.problems', envir=soilDB.env)` for component record IDs (coiid)")

  # set NASIS component specific horizon identifier
  if(!fill & length(filled.ids) == 0) {
    res <- try(hzidname(f.chorizon) <- 'chiid')
    if(inherits(res, 'try-error')) {
      if(!rmHzErrors) {
        warning("cannot set `chiid` as unique component horizon key -- duplicate horizons present with rmHzErrors=FALSE")
      } else {
        warning("cannot set `chiid` as unique component horizon key -- defaulting to `hzID`")
      }
    }
  } else {
    warning("cannot set `chiid` as unique component horizon key - `NA` introduced by fill=TRUE", call.=F)
  }

  # set metadata
  m <- metadata(f.chorizon)
  m$origin <- 'NASIS components'
  metadata(f.chorizon) <- m

  # set optional hz designation and texture slots
  hzdesgnname(f.chorizon) <- "hzname"
  hztexclname(f.chorizon) <- "texture"

  # done, return SPC
  return(f.chorizon)

}
