##
## December, 2017
## D.E. Beaudette
## J.M. Skovlin
## S.M. Roecker
##

#' Get component data from a local NASIS Database
#'
#' @param SS fetch data from the currently loaded selected set in NASIS or from
#' the entire local database (default: `TRUE`)
#' @param nullFragsAreZero should surface fragment cover percentages of NULL be interpreted as 0? (default: TRUE)
#' @param stringsAsFactors logical: should character vectors be converted to
#' factors? This argument is passed to the `uncode()` function. It does not
#' convert those vectors that have set outside of `uncode()` (i.e. hard coded).
#' The 'factory-fresh' default is TRUE, but this can be changed by setting
#' options(`stringsAsFactors = FALSE`)
#'
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: `NULL`
#'
#' @return a `data.frame`
#' @author Dylan E. Beaudette, Stephen Roecker, and Jay M. Skovlin
#' @seealso \code{\link{fetchNASIS}}
#' @keywords manip
#' @examples
#'
#' \donttest{
#' if(local_NASIS_defined()) {
#'  # query text note data
#'  fc <- try(get_component_data_from_NASIS_db())
#'
#'  # show structure of component data returned
#'  str(fc)
#' }
#' }
#'
#' @export get_component_data_from_NASIS_db
get_component_data_from_NASIS_db <- function(SS = TRUE,
                                             nullFragsAreZero = TRUE,
                                             stringsAsFactors = default.stringsAsFactors(),
                                             dsn = NULL) {
  
  q1 <- "SELECT dmudesc, compname, comppct_r, compkind, majcompflag, localphase, drainagecl, hydricrating, elev_l, elev_r, elev_h, slope_l, slope_r, slope_h, aspectccwise, aspectrep, aspectcwise, map_l, map_r, map_h, airtempa_l as maat_l, airtempa_r as maat_r, airtempa_h as maat_h, soiltempa_r as mast_r, reannualprecip_r, ffd_l, ffd_r, ffd_h, tfact, wei, weg, nirrcapcl, nirrcapscl, nirrcapunit, irrcapcl, irrcapscl, irrcapunit, frostact, hydricrating, hydgrp, corcon, corsteel, taxclname, taxorder, taxsuborder, taxgrtgroup, taxsubgrp, taxpartsize, taxpartsizemod, taxceactcl, taxreaction, taxtempcl, taxmoistscl, taxtempregime, soiltaxedition, coiid, dmuiid

  FROM
  datamapunit_View_1 AS dmu
  INNER JOIN component_View_1 AS co ON co.dmuiidref = dmu.dmuiid

  ORDER BY dmudesc, comppct_r DESC, compname ASC;"
  
  q2 <- "SELECT * FROM cosurffrags_View_1"
  
  channel <- dbConnectNASIS(dsn)
  
  if (inherits(channel, 'try-error'))
    return(data.frame())
  
  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q1 <- gsub(pattern = '_View_1', replacement = '', x = q1, fixed = TRUE)
    q2 <- gsub(pattern = '_View_1', replacement = '', x = q2, fixed = TRUE)
  }
  
  # exec query
  d <- dbQueryNASIS(channel, q1, close = FALSE)
  
  # test for duplicate coiids
  idx <- which(table(d$coiid) > 1)
  if (length(idx) > 0) {
    dupes <- names(idx)
    assign('dupe.coiids', value=dupes, envir=soilDB.env)
    message("-> QC: duplicate coiids, this should not happen.\n\tUse `get('dupe.coiids', envir=soilDB.env)` for component record IDs (coiid)")
  }
  
  # uncode metadata domains
  if (nrow(d) > 0) {
    d <- uncode(d, stringsAsFactors = stringsAsFactors, dsn = dsn)
  }
  
  # surface fragments
  chs <- simplifyFragmentData(
    uncode(dbQueryNASIS(channel, q2, close = FALSE), dsn = dsn),
    id.var = "coiidref",
    vol.var = "sfragcov_r",
    prefix = "sfrag",
    msg = "surface fragment cover")
  
  if (sum(complete.cases(chs)) == 0) {
    chs <- chs[1:nrow(d),]
    chs$coiidref <- d$coiid
  } else {
    ldx <- !d$coiid %in% chs$coiidref
    chs_null <- chs[0,][1:sum(ldx),]
    chs_null$coiidref <- d$coiid[ldx]
    chs <- rbind(chs, chs_null)
    
    # handle NA for totals
    if (nullFragsAreZero) {
      chs[is.na(chs)] <- 0
    } 
    colnames(chs) <- paste0("surface_", colnames(chs))
    colnames(chs)[1] <- "coiidref"
    d <- merge(d, chs, by.x = "coiid", by.y = "coiidref", all.x = TRUE, sort = FALSE)
  }
  
  # done
  return(d)
}


## component diagnostic features
#' @export
#' @rdname get_component_data_from_NASIS_db
get_component_diaghz_from_NASIS_db <- function(SS = TRUE, dsn = NULL) {

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # query diagnostic horizons, usually a 1:many relationship with pedons
  q <- "SELECT coiidref as coiid, featkind, featdept_l, featdept_r, featdept_h, featdepb_l, featdepb_r, featdepb_h, featthick_l, featthick_r, featthick_h FROM codiagfeatures_View_1 AS cdf ORDER BY cdf.coiidref, cdf.featdept_r;"

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # exec query
  d <- dbQueryNASIS(channel, q)

  # convert codes
  d <- uncode(d, dsn = dsn)

}

## component diagnostic features
#' @export
#' @rdname get_component_data_from_NASIS_db
get_component_restrictions_from_NASIS_db <- function(SS = TRUE, dsn = NULL) {

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # query restrictions, can be 1:many relationship with pedons
  q <- "SELECT coiidref as coiid, reskind, resdept_l, resdept_r, resdept_h, resdepb_l, resdepb_r, resdepb_h, resthk_l, resthk_r, resthk_h, reskind, reshard FROM corestrictions_View_1 AS cr ORDER BY cr.coiidref, cr.resdept_r;"

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # exec query
  d <- dbQueryNASIS(channel, q)

  # convert codes
  return(uncode(d, dsn = dsn)
)
}

# return all rows from correlation -- map unit -- legend map unit -- dmu / legend -- area
# note that all of these "target tables" have to be selected
#' @export
#' @param dropAdditional Remove map units with "additional" status? Default: `TRUE`
#' @param dropNotRepresentative Remove non-representative data map units? Default: `TRUE`
#' @rdname get_component_data_from_NASIS_db
get_component_correlation_data_from_NASIS_db <- function(SS = TRUE,
                                                         dropAdditional = TRUE,
                                                         dropNotRepresentative = TRUE,
                                                         stringsAsFactors = default.stringsAsFactors(),
                                                         dsn = NULL) {
  
  q <- "SELECT lmapunitiid, mu.muiid, musym, nationalmusym, mu.muname, mukind, mutype, mustatus, muacres, farmlndcl, repdmu, dmuiid, areasymbol, areaname, ssastatus, cordate

  FROM  mapunit_View_1 AS mu

  LEFT OUTER JOIN correlation_View_1 AS cor ON cor.muiidref = mu.muiid
  LEFT OUTER JOIN datamapunit_View_1 AS dmu ON cor.dmuiidref = dmu.dmuiid
  LEFT OUTER JOIN lmapunit_View_1 AS lm ON lm.muiidref = mu.muiid
  LEFT OUTER JOIN legend_View_1 AS leg ON leg.liid = lm.liidref
  LEFT OUTER JOIN area_View_1 AS a ON a.areaiid = leg.areaiidref

  ORDER BY nationalmusym, dmuiid;"

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  d <- dbQueryNASIS(channel, q)

  ## TODO: is this a good idea?
  # test for no data
  if(nrow(d) == 0)
    warning('there are no records in your selected set!', call. = FALSE)

  # recode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors, dsn = dsn)

  # optionally drop additional | NA mustatus
  if(dropAdditional) {
    idx <- which(d$mustatus == 'additional')
    if(length(idx) > 0) {
      d <- d[-idx, ]
    }
  }

  # optionally drop not-representative
  if(dropNotRepresentative) {
    d <- d[which(d$repdmu == 1), ]
  }

  # check for non-unique MUs
  idx <- which(table(d$muiid) > 1)
  if(length(idx) > 0) {
    dupes <- names(idx)
    assign('dupe.muiids', value=dupes, envir=soilDB.env)
    message("-> QC: duplicate muiids: multiple 'representative' DMU / MU?.\n\tUse `get('dupe.muiids', envir=soilDB.env)` for mapunit record IDs (muiid)")
  }


  # check for multiple DMUs:
  idx <- which(table(d$dmuiid) > 1)
  if(length(idx) > 0) {
    dupes <- names(idx)
    assign('multiple.mu.per.dmu', value=dupes, envir=soilDB.env)
    message("-> QC: DMUs assigned to multiple MU.\n\tUse `get('multiple.mu.per.dmu', envir=soilDB.env)` for data mapunit record IDs (dmuiid)")
  }


  # done
  return(d)
}

# get geomorphic desc for each component
#' @export
#' @rdname get_component_data_from_NASIS_db
get_component_cogeomorph_data_from_NASIS_db <- function(SS = TRUE, dsn = NULL) {

  q <- "SELECT cogeo.coiidref as coiid, cogeo.geomfmod, geomorfeat.geomfname, cogeo.geomfeatid, cogeo.existsonfeat, cogeo.geomfiidref, lower(geomorfeattype.geomftname) as geomftname

  FROM
  component_View_1 AS co
  INNER JOIN cogeomordesc_View_1 AS cogeo ON co.coiid = cogeo.coiidref
  INNER JOIN geomorfeat ON geomorfeat.geomfiid = cogeo.geomfiidref
  INNER JOIN geomorfeattype ON geomorfeattype.geomftiid = geomorfeat.geomftiidref

  ORDER BY coiid, geomfeatid ASC;"

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  d <- dbQueryNASIS(channel, q)

  # done
  return(d)
}

# get geomorphic desc for each component
#' @export
#' @rdname get_component_data_from_NASIS_db
get_component_cogeomorph_data_from_NASIS_db2 <- function(SS = TRUE, dsn = NULL) {
  
  q <- "SELECT cogeo.coiidref as coiid, cogeo.geomfmod, geomorfeat.geomfname, cogeo.geomfeatid, cogeo.existsonfeat, cogeo.geomfiidref, lower(geomorfeattype.geomftname) as geomftname, cogeo.rvindicator AS cogeomordescrv, hillslopeprof, cosurfmorphhpp.rvindicator AS cosurfmorphhpprv, geomposmntn, geomposhill, geompostrce, geomposflats, shapeacross, shapedown, geomicrorelief

  FROM
  component_View_1 AS co
  INNER JOIN cogeomordesc_View_1 AS cogeo ON co.coiid = cogeo.coiidref
  INNER JOIN geomorfeat ON geomorfeat.geomfiid = cogeo.geomfiidref
  INNER JOIN geomorfeattype ON geomorfeattype.geomftiid = geomorfeat.geomftiidref
  LEFT JOIN cosurfmorphhpp ON cosurfmorphhpp.cogeomdiidref = cogeo.cogeomdiid
  LEFT JOIN cosurfmorphgc ON cosurfmorphgc.cogeomdiidref = cogeo.cogeomdiid
  LEFT JOIN cosurfmorphmr ON cosurfmorphmr.cogeomdiidref = cogeo.cogeomdiid
  LEFT JOIN cosurfmorphss ON cosurfmorphss.cogeomdiidref = cogeo.cogeomdiid
  ORDER BY coiid, geomfeatid ASC;"
  
  channel <- dbConnectNASIS(dsn)
  
  if (inherits(channel, 'try-error'))
    return(data.frame())
  
  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }
  
  d <- dbQueryNASIS(channel, q)
  
  # done
  return(uncode(d))
}

# get copm for each component
#' @export
#' @rdname get_component_data_from_NASIS_db
get_component_copm_data_from_NASIS_db <- function(SS = TRUE,
                                                  stringsAsFactors = default.stringsAsFactors(),
                                                  dsn = NULL) {
  

  q <- "SELECT cpmg.coiidref as coiid, cpm.seqnum as seqnum, pmorder, pmdept_r, pmdepb_r, pmmodifier, pmgenmod, pmkind, pmorigin

  FROM
  component_View_1 AS co
  INNER JOIN copmgrp_View_1 AS cpmg ON cpmg.coiidref = co.coiid
  INNER JOIN copm_View_1 AS cpm ON cpm.copmgrpiidref = cpmg.copmgrpiid

  ORDER BY coiidref, seqnum, pmorder, copmgrpiid ASC;"

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  d <- dbQueryNASIS(channel, q)

  # uncode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors, dsn = dsn)

  # done
  return(d)
}

# get ESD information for each component
#' @export
#' @rdname get_component_data_from_NASIS_db
get_component_esd_data_from_NASIS_db <- function(SS = TRUE,
                                                 stringsAsFactors = default.stringsAsFactors(),
                                                 dsn = NULL) {
  
  q <- "SELECT coiidref as coiid, ecositeid, ecositenm,
  ecositeorigin, ecositetype, ecositemlra, ecositelru, ecositenumber, ecositestate

  FROM coecosite_View_1 AS coecosite

  INNER JOIN ecologicalsite AS es ON es.ecositeiid = coecosite.ecositeiidref

  ORDER BY coiid;"

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  d <- dbQueryNASIS(channel, q)

  # check for more than 1 record / coiid
  idx <- which(table(d$coiid) > 1)
  dupes <- names(idx)
  assign('multiple.ecosite.per.coiid', value=dupes, envir=soilDB.env)
  if (length(idx) > 0) {
    message("-> QC: multiple ecosites / component.\n\tUse `get('multiple.ecosite.per.coiid', envir=soilDB.env)` for component record IDs (coiid)")
  }

  # uncode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors, dsn = dsn)

  # done
  return(d)
}

## TODO: convert any multiple entries into a comma delimited string
# get OtherVeg information for each component
#' @export
#' @rdname get_component_data_from_NASIS_db
get_component_otherveg_data_from_NASIS_db <- function(SS = TRUE, dsn = NULL) {
  

  q <- "SELECT coiidref as coiid, ovegclid, ovegclname, coothvegcl.recwlupdated
  FROM coothvegclass_View_1 coothvegcl
  INNER JOIN othvegclass as ovc ON ovc.ovegcliid = coothvegcl.ovegcliidref
  ORDER BY coiid;"

  # setup connection local NASIS
  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  d <- dbQueryNASIS(channel, q)

  # check for more than 1 record / coiid
  idx <- which(table(d$coiid) > 1)
  if (length(idx) > 0) {
    dupes <- names(idx)
    assign('multiple.otherveg.per.coiid', value=dupes, envir=soilDB.env)
    message("-> QC: multiple othervegclasses / component.\n\tUse `get('multiple.otherveg.per.coiid', envir=soilDB.env)` for component record IDs (coiid)")
  }

  # uncode metadata domains
  #d <- uncode(d, dsn = dsn)

  # done
  return(d)
}



#' Get component month data from a local NASIS Database
#'
#' Get component month data from a local NASIS Database.
#'
#' @param SS get data from the currently loaded Selected Set in NASIS or from
#' the entire local database (default: TRUE)
#' @param fill should missing "month" rows in the comonth table be filled with
#' NA (FALSE)
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: `NULL`
#' @param stringsAsFactors logical: should character vectors be converted to
#' factors? This argument is passed to the uncode() function. It does not
#' convert those vectors that have set outside of uncode() (i.e. hard coded).
#' The 'factory-fresh' default is TRUE, but this can be changed by setting
#' options(stringsAsFactors = FALSE)
#' @return A list with the results.
#' @author Stephen Roecker
#' @seealso \code{\link{fetchNASIS}}
#' @keywords manip
#' @examples
#'
#' \donttest{
#' if(local_NASIS_defined()) {
#'   # query text note data
#'   cm <- try(get_comonth_from_NASIS_db())
#'
#'   # show structure of component month data
#'   str(cm)
#' }
#' }
#'
#' @export get_comonth_from_NASIS_db
get_comonth_from_NASIS_db <- function(SS = TRUE,
                                      fill = FALSE,
                                      stringsAsFactors = default.stringsAsFactors(),
                                      dsn = NULL) {
  

  q <- "SELECT coiidref AS coiid, month, flodfreqcl, floddurcl, pondfreqcl, ponddurcl, ponddep_l, ponddep_r, ponddep_h, dlyavgprecip_l, dlyavgprecip_r, dlyavgprecip_h, comonthiid
  FROM comonth_View_1 AS comonth;"

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # exec query
  d <- dbQueryNASIS(channel, q)

  # uncode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors, dsn = dsn)

  # optionally fill missing coiids
  if (fill) {
    q <- "SELECT coiid
    FROM component_View_1
    ORDER BY coiid;"

    channel <- dbConnectNASIS(dsn)

    if (inherits(channel, 'try-error'))
      return(data.frame())

    # toggle selected set vs. local DB
    if (SS == FALSE) {
      q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
    }

    # exec query
    d.coiid <- dbQueryNASIS(channel, q)
  }

  # re-format month names
  # only if > 0 rows of data
  if (nrow(d) > 0) {
    # using 3-letter month names
    d$month <- months(as.Date(paste0("2016-", d$month, "-01"), format="%Y-%B-%d"), abbreviate = TRUE)
  }

  ## these degrade gracefully when comonth data are missing
  # calendar order
  d$month <- factor(d$month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

  # fix other factor levels
  d$flodfreqcl <- factor(d$flodfreqcl, levels=c('none', 'very rare', 'rare', 'occasional', 'common', 'frequent', 'very frequent'))
  d$floddurcl <- factor(d$floddurcl, levels=c('extremely brief', 'very brief', 'brief', 'long', 'very long'), labels=c('Extremely brief (0.1 to 4 hours)', 'Very brief (4 to 48 hours)', 'Brief (2 to 7 days)', 'Long (7 to 30 days)', 'Very long (more than 30 days)'))

  d$pondfreqcl <- factor(d$pondfreqcl, levels=c('none', 'rare', 'occasional', 'common', 'frequent'))
  d$ponddurcl <- factor(d$ponddurcl, levels=c('very brief', 'brief', 'long', 'very long'), labels=c('Very brief (4 to 48 hours)', 'Brief (2 to 7 days)', 'Long (7 to 30 days)', 'Very long (more than 30 days)'))


  # optionally fill missing coiids
  if (fill) {
    # make a new DF with all coiids and months
    nd <- expand.grid(coiid = d.coiid$coiid, month = levels(d$month))
    nd$month <- factor(nd$month, levels = levels(d$month))

    # join full version to comonth records
    # nd contains the full set of component records IDs
    d <- merge(nd, d, by=c('coiid', 'month'), all.x = TRUE, sort = FALSE)

    ## this isn't likely needed, will re-visit after some testing

    # # add "not-populated" to rows that have been filled
    # levels(d$flodfreqcl) <- c("not populated", levels(d$flodfreqcl))
    # d$flodfreqcl[is.na(d$flodfreqcl)] <- 'not populated'
    #
    # levels(d$floddurcl) <- c("not populated", levels(d$floddurcl))
    # d$floddurcl[is.na(d$floddurcl)] <- 'not populated'
    #
    # levels(d$pondfreqcl) <- c("not populated", levels(d$pondfreqcl))
    # d$pondfreqcl[is.na(d$pondfreqcl)] <- 'not populated'
    #
    # levels(d$ponddurcl) <- c("not populated", levels(d$ponddurcl))
    # d$ponddurcl[is.na(d$ponddurcl)] <- 'not populated'
  }

  # re-order by coiid, then months
  d <- d[order(d$coiid, as.numeric(d$month)), ]

  # done
  return(d)
}





# get linked pedons by peiid and user pedon ID
# note that there may be >=1 pedons / coiid
#' @export
#' @rdname get_component_data_from_NASIS_db
get_copedon_from_NASIS_db <- function(SS = TRUE, dsn = NULL) {
  
  q <- "SELECT coiidref as coiid, peiidref as peiid, upedonid as pedon_id, rvindicator as representative

  FROM copedon_View_1 copedon

  LEFT OUTER JOIN pedon_View_1 p ON p.peiid = copedon.peiidref;
  "

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # exec query
  d <- dbQueryNASIS(channel, q)

  # missing pedon ID suggests records not in the selected set or local database
  if(nrow(d) > 0 & any(is.na(d$pedon_id))) {
    message('some linked pedons not in selected set or local database')
  }

  # done
  return(d)
}

#' @export
#' @param fill Return a single minimal (NA-filled) horizon for components with no horizon records? Default `FALSE`
#' @rdname get_component_data_from_NASIS_db
get_component_horizon_data_from_NASIS_db <- function(SS = TRUE,
                                                     fill = FALSE,
                                                     dsn = NULL,
                                                     nullFragsAreZero = TRUE) {

  q <- "SELECT coiid, chiid, hzname, hzdept_r, hzdepb_r, texture, fragvoltot_l, fragvoltot_r, fragvoltot_h, sandtotal_l, sandtotal_r, sandtotal_h, silttotal_l, silttotal_r, silttotal_h, claytotal_l, claytotal_r, claytotal_h, om_l, om_r, om_h, structgrpname, dbthirdbar_l, dbthirdbar_r, dbthirdbar_h, ksat_l, ksat_r, ksat_h, awc_l, awc_r, awc_h, lep_l, lep_r, lep_h, ll_l, ll_r, ll_h, pi_l, pi_r, pi_h, sieveno4_l, sieveno4_r, sieveno4_h, sieveno10_l, sieveno10_r, sieveno10_h, sieveno40_l, sieveno40_r, sieveno40_h, sieveno200_l, sieveno200_r, sieveno200_h, sar_l, sar_r, sar_h, ec_l, ec_r, ec_h, cec7_l, cec7_r, cec7_h, sumbases_l, sumbases_r, sumbases_h, ecec_l, ecec_r, ecec_h, ph1to1h2o_l, ph1to1h2o_r, ph1to1h2o_h, ph01mcacl2_l, ph01mcacl2_r, ph01mcacl2_h, caco3_l, caco3_r, caco3_h, kffact, kwfact, aashind_l, aashind_r, aashind_h

  FROM
  component_View_1 co
  LEFT OUTER JOIN chorizon_View_1 ch ON ch.coiidref = co.coiid
  LEFT OUTER JOIN chtexturegrp_View_1 cht ON cht.chiidref = ch.chiid AND cht.rvindicator = 1
  LEFT OUTER JOIN chstructgrp_View_1 chs ON chs.chiidref = ch.chiid AND chs.rvindicator = 1

  -- why is this here?
  INNER JOIN datamapunit_View_1 dmu ON dmu.dmuiid = co.dmuiidref

  ORDER BY dmudesc, comppct_r DESC, compname ASC, hzdept_r ASC;"
  
  q2 <- "SELECT * FROM chfrags_View_1"
  q3 <- "SELECT * FROM chhuarts_View_1"
  
  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
    q2 <- gsub(pattern = '_View_1', replacement = '', x = q2, fixed = TRUE)
    q3 <- gsub(pattern = '_View_1', replacement = '', x = q3, fixed = TRUE)
  }

  # exec query
  d <- dbQueryNASIS(channel, q, close = FALSE)

  ## TODO: better documentation for "fill" argument
  # https://github.com/ncss-tech/soilDB/issues/50
  # remove records what are missing horizon data
  if (fill == FALSE) {
    d <- d[!is.na(d$chiid), ]
  }
  
  # "sieving" chfrags, chuarts tables for parity with fetchNASIS("pedons") @horizons slot columns 
  
  if (nrow(d) > 0){
    # horizon fragments
    chf <- simplifyFragmentData(
      uncode(dbQueryNASIS(channel, q2, close = FALSE), dsn = dsn),
      id.var = "chiidref",
      vol.var = "fragvol_r",
      nullFragsAreZero = nullFragsAreZero
    )
    if (sum(complete.cases(chf)) == 0) {
      chf <- chf[1:nrow(d),]
      chf$chiidref <- d$chiid
    } else {
      ldx <- !d$chiid %in% chf$chiidref
      chf_null <- chf[0,]
      if (any(ldx)) {
        chf_null <- chf_null[seq(sum(ldx)),]
      }
      chf_null$chiidref <- d$chiid[ldx]
      chf <- rbind(chf, chf_null)
    }
    # handle NA for totals
    if (nullFragsAreZero) {
      chf[is.na(chf)] <- 0
    } 
  
    # human artifacts  
    cha <- simplifyArtifactData(
      uncode(dbQueryNASIS(channel, q3, close = FALSE), dsn = dsn),
      id.var = "chiidref",
      vol.var = "huartvol_r",
      nullFragsAreZero = nullFragsAreZero
    )
    # handle NULL result
    if (sum(complete.cases(cha)) == 0) {
      cha <- cha[1:nrow(d),]
      cha$chiidref <- d$chiid
    } else {
      ldx <- !d$chiid %in% cha$chiidref
      cha_null <- cha[0,][1:sum(ldx),]
      cha_null$chiidref <- d$chiid[ldx]
      cha <- rbind(cha, cha_null)
    }
    # handle NA for totals
    if (nullFragsAreZero) {
      cha[is.na(cha)] <- 0
    }
    
    
    d <- merge(d, chf, by.x = "chiid", by.y = "chiidref", all.x = TRUE, sort = FALSE)
    d <- merge(d, cha, by.x = "chiid", by.y = "chiidref", all.x = TRUE, sort = FALSE)
  }
  
  return(d)
}

