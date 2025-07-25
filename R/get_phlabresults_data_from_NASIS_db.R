.get_phlabresults_data_from_NASIS_db <- function(SS=TRUE, dsn = NULL) {

  # hacks to make R CMD check --as-cran happy:
  sampledepthbottom <- NULL
  sampledepthtop    <- NULL
  phiidref          <- NULL
  # test_ph <- NULL

  q <- "SELECT peiidref AS peiid, phiid, phlabresultiid, phl.seqnum, sampledepthtop, sampledepthbottom, sampleid, 
               datacollector, claytotmeasured, claycarbmeasured, clayfinemeasured, silttotmeasured, siltfinemeasured, 
               siltcomeasured, sandtotmeasured, sandtotmethod, sandvcmeasured, sandcomeasured, 
               sandmedmeasured, sandfinemeasured, sandvfmeasured, sandvfmethod, sieve3inch, 
               sieve34inch, sieveno4, sieveno10, sieveno18, sieveno35, sieveno40, sieveno60, sieveno140,
               sieveno200, sieveno270, vonposthumification, lossonignition, textureclfieldlab, 
               fiberrubbedpct, fiberunrubbedpct, ph1to1h2o, ph01mcacl2, phnaf, phoxidized, phdeltah2o2,
               liquidlimitmeasured, plasticlimitmeasured, pi, atterbergsampcond, cole, esttotpotacidityetpa,
               camgmeh2, potassiummeh2, camgsatpaste, extractaciditykcl, basesatmeh2, cec7, cec82, ecec,
               phosphatephos, nitratenitrogen, ecmeasured, ecdeterminemeth, ec15, caco3equivmeasured, 
               gypsumequiv, sodium, sar, gypsumreq, humiccolor, fulviccolor, humicfulviccolor, alummeasured,
               pyrophoshue, pyrophosvalue, pyrophoschroma, melanicindex
  FROM phorizon_View_1 ph
  LEFT OUTER JOIN phlabresults_View_1 phl on phl.phiidref = ph.phiid
  ORDER BY peiidref, phiid, sampledepthtop;"

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (isFALSE(SS)) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # exec query
  d.phlabresults <- dbQueryNASIS(channel, q)

  # recode metadata domains
  d.phlabresults <- uncode(d.phlabresults, dsn = dsn)
  
  # cache original column names
  orig_names <- names(d.phlabresults)
  
  # compute thickness
  d.phlabresults$hzthk <- d.phlabresults$sampledepthbottom - d.phlabresults$sampledepthtop

  # identify horizons with duplicate phiid
  idx <- which(duplicated(d.phlabresults$phiid))

  if (length(idx) > 0) {
    message(paste("NOTICE: multiple records per pedon horizon exist in the `phlabresults` table, computing weighted averages and dominant values based on sample thickness"))
    
    if (anyNA(d.phlabresults[idx, "sampledepthbottom"])) {
      message("NOTICE: some `phlabresults` records are missing `sampledepthbottom`; affected weighted averages will return `NA` and dominant values will be from the first (shallowest top depth) record per horizon")
    }
  
    # aggregate dup phiid
    dup <- d.phlabresults[idx, "phiid"]
    dup_idx <- which(d.phlabresults$phiid %in% dup)
    d.dups <- d.phlabresults[dup_idx, ]
    
    id_vars <- c("peiid","phiid")
    num_vars <- names(d.dups)[!grepl("ph1to1h2o|ph01mcacl2|peiid|phiid", names(d.dups)) & 
                                sapply(d.dups, is.numeric)]
    d.dups_num <- cbind(d.dups[, id_vars, drop = FALSE], d.dups[, num_vars, drop = FALSE])

    var <- "phiid"
    d.dups_num <- do.call("rbind",
      by(d.dups_num, d.dups_num[[var]], function(x) { 
        data.frame(
        peiid = unique(x[['peiid']]),
        lapply(x[,colnames(x)[2:ncol(x)]], function(x2)
          weighted.mean(x2, w = x$hzthk, na.rm = TRUE))
        )})
      )

    char_vars <- names(d.dups)[names(d.dups) %in% c("hzthk") |
                                 sapply(d.dups, function(x) is.character(x) | is.factor(x) | is.logical(x))]
    d.dups_char <- cbind(d.dups[, id_vars, drop = FALSE], d.dups[, char_vars, drop = FALSE])

    d.dups_char <- do.call(
      "rbind", by(d.dups_char, d.dups_char[[var]], function(x) { 
        data.frame(
          peiid = unique(x[['peiid']]),
          lapply(x[2:ncol(x)], function(x2) x2[max(c(1, which.max(x$hzthk)), na.rm = TRUE)])
        )})
      )

    num_ph <- names(d.dups)[names(d.dups) %in% c("hzthk") |
                          grepl("ph1to1h2o|ph01mcacl2", names(d.dups))]
    d.dups_ph <- cbind(d.dups[, id_vars, drop = FALSE], d.dups[, num_ph, drop = FALSE])
    
    d.dups_ph <- do.call(
      "rbind",
      by(d.dups_ph, d.dups_ph[[var]], function(x) { data.frame(
          peiid = unique(x[['peiid']]),
          phiid = unique(x[['phiid']]),
          lapply(x[3:ncol(x)], function(x2) -log10(weighted.mean(1/10^x2, weights = x$hzthk, na.rm = TRUE)))
        )})
      )

    # remove calculated horizon thickness
    # TODO: should hzthk be returned as metadata? relevant to interpretation of combined lab results
    d.dups_num$hzthk <- NULL
    d.dups_char$hzthk <- NULL
    d.dups_ph$hzthk <- NULL
    d.phlabresults$hzthk <- NULL

    d.nodups <- merge(d.dups_num, d.dups_char, by = c("peiid", "phiid"), all.x = TRUE)
    d.nodups <- merge(d.nodups, d.dups_ph, by  = c("peiid", "phiid"), all.x = TRUE)
    
    # fill missing columns in "duplicates removed" data with NA
    # these are choice lists e.g. "sandtotmethod", "sandvfmethod", "atterbergsampcond", "ecdeterminemeth", 
    #                           "pyrophoshue", "pyrophosvalue", "pyrophoschroma"
    # when these columns area all NA the data type is logical and they fall through the cracks of data type splits                     
    d.nodups[orig_names[!orig_names %in% colnames(d.nodups)]] <- NA
    
    # recombine duplicated and non-duplicated data
    d.phlabresults <- rbind(d.phlabresults[-dup_idx, orig_names], d.nodups[orig_names])
  }

  # relabel names
  d.phlabresults[c("sampledepthtop", "sampledepthbottom", "hzthk")] <- NULL

  names(d.phlabresults) <- gsub("measured|lab$", "", names(d.phlabresults))

  vars <- c("peiid","phiid")
  idx <- names(d.phlabresults) %in% vars
  names(d.phlabresults)[idx] <- vars

  names(d.phlabresults)[!idx] <- paste0(names(d.phlabresults)[!idx], "_lab")

  # TODO: final cleaning of duplicate rows - dups exist in NASIS for some reason, so should this happen first
  # to eliminate extra rows with no data? Not sure what is causing this on the NASIS side
  d.phlabresults <-  d.phlabresults[rowSums(is.na(d.phlabresults)) < (length(d.phlabresults) - 1),]

  # done
  return(d.phlabresults)
}
