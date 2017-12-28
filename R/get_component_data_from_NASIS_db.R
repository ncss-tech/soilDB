##
## December, 2017
## D.E. Beaudette
## J.M. Skovlin
## S.M. Roecker
## 


## just the components
get_component_data_from_NASIS_db <- function(SS=TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q <- "SELECT dmudesc, compname, comppct_r, compkind, majcompflag, localphase, drainagecl, pmgroupname, elev_r, slope_l, slope_r, slope_h, aspectrep, map_r, airtempa_r as maat_r, soiltempa_r as mast_r, reannualprecip_r, ffd_r, tfact, wei, weg, nirrcapcl, nirrcapscl, irrcapcl, irrcapscl, frostact, hydgrp, corcon, corsteel, taxclname, taxorder, taxsuborder, taxgrtgroup, taxsubgrp, taxpartsize, taxpartsizemod, taxceactcl, taxreaction, taxtempcl, taxmoistscl, taxtempregime, soiltaxedition, coiid, dmuiid
  
  FROM 
  datamapunit_View_1 AS dmu 
  INNER JOIN component_View_1 AS co ON co.dmuiidref = dmu.dmuiid 
  LEFT OUTER JOIN copmgrp_View_1 ON copmgrp_View_1.coiidref = co.coiid 
  WHERE copmgrp_View_1.rvindicator = 1

  ORDER BY dmudesc, comppct_r DESC, compname ASC;"
  
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }
  
  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  
  # close connection
  RODBC::odbcClose(channel)
  
  # test for duplicate coiids
  idx <- which(table(d$coiid) > 1)
  if(length(idx) > 0) {
    dupes <- names(idx)
    assign('dupe.coiids', value=dupes, envir=soilDB.env)
    message("-> QC: duplicate coiids, this should not happen. Use `get('dupe.coiids', envir=soilDB.env)` for related coiid values.")
  }
  
  # test for no data
  if(nrow(d) == 0)
    stop('there are no NASIS components in your selected set!')
  
  # uncode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors)
  
  # done
  return(d)
}


# return all rows from correlation -- map unit -- legend map unit -- dmu / legend -- area
# note that all of these "target tables" have to be selected
get_component_correlation_data_from_NASIS_db <- function(SS=TRUE, dropAdditional=TRUE, dropNotRepresentative=TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q <- "SELECT mu.muiid, musym, nationalmusym, mu.muname, mukind, mutype, mustatus, muacres, farmlndcl, repdmu, dmuiid, areasymbol, areaname, ssastatus, cordate
  
  FROM  mapunit_View_1 AS mu
  
  LEFT OUTER JOIN correlation_View_1 AS cor ON cor.muiidref = mu.muiid
  LEFT OUTER JOIN datamapunit_View_1 AS dmu ON cor.dmuiidref = dmu.dmuiid  
  LEFT OUTER JOIN lmapunit_View_1 AS lm ON lm.muiidref = mu.muiid
  LEFT OUTER JOIN legend_View_1 AS leg ON leg.liid = lm.liidref
  LEFT OUTER JOIN area_View_1 AS a ON a.areaiid = leg.areaiidref
  
  ORDER BY nationalmusym, dmuiid;"
  
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }
  
  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  
  # close connection
  RODBC::odbcClose(channel)
  
  # test for no data
  if(nrow(d) == 0)
    stop('there are no records in your selected set!')
  
  # recode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors)
  
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
    message("-> QC: duplicate muiids: multiple 'representative' DMU / MU?. Use `get('dupe.muiids', envir=soilDB.env)` for related muiid values.")
  }
    
  
  # check for multiple DMUs:
  idx <- which(table(d$dmuiid) > 1)
  if(length(idx) > 0) {
    dupes <- names(idx)
    assign('multiple.mu.per.dmu', value=dupes, envir=soilDB.env)
    message("-> QC: DMUs assigned to multiple MU. Use `get('multiple.mu.per.dmu', envir=soilDB.env)` for related dmuiid values.")
  }
    
  
  # done
  return(d)
}

# get geomorphic desc for each component
get_component_cogeomorph_data_from_NASIS_db <- function(SS=TRUE) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)	
  
  
  q <- "SELECT cogeo.coiidref as coiid, cogeo.geomfmod, geomorfeat.geomfname, cogeo.geomfeatid, cogeo.existsonfeat, cogeo.geomfiidref, lower(geomorfeattype.geomftname) as geomftname
  
  FROM 
  component_View_1 AS co
  INNER JOIN cogeomordesc_View_1 AS cogeo ON co.coiid = cogeo.coiidref
  INNER JOIN geomorfeat ON geomorfeat.geomfiid = cogeo.geomfiidref  
  INNER JOIN geomorfeattype ON geomorfeattype.geomftiid = geomorfeat.geomftiidref 

  ORDER BY coiid, geomfeatid ASC;"
  
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }
  
  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  
  # close connection
  RODBC::odbcClose(channel)
  
  # done
  return(d)
}


# get copm for each component
get_component_copm_data_from_NASIS_db <- function(SS=TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)	
  
  
  q <- "SELECT cpmg.coiidref as coiid, cpm.seqnum as seqnum, pmorder, pmdept_r, pmdepb_r, pmmodifier, pmgenmod, pmkind, pmorigin
  
  FROM 
  component_View_1 AS co
  INNER JOIN copmgrp_View_1 AS cpmg ON cpmg.coiidref = co.coiid
  INNER JOIN copm_View_1 AS cpm ON cpm.copmgrpiidref = cpmg.copmgrpiid
  
  ORDER BY coiidref, seqnum, pmorder, copmgrpiid ASC;" 
  
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }
  
  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  
  # close connection
  RODBC::odbcClose(channel)
  
  # uncode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors)
  
  # done
  return(d)
}



# get ESD information for each component
get_component_esd_data_from_NASIS_db <- function(SS=TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q <- "SELECT coiidref as coiid, ecositeid, ecositenm, 
  ecositeorigin, ecositetype, ecositemlra, ecositelru, ecositenumber, ecositestate
  
  FROM coecosite_View_1 AS coecosite
  
  INNER JOIN ecologicalsite_View_1 AS es ON es.ecositeiid = coecosite.ecositeiidref
  
  ORDER BY coiid;"
  
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }
  
  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  
  # close connection
  RODBC::odbcClose(channel)
  
  # check for more than 1 record / coiid
  idx <- which(table(d$coiid) > 1)
  if(length(idx) > 0) {
    dupes <- names(idx)
    assign('multiple.ecosite.per.coiid', value=dupes, envir=soilDB.env)
    message("-> QC: multiple ecosites / component. Use `get('multiple.ecosite.per.coiid', envir=soilDB.env)` for related coiid values.")
  }
  
  # uncode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors)
  
  # done
  return(d)
}

## TODO: convert any multiple entries into a comma delimited string
# get OtherVeg information for each component
get_component_otherveg_data_from_NASIS_db <- function(SS=TRUE) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q <- "SELECT coiidref as coiid, ovegclid, ovegclname, coothvegcl.recwlupdated
  FROM coothvegclass_View_1 coothvegcl
  INNER JOIN othvegclass_View_1 as ovc ON ovc.ovegcliid = coothvegcl.ovegcliidref
  ORDER BY coiid;"
  
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }
  
  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  
  # close connection
  RODBC::odbcClose(channel)
  
  # check for more than 1 record / coiid
  idx <- which(table(d$coiid) > 1)
  if(length(idx) > 0) {
    dupes <- names(idx)
    assign('multiple.otherveg.per.coiid', value=dupes, envir=soilDB.env)
    message("-> QC: multiple othervegclasses / component. Use `get('multiple.otherveg.per.coiid', envir=soilDB.env)` for related coiid values.")
  }
  
  # uncode metadata domains
  #d <- uncode(d)
  
  # done
  return(d)
}

get_comonth_from_NASIS_db <- function(SS=TRUE, fill=FALSE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q <- "SELECT coiidref AS coiid, month, flodfreqcl, floddurcl, pondfreqcl, ponddurcl, ponddep_l, ponddep_r, ponddep_h, dlyavgprecip_l, dlyavgprecip_r, dlyavgprecip_h, comonthiid
  FROM comonth_View_1 AS comonth;"
  
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }
  
  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  
  # uncode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors)
  
  # optionally fill missing coiids
  if(fill) {
    q <- "SELECT coiid
    FROM component_View_1
    ORDER BY coiid;"
    
    # toggle selected set vs. local DB
    if(SS == FALSE) {
      q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
    }
    
    # exec query
    d.coiid <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  }
  
  # close connection
  RODBC::odbcClose(channel)
  
  # fix month factor levels
  # using 3-letter month names
  d$month <- months(as.Date(paste0("2016-", d$month, "-01"), format="%Y-%B-%d"), abbreviate = TRUE)
  d$month <- factor(d$month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  
  # fix other factor levels
  d$flodfreqcl <- factor(d$flodfreqcl, levels=c('none', 'very rare', 'rare', 'occasional', 'common', 'frequent', 'very frequent'))
  d$floddurcl <- factor(d$floddurcl, levels=c('extremely brief', 'very brief', 'brief', 'long', 'very long'), labels=c('Extremely brief (0.1 to 4 hours)', 'Very brief (4 to 48 hours)', 'Brief (2 to 7 days)', 'Long (7 to 30 days)', 'Very long (more than 30 days)'))
  
  d$pondfreqcl <- factor(d$pondfreqcl, levels=c('none', 'rare', 'occasional', 'common', 'frequent'))
  d$ponddurcl <- factor(d$ponddurcl, levels=c('very brief', 'brief', 'long', 'very long'), labels=c('Very brief (4 to 48 hours)', 'Brief (2 to 7 days)', 'Long (7 to 30 days)', 'Very long (more than 30 days)'))
  
  
  # optionally fill missing coiids
  if(fill) {
    # make a new DF with all coiids and months
    nd <- expand.grid(coiid=d.coiid$coiid, month=levels(d$month))
    nd$month <- factor(nd$month, levels=levels(d$month))
    
    # join full version to comonth records
    # nd contains the full set of component records IDs
    d <- join(nd, d, by=c('coiid', 'month'), type='left')
    
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
get_copedon_from_NASIS_db <- function(SS=TRUE) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q <- "SELECT coiidref as coiid, peiidref as peiid, upedonid as pedon_id, rvindicator as representative 
  
  FROM copedon_View_1 copedon
  
  LEFT OUTER JOIN pedon_View_1 p ON p.peiid = copedon.peiidref;
  "
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }
  
  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  
  # close connection
  RODBC::odbcClose(channel)
  
  # missing pedon ID suggests records not in the selected set or local database
  if(nrow(d) > 0 & any(is.na(d$pedon_id))) {
    message('some linked pedons not in selected set or local database')
  }
  
  # done
  return(d)
}


## TODO: better documentation for "fill" argument
# https://github.com/ncss-tech/soilDB/issues/50
get_component_horizon_data_from_NASIS_db <- function(SS=TRUE, fill = FALSE) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q <- "SELECT coiid, chiid, hzname, hzdept_r, hzdepb_r, texture, fragvoltot_l, fragvoltot_r, fragvoltot_h, sandtotal_l, sandtotal_r, sandtotal_h, silttotal_l, silttotal_r, silttotal_h, claytotal_l, claytotal_r, claytotal_h, om_l, om_r, om_h, structgrpname, dbthirdbar_l, dbthirdbar_r, dbthirdbar_h, ksat_l, ksat_r, ksat_h, awc_l, awc_r, awc_h, lep_r, sar_r, ec_r, cec7_r, sumbases_r, ph1to1h2o_l, ph1to1h2o_r, ph1to1h2o_h, caco3_l, caco3_r, caco3_h
  
  FROM
  component_View_1 co 
  LEFT OUTER JOIN chorizon_View_1 ch ON ch.coiidref = co.coiid 
  LEFT OUTER JOIN chtexturegrp_View_1 cht ON cht.chiidref = ch.chiid AND cht.rvindicator = 1
  LEFT OUTER JOIN chstructgrp_View_1 chs ON chs.chiidref = ch.chiid AND chs.rvindicator = 1

  -- why is this here?
  INNER JOIN datamapunit_View_1 dmu ON dmu.dmuiid = co.dmuiidref

  ORDER BY dmudesc, comppct_r DESC, compname ASC, hzdept_r ASC;"
  
  
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }
  
  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  
  # close connection
  RODBC::odbcClose(channel)
  
  ## TODO: better documentation for "fill" argument
  # https://github.com/ncss-tech/soilDB/issues/50
  # remove records what are missing horizon data
  if (fill == FALSE) {
    d <- d[!is.na(d$chiid), ]
  }
  
  # done
  return(d)
}

