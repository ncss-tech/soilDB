##
## December, 2015
## D.E. Beaudette
## J.M. Skovlin
## S.M. Roecker
## 

##
## re-boot of previous functionality
##


## just the components
get_component_data_from_NASIS_db <- function() {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q.component <- "SELECT dmudesc, compname, comppct_r, compkind, majcompflag, localphase, drainagecl, pmgroupname, elev_r, slope_l, slope_r, slope_h, aspectrep, map_r, airtempa_r as maat_r, soiltempa_r as mast_r, reannualprecip_r, ffd_r, tfact, wei, weg, nirrcapcl, nirrcapscl, irrcapcl, irrcapscl, frostact, hydgrp, corcon, corsteel, taxclname, taxorder, taxsuborder, taxgrtgroup, taxsubgrp, taxpartsize, taxpartsizemod, taxceactcl, taxreaction, taxtempcl, taxmoistscl, taxtempregime, soiltaxedition, coiid, dmuiid
  
  FROM 
  datamapunit_View_1 AS dmu 
  INNER JOIN component_View_1 AS co ON co.dmuiidref = dmu.dmuiid 
  LEFT OUTER JOIN copmgrp_View_1 ON copmgrp_View_1.coiidref = co.coiid 
  WHERE copmgrp_View_1.rvindicator = 1

  ORDER BY dmudesc, comppct_r DESC, compname ASC;"
  
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  # exec query
  d.component <- RODBC::sqlQuery(channel, q.component, stringsAsFactors=FALSE)
  
  # close connection
  RODBC::odbcClose(channel)
  
  # test for duplicate coiids
  idx <- which(table(d.component$coiid) > 1)
  if(length(idx) > 0) {
    dupes <- names(idx)
    assign('dupe.coiids', value=dupes, envir=soilDB.env)
    message("-> QC: duplicate coiids, this should not happen. Use `get('dupe.coiids', envir=soilDB.env)` for related coiid values.")
  }
  
  # test for no data
  if(nrow(d.component) == 0)
    stop('there are no NASIS components in your selected set!')
  
  # recode metadata domains
  d.component <- uncode(d.component)
  
  # done
  return(d.component)
}


# return all rows from correlation -- map unit -- legend map unit -- dmu / legend -- area
# note that all of these "target tables" have to be selected
get_component_correlation_data_from_NASIS_db <- function(dropAdditional=TRUE, dropNotRepresentative=TRUE) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q <- "SELECT mu.muiid, musym, nationalmusym, mu.muname, mukind, mutype, mustatus, muacres, farmlndcl, repdmu, dmuiid, areasymbol, areaname, ssastatus, cordate
  
  FROM  mapunit_View_1 mu
  
  LEFT OUTER JOIN correlation_View_1 ON correlation_View_1.muiidref = mu.muiid
  LEFT OUTER JOIN datamapunit_View_1 ON correlation_View_1.dmuiidref = datamapunit_View_1.dmuiid  
  LEFT OUTER JOIN lmapunit_View_1 ON lmapunit_View_1.muiidref = mu.muiid
  LEFT OUTER JOIN legend_View_1 ON legend_View_1.liid = lmapunit_View_1.liidref
  LEFT OUTER JOIN area_View_1 ON area_View_1.areaiid = legend_View_1.areaiidref
  
  ORDER BY nationalmusym, dmuiid;"
  
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  
  # close connection
  RODBC::odbcClose(channel)
  
  # test for no data
  if(nrow(d) == 0)
    stop('there are no records in your selected set!')
  
  # recode metadata domains
  d <- uncode(d)
  
  # optionally drop additional | NA mustatus
  if(dropAdditional) {
    idx <- which(d$mustatus == 'Additional')
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
get_component_cogeomorph_data_from_NASIS_db <- function() {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)	
  
  
  q.cogeomorph <- "SELECT cogeomordesc_View_1.coiidref as coiid, cogeomordesc_View_1.geomfmod, geomorfeat.geomfname, cogeomordesc_View_1.geomfeatid, cogeomordesc_View_1.existsonfeat, cogeomordesc_View_1.geomfiidref, lower(geomorfeattype.geomftname) as geomftname
  
  FROM 
  component_View_1
  INNER JOIN cogeomordesc_View_1 ON component_View_1.coiid = cogeomordesc_View_1.coiidref
  INNER JOIN geomorfeat ON geomorfeat.geomfiid = cogeomordesc_View_1.geomfiidref  
  INNER JOIN geomorfeattype ON geomorfeattype.geomftiid = geomorfeat.geomftiidref 

  ORDER BY coiid, geomfeatid ASC;"
  
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  # exec query
  d <- RODBC::sqlQuery(channel, q.cogeomorph, stringsAsFactors=FALSE)
  
  # close connection
  RODBC::odbcClose(channel)
  
  # done
  return(d)
}


# get copm for each component
get_component_copm_data_from_NASIS_db <- function() {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)	
  
  
  q.copm <- "SELECT copmgrp_View_1.coiidref as coiid, copm_View_1.seqnum as seqnum, pmorder, pmdept_r, pmdepb_r, pmmodifier, pmgenmod, pmkind, pmorigin
  
  FROM 
  component_View_1
  INNER JOIN copmgrp_View_1 ON copmgrp_View_1.coiidref = component_View_1.coiid
  INNER JOIN copm_View_1 ON copm_View_1.copmgrpiidref = copmgrp_View_1.copmgrpiid
  
  ORDER BY coiidref, seqnum, pmorder, copmgrpiid ASC;" 
  
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  # exec query
  d <- RODBC::sqlQuery(channel, q.copm, stringsAsFactors=FALSE)
  
  # close connection
  RODBC::odbcClose(channel)
  
  # recode metadata domains
  d <- uncode(d)
  
  # done
  return(d)
}


## TODO: there are still columns that need decoding
# get ESD information for each component
get_component_esd_data_from_NASIS_db <- function() {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q <- "SELECT coiidref as coiid, ecositeid, ecositenm, 
  ecositeorigin, ecositetype, ecositemlra, ecositelru, ecositenumber, ecositestate
  
  FROM coecosite_View_1 AS coecosite
  
  INNER JOIN ecologicalsite_View_1 ON ecologicalsite_View_1.ecositeiid = coecosite.ecositeiidref
  
  ORDER BY coiid;"
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  
  # check for more than 1 record / coiid
  idx <- which(table(d$coiid) > 1)
  if(length(idx) > 0) {
    dupes <- names(idx)
    assign('multiple.ecosite.per.coiid', value=dupes, envir=soilDB.env)
    message("-> QC: multiple ecosites / component. Use `get('multiple.ecosite.per.coiid', envir=soilDB.env)` for related coiid values.")
  }
    
  
  # close connection
  RODBC::odbcClose(channel)
  
  # recode metadata domains
  d <- uncode(d)
  
  # done
  return(d)
}

## TODO: convert any multiple entries into a comma delimited string
# get OtherVeg information for each component
get_component_otherveg_data_from_NASIS_db <- function() {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q <- "SELECT coiidref as coiid, ovegclid, ovegclname, coothvegcl.recwlupdated
  FROM coothvegclass_View_1 coothvegcl
  INNER JOIN othvegclass ON othvegclass.ovegcliid = coothvegcl.ovegcliidref
  ORDER BY coiid;"
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  
  # check for more than 1 record / coiid
  idx <- which(table(d$coiid) > 1)
  if(length(idx) > 0) {
    dupes <- names(idx)
    assign('multiple.otherveg.per.coiid', value=dupes, envir=soilDB.env)
    message("-> QC: multiple othervegclasses / component. Use `get('multiple.otherveg.per.coiid', envir=soilDB.env)` for related coiid values.")
  }
  
  
  # close connection
  RODBC::odbcClose(channel)
  
  # recode metadata domains
  #d <- uncode(d)
  
  # done
  return(d)
}

get_comonth_from_NASIS_db <- function(fill=FALSE) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q <- "SELECT coiidref AS coiid, month, flodfreqcl, floddurcl, pondfreqcl, ponddurcl, ponddep_l, ponddep_r, ponddep_h, dlyavgprecip_l, dlyavgprecip_r, dlyavgprecip_h, comonthiid
  FROM comonth_View_1 AS comonth;"
  
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  
  # optionally fill missing coiids
  if(fill) {
    q <- "SELECT coiid
  FROM component_View_1
    ORDER BY coiid;"
    
    # exec query
    d.coiid <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  }
  
  # close connection
  RODBC::odbcClose(channel)
  
  # recode metadata domains
  d <- uncode(d)
  
  # fix month factor levels
  # using 3-letter month names
  d$month <- months(as.Date(paste0("2016-", d$month, "-01"), format="%Y-%B-%d"), abbreviate = TRUE)
  d$month <- factor(d$month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  
  # fix other factor levels
  d$flodfreqcl <- factor(d$flodfreqcl, levels=c('None', 'Very rare', 'Rare', 'Occasional', 'Frequent', 'Very frequent'))
  d$floddurcl <- factor(d$floddurcl, levels=c('Extremely brief (0.1 to 4 hours)', 'Very brief (4 to 48 hours)', 'Brief (2 to 7 days)', 'Long (7 to 30 days)', 'Very long (more than 30 days)'))
  
  d$pondfreqcl <- factor(d$pondfreqcl, levels=c('None', 'Rare', 'Occasional', 'Frequent'))
  d$ponddurcl <- factor(d$ponddurcl, levels=c('Very brief (4 to 48 hours)', 'Brief (2 to 7 days)', 'Long (7 to 30 days)', 'Very long (more than 30 days)'))
  
  
  # optionally fill missing coiids
  if(fill) {
    # make a new DF with all coiids and months
    nd <- expand.grid(coiid=d.coiid$coiid, month=levels(d$month))
    nd$month <- factor(nd$month, levels=levels(d$month))
    
    # join full version to comonth records
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
get_copedon_from_NASIS_db <- function() {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q <- "SELECT coiidref as coiid, peiidref as peiid, upedonid as pedon_id, rvindicator as representative 
  
  FROM copedon_View_1 copedon
  
  LEFT OUTER JOIN pedon ON pedon.peiid = copedon.peiidref;
  "
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  
  # close connection
  RODBC::odbcClose(channel)
  
  # done
  return(d)
}



get_component_horizon_data_from_NASIS_db <- function(fill = FALSE) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q.chorizon <- "SELECT coiid, chiid, hzname, hzdept_r, hzdepb_r, texture, fragvoltot_l, fragvoltot_r, fragvoltot_h, sandtotal_l, sandtotal_r, sandtotal_h, silttotal_l, silttotal_r, silttotal_h, claytotal_l, claytotal_r, claytotal_h, om_l, om_r, om_h, structgrpname, dbthirdbar_l, dbthirdbar_r, dbthirdbar_h, ksat_l, ksat_r, ksat_h, awc_l, awc_r, awc_h, lep_r, sar_r, ec_r, cec7_r, sumbases_r, ph1to1h2o_l, ph1to1h2o_r, ph1to1h2o_h, caco3_l, caco3_r, caco3_h
  
  FROM
  component_View_1 co LEFT OUTER JOIN
  chorizon ch ON ch.coiidref = co.coiid LEFT OUTER JOIN
  chtexturegrp cht ON cht.chiidref = ch.chiid AND cht.rvindicator = 1

  LEFT OUTER JOIN
    chstructgrp chs ON chs.chiidref = ch.chiid AND chs.rvindicator = 1

  ORDER BY coiidref, hzdept_r ASC;"
  
  
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  # exec query
  d.chorizon <- RODBC::sqlQuery(channel, q.chorizon, stringsAsFactors=FALSE)
  
  # close connection
  RODBC::odbcClose(channel)
  
  # fill
  if (fill == FALSE) {
    d.chorizon <- d.chorizon[!is.na(d.chorizon$chiid), ]
  }
  
  # done
  return(d.chorizon)
}


## TODO: this will not ID horizons with no depths
## TODO: better error checking / reporting is needed: coiid, dmu id, component name
fetchNASIS_component_data <- function(rmHzErrors=TRUE, fill = FALSE) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  # load data in pieces
  f.comp <- get_component_data_from_NASIS_db()
  f.chorizon <- get_component_horizon_data_from_NASIS_db(fill)
  f.copm <- get_component_copm_data_from_NASIS_db()
  f.cogeomorph <- get_component_cogeomorph_data_from_NASIS_db()
  f.otherveg <- get_component_otherveg_data_from_NASIS_db()
  f.ecosite <- get_component_esd_data_from_NASIS_db()
  
  # optionally test for bad horizonation... flag, and remove
  if(rmHzErrors) {
    f.chorizon.test <- plyr::ddply(f.chorizon, 'coiid', test_hz_logic, topcol='hzdept_r', bottomcol='hzdepb_r', strict=TRUE)
    
    # which are the good (valid) ones?
    good.ids <- as.character(f.chorizon.test$coiid[which(f.chorizon.test$hz_logic_pass)])
    bad.ids <- as.character(f.chorizon.test$coiid[which(! f.chorizon.test$hz_logic_pass)])
    
    # keep the good ones
    f.chorizon <- f.chorizon[which(f.chorizon$coiid %in% good.ids), ]
    
    # keep track of those components with horizonation errors
    if(length(bad.ids) > 0)
      assign('component.hz.problems', value=bad.ids, envir=soilDB.env)
  }
  
  
  # upgrade to SoilProfilecollection
  depths(f.chorizon) <- coiid ~ hzdept_r + hzdepb_r
  
  ## TODO: this will fail in the presence of duplicates
  ## TODO: make this error more informative
  # add site data to object
  site(f.chorizon) <- f.comp # left-join via coiid
  
  # join-in copm strings
  ## 2017-3-13: short-circuts need testing, consider pre-marking mistakes before parsing
  pm <- plyr::ddply(f.copm, 'coiid', .formatcoParentMaterialString, name.sep=' & ')
  if(nrow(pm) > 0)
    site(f.chorizon) <- pm
  
  # join-in cogeomorph strings
  ## 2017-3-13: short-circuts need testing, consider pre-marking mistakes before parsing
  lf <- plyr::ddply(f.cogeomorph, 'coiid', .formatcoLandformString, name.sep=' & ')
  if(nrow(lf) > 0)
    site(f.chorizon) <- lf
  
  # join-in ecosite string
  ## 2017-3-06: short-circuts need testing, consider pre-marking mistakes before parsing
  es <- plyr::ddply(f.ecosite, 'coiid', .formatEcositeString, name.sep=' & ')
  if(nrow(es) > 0)
    site(f.chorizon) <- es
  
  # join-in othervegclass string
  ## 2017-3-06: short-circuts need testing, consider pre-marking mistakes before parsing
  ov <- plyr::ddply(f.otherveg, 'coiid', .formatOtherVegString, name.sep=' & ')
  if(nrow(ov) > 0)
    site(f.chorizon) <- ov
  
  # print any messages on possible data quality problems:
  if(exists('component.hz.problems', envir=soilDB.env))
    message("-> QC: horizon errors detected, use `get('component.hz.problems', envir=soilDB.env)` for related coiid values")
  
  # done, return SPC
  return(f.chorizon)
  
}


