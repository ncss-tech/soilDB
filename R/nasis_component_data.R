##
## December, 2015
## D.E. Beaudette
## J.M. Scovlin
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
  
  q <- "SELECT dmuiid, dmudesc, coiid, compname, comppct_r, compkind, majcompflag, localphase, slope_r, tfact, wei, weg, drainagecl, elev_r, aspectrep, map_r, airtempa_r as maat_r, soiltempa_r as mast_r, reannualprecip_r, ffd_r, nirrcapcl, nirrcapscl, irrcapcl, irrcapscl, frostact, hydgrp, corcon, corsteel, taxclname, taxorder, taxsuborder, taxgrtgroup, taxsubgrp, taxpartsize, taxpartsizemod, taxceactcl, taxreaction, taxtempcl, taxmoistscl, taxtempregime, soiltaxedition
  
  FROM  component

  INNER JOIN datamapunit_View_1 ON datamapunit_View_1.dmuiid = component.dmuiidref


  ORDER BY dmudesc, coiid, comppct_r DESC;"
  
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
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
  
  # recode metadata domains
  d <- .metadata_replace(d)
  
  # done
  return(d)
}


# return all rows from correlation -- map unit -- legend map unit -- dmu / legend -- area
# note that all of these "target tables" have to be selected
get_component_correlation_data_from_NASIS_db <- function(dropAdditional=TRUE, dropNotRepresentative=TRUE) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q <- "SELECT mu.muiid, musym, nationalmusym, mu.muname, mukind, mustatus, muacres, farmlndcl, repdmu, dmuiid, areasymbol, areaname, ssastatus, cordate
  
  FROM  mapunit_View_1 mu
  
  LEFT OUTER JOIN lmapunit ON lmapunit.muiidref = mu.muiid
  LEFT OUTER JOIN legend ON legend.liid = lmapunit.liidref
  LEFT OUTER JOIN area ON area.areaiid = legend.areaiidref
  LEFT OUTER JOIN correlation ON correlation.muiidref = mu.muiid 
  LEFT OUTER JOIN datamapunit ON correlation.dmuiidref = datamapunit.dmuiid
  
  ORDER BY dmuiid;"
  
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
  d <- .metadata_replace(d)
  
  # optionally drop additional | NA mustatus
  if(dropAdditional) {
    idx <- which(! d$mustatus == 'additional')
    d <- d[idx, ]
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



## TODO: there are still columns that need decoding
# get ESD information for each component
get_component_esd_data_from_NASIS_db <- function() {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q <- "SELECT coiidref as coiid, ecositeid, ecositenm, 
  ecositeorigin, ecositetype, ecositemlra, ecositelru, ecositenumber, ecositestate
  
  FROM coecosite_View_1 coecosite
  
  INNER JOIN ecologicalsite ON ecologicalsite.ecositeiid = coecosite.ecositeiidref
  
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
  d <- .metadata_replace(d)
  
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



get_component_horizon_data_from_NASIS_db <- function() {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q <- "SELECT coiidref as coiid, chiid, hzname, hzdept_r, hzdepb_r, fragvoltot_l, fragvoltot_r, fragvoltot_h, sandtotal_r, silttotal_r, claytotal_r, texture, om_r, dbovendry_r, ksat_r, awc_r, lep_r, sar_r, ec_r, cec7_r, sumbases_r, ph1to1h2o_r
  
  FROM chorizon_View_1 ch
  
  LEFT OUTER JOIN (SELECT * FROM chtexturegrp WHERE rvindicator = 'TRUE') AS cht ON cht.chiidref = ch.chiid

  ORDER BY coiidref, hzdept_r ASC;"
  
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  
  # close connection
  RODBC::odbcClose(channel)
  
  # done
  return(d)
}


## TODO: this will not ID horizons with no depths
## TODO: better error checking / reporting is needed: coiid, dmu id, component name
fetchNASIS_component_data <- function(rmHzErrors=TRUE) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  # load data in pieces
  f.comp <- get_component_data_from_NASIS_db()
  f.chorizon <- get_component_horizon_data_from_NASIS_db()
  
  # optionally test for bad horizonation... flag, and remove
  if(rmHzErrors) {
    f.chorizon.test <- ddply(f.chorizon, 'coiid', test_hz_logic, topcol='hzdept_r', bottomcol='hzdepb_r', strict=TRUE)
    
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
  
  
  # print any messages on possible data quality problems:
  if(exists('component.hz.problems', envir=soilDB.env))
    message("-> QC: horizon errors detected, use `get('component.hz.problems', envir=soilDB.env)` for related coiid values")
  
  # done, return SPC
  return(f.chorizon)
  
}


