##
## December, 2015
## D.E. Beaudette
## J.M. Scovlin
## 

##
## re-boot of previous functionality
##


## just the components
get_component_data_from_NASIS_db <- function() {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q <- "SELECT dmuiid, dmudesc, coiid, compname, comppct_r, ck.ChoiceName as compkind, majcompflag, localphase, slope_r, tfact, wei, weg, dc.ChoiceName as drainage_class, elev_r, aspectrep, map_r, airtempa_r as maat_r, soiltempa_r as mast_r, reannualprecip_r, ffd_r, nirrcapcl, nirrcapscl, irrcapcl, irrcapscl, fa.ChoiceName as frost_action, hg.ChoiceLabel as hydgrp, crc.ChoiceName as corcon, crs.ChoiceName as corsteel, taxclname, txo.ChoiceName as taxorder, txs.ChoiceName as taxsuborder, txgg.ChoiceName as  taxgrtgroup, txsg.ChoiceName as taxsubgrp, txps.ChoiceName as taxpartsize, txpsm.ChoiceName as taxpartsizemod, txact.ChoiceName as taxceactcl, txr.ChoiceName as taxreaction, txtc.ChoiceName as taxtempcl, txmc.ChoiceName as taxmoistscl, txtr.ChoiceName as taxtempregime, txed.ChoiceName as soiltaxedition
  FROM
  component_View_1
  INNER JOIN datamapunit_View_1 ON datamapunit_View_1.dmuiid = component_View_1.dmuiidref
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 148) AS dc ON drainagecl = dc.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 120) AS fa ON frostact = fa.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 103) AS crc ON corcon = crc.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 104) AS crs ON corsteel = crs.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 102) AS ck ON compkind = ck.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 132) AS txo ON taxorder = txo.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 134) AS txs ON taxsuborder = txs.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 130) AS txgg ON taxgrtgroup = txgg.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 187) AS txsg ON taxsubgrp = txsg.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 127) AS txps ON taxpartsize = txps.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 521) AS txpsm ON taxpartsizemod = txpsm.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 520) AS txact ON taxceactcl = txact.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 128) AS txr ON taxreaction = txr.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 185) AS txtc ON taxtempcl = txtc.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 131) AS txmc ON taxmoistscl = txmc.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 188) AS txtr ON taxtempregime = txtr.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 2030) AS txed ON soiltaxedition = txed.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 115) AS hg ON hydgrp = hg.ChoiceValue
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
  
  # done
  return(d)
}


# return all rows from correlation -- map unit -- legend map unit -- dmu
get_component_correlation_data_from_NASIS_db <- function(dropAdditional=TRUE, dropNotRepresentative=TRUE) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q <- "SELECT areasymbol, muiid, musym, nationalmusym, mapunit_View_1.muname, mk.ChoiceName as mukind, ms.ChoiceName as mustatus, muacres, fc.ChoiceLabel as farmlndcl, repdmu, dmuiid
  FROM
  mapunit_View_1
  LEFT OUTER JOIN lmapunit_View_1 ON lmapunit_View_1.muiidref = mapunit_View_1.muiid
  LEFT OUTER JOIN legend_View_1 ON legend_View_1.liid = lmapunit_View_1.liidref
  LEFT OUTER JOIN area_View_1 ON area_View_1.areaiid = legend_View_1.areaiidref
  LEFT OUTER JOIN correlation_View_1 ON mapunit_View_1.muiid = correlation_View_1.muiidref
  LEFT OUTER JOIN datamapunit_View_1 ON correlation_View_1.dmuiidref = datamapunit_View_1.dmuiid
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 118) AS mk ON mukind = mk.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 138) AS ms ON mustatus = ms.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 151) AS fc ON farmlndcl = fc.ChoiceValue
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
  
  # optionally drop additional | NA mustatus
  if(dropAdditional) {
    idx <- which(! (d$mustatus == 'additional' &  is.na(d$mustatus)))
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
  eo.ChoiceName AS ecositeorigin, et.ChoiceName AS ecositetype, em.ChoiceName AS ecositemlra, el.ChoiceName AS ecositelru, ecositenumber, es.ChoiceName AS ecositestate
  FROM coecosite_View_1
  INNER JOIN ecologicalsite_View_1 ON ecositeiidref = ecositeiid
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 4913) AS eo ON ecositeorigin = eo.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1253) AS et ON ecositetype = et.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1252) AS em ON ecositemlra = em.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1310) AS el ON ecositelru = el.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1298) AS es ON ecositestate = es.ChoiceValue
  ORDER BY coiid ;"
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
  FROM copedon_View_1
  JOIN pedon_View_1 ON peiidref = peiid
  WHERE rvindicator = 1;
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
  
  q <- "SELECT coiidref as coiid, chiid, hzname, hzdept_r, hzdepb_r, fragvoltot_r, sandtotal_r, silttotal_r, claytotal_r, om_r, dbovendry_r, ksat_r, awc_r, lep_r, sar_r, ec_r, cec7_r, sumbases_r, ph1to1h2o_r
  FROM chorizon_View_1 
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


