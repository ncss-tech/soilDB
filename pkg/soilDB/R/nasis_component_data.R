##
## December, 2012
## D.E. Beaudette
## J.M. Scovlin
## 
## prototype functions for extracting component data from local NASIS
##
## requires that named tables are populated in the local NASIS database
## TODO: add more child tables via 'extra-' query functions


get_component_data_from_NASIS_db <- function() {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
	q <- "SELECT dmudesc, coiid, compname, comppct_r, ck.ChoiceName as compkind, majcompflag, localphase, slope_r, tfact, wei, weg, dc.ChoiceName as drainage_class, elev_r, aspectrep, map_r, airtempa_r as maat_r, soiltempa_r as mast_r, reannualprecip_r, ffd_r, nirrcapcl, nirrcapscl, irrcapcl, irrcapscl, fa.ChoiceName as frost_action, hg.ChoiceLabel as hydgrp, crc.ChoiceName as corcon, crs.ChoiceName as corsteel, taxclname, txo.ChoiceName as taxorder, txs.ChoiceName as taxsuborder, txgg.ChoiceName as  taxgrtgroup, txsg.ChoiceName as taxsubgrp, txps.ChoiceName as taxpartsize, txpsm.ChoiceName as taxpartsizemod, txact.ChoiceName as taxceactcl, txr.ChoiceName as taxreaction, txtc.ChoiceName as taxtempcl, txmc.ChoiceName as taxmoistscl, txtr.ChoiceName as taxtempregime, txed.ChoiceName as soiltaxedition, nationalmusym, muname, mk.ChoiceName as mukind, musym, ms.ChoiceName as mustatus, fc.ChoiceLabel as farmlndcl, dmuiid, muiid, repdmu
FROM ((((((((((((((((((((((((
component_View_1
INNER JOIN datamapunit_View_1 ON datamapunit_View_1.dmuiid = component_View_1.dmuiidref)
LEFT OUTER JOIN correlation_View_1 ON correlation_View_1.dmuiidref = datamapunit_View_1.dmuiid AND repdmu = 1)
LEFT OUTER JOIN mapunit_View_1 ON mapunit_View_1.muiid = correlation_View_1.muiidref AND repdmu = 1)
LEFT OUTER JOIN lmapunit_View_1 ON lmapunit_View_1.muiidref = mapunit_View_1.muiid)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 148) AS dc ON drainagecl = dc.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 120) AS fa ON frostact = fa.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 103) AS crc ON corcon = crc.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 104) AS crs ON corsteel = crs.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 102) AS ck ON compkind = ck.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 132) AS txo ON taxorder = txo.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 134) AS txs ON taxsuborder = txs.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 130) AS txgg ON taxgrtgroup = txgg.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 187) AS txsg ON taxsubgrp = txsg.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 127) AS txps ON taxpartsize = txps.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 521) AS txpsm ON taxpartsizemod = txpsm.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 520) AS txact ON taxceactcl = txact.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 128) AS txr ON taxreaction = txr.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 185) AS txtc ON taxtempcl = txtc.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 131) AS txmc ON taxmoistscl = txmc.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 188) AS txtr ON taxtempregime = txtr.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 2030) AS txed ON soiltaxedition = txed.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 118) AS mk ON mukind = mk.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 138) AS ms ON mustatus = ms.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 115) AS hg ON hydgrp = hg.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 151) AS fc ON farmlndcl = fc.ChoiceValue
WHERE ms.ChoiceName IS NULL OR ms.ChoiceName != 'additional'
ORDER BY dmudesc, coiid, comppct_r DESC;"
	
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
	
	# exec query
	d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)

	# close connection
	RODBC::odbcClose(channel)
	
	# test for no data
	if(nrow(d) == 0)
		stop('there are no NASIS components in your selected set!')
	
  # test for duplication, this suggests errors:
  # 1. a map unit NOT marked as additional that should be
  # 2. two or more map unit symbols linked to the same DMU, and rep DMU checked on >1 row
	dupe.check <- which(table(d$coiid) > 1)
  if(length(dupe.check) > 0) {
    bad.data <- d[d$coiid %in% names(dupe.check), c('musym', 'mustatus', 'dmudesc', 'compname', 'coiid', 'dmuiid', 'muiid')]
    assign('tangled.comp.legend', value=bad.data, envir=soilDB.env)
    message('tangled [legend]--[correlation]--[data mapunit] links')
  }
  
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
FROM copedon
JOIN pedon ON peiidref = peiid
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
  
	q <- "SELECT chiid, coiidref as coiid, hzname, hzdept_r, hzdepb_r, fragvoltot_r, sandtotal_r, silttotal_r, claytotal_r, om_r, dbovendry_r, ksat_r, awc_r, lep_r, sar_r, ec_r, cec7_r, sumbases_r, ph1to1h2o_r
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
fetchNASIS_component_data <- function() {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
	# load data in pieces
	f.comp <- suppressWarnings(get_component_data_from_NASIS_db())
	f.chorizon <- get_component_horizon_data_from_NASIS_db()
	
	# test for bad horizonation... flag, and remove
	f.chorizon.test <- ddply(f.chorizon, 'coiid', test_hz_logic, topcol='hzdept_r', bottomcol='hzdepb_r', strict=TRUE)
	
	# which are the good (valid) ones?
	good.ids <- as.character(f.chorizon.test$coiid[which(f.chorizon.test$hz_logic_pass)])
	bad.ids <- as.character(f.chorizon.test$coiid[which(f.chorizon.test$hz_logic_pass == FALSE)])
	
	# keep the good ones
	f.chorizon <- f.chorizon[which(f.chorizon$coiid %in% good.ids), ]
	
	# upgrade to SoilProfilecollection
	depths(f.chorizon) <- coiid ~ hzdept_r + hzdepb_r
	
	## TODO: this will fail in the presence of duplicates
  ## TODO: make this error more informative
	# add site data to object
	site(f.chorizon) <- f.comp # left-join via coiid
	
	# 7. save and mention bad pedons
	if(length(bad.ids) > 0) {
		bad.idx <- which(f.comp$coiid %in% bad.ids)
		bad.labels <- paste(f.comp[bad.idx, ]$dmudesc, f.comp[bad.idx, ]$compname, sep='-')
		assign('bad.components', value=cbind(coiid=bad.ids, component=bad.labels), envir=soilDB.env)
	}
	
  # print any messages on possible data quality problems:
  if(exists('bad.components', envir=soilDB.env))
    message("-> QC: horizon errors detected, use `get('bad.components', envir=soilDB.env)` for related coiid values")
  
  if(exists('tangled.comp.legend', envir=soilDB.env))
    message("-> QC: tangled [legend]--[correlation]--[data mapunit] links, use `get('bad.components', envir=soilDB.env)` for more information")
  
	# done, return SPC
	return(f.chorizon)
	
}


