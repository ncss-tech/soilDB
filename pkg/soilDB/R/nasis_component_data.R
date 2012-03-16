##
## 13 March, 2012
## D.E. Beaudette
## 
## prototype functions for extracting component data from local NASIS
##

get_component_data_from_NASIS_db <- function() {
	q <- "SELECT dmudesc, coiid, comppct_r, compname, localphase, compkind, majcompflag 
	FROM datamapunit 
	INNER JOIN component ON datamapunit.dmuiid = component.dmuiidref
	ORDER BY dmudesc"
	
	# setup connection to our pedon database
	channel <- odbcConnect('nasis_local', uid='NasisSqlRO', pwd='Re@d0n1y')
	
	# exec query
	d <- sqlQuery(channel, q, stringsAsFactors=FALSE)
	
	# close connection
	odbcClose(channel)
	
	# done
	return(d)
}


get_component_horizon_data_from_NASIS_db <- function() {
	q <- "SELECT chiid, coiidref as coiid, hzname, hzdept_r, hzdepb_r, fragvoltot_r, sandtotal_r, silttotal_r, claytotal_r, om_r, dbovendry_r, ksat_r, awc_r, lep_r, sar_r, ec_r, cec7_r, sumbases_r, ph1to1h2o_r
	FROM chorizon ORDER BY coiidref, hzdept_r ASC;"
	
	# setup connection to our pedon database
	channel <- odbcConnect('nasis_local', uid='NasisSqlRO', pwd='Re@d0n1y')
	
	# exec query
	d <- sqlQuery(channel, q, stringsAsFactors=FALSE)
	
	# close connection
	odbcClose(channel)
	
	# done
	return(d)
}


.fetchNASIS_component_data <- function() {
	
	# get component table
	f.comp <- get_component_data_from_NASIS_db()
	
	# get chorizon table
	f.chorizon <- get_component_horizon_data_from_NASIS_db()
	
	# join
	f <- join(f.comp, f.chorizon, by='coiid')
	
	cat('finding horizonation errors ...\n')
	f.test <- ddply(f, .(coiid), test_hz_logic, topcol='hzdept_r', bottomcol='hzdepb_r', strict=TRUE)
	
	# which are the good (valid) ones?
	good.ids <- as.character(f.test$coiid[which(f.test$hz_logic_pass)])
	bad.ids <- as.character(f.test$coiid[which(f.test$hz_logic_pass == FALSE)])
	
	# mention bad pedons
	if(length(bad.ids) > 0)
		cat(paste('horizon errors in:', paste(bad.ids, collapse=','), '\n'))
	
	# keep the good ones
	f <- subset(f, coiid %in% good.ids)
	
	# init SPC from chorizon data
	depths(f) <- coiid ~ hzdept_r + hzdepb_r
	
	# move site data
	site(f) <- ~ dmudesc + comppct_r + compname + localphase + compkind + majcompflag
	
	# done, return SPC
	return(f)
	
}


