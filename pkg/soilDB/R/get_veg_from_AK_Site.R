get_veg_from_AK_Site <- function(dsn) {
	# basic query
	q <- "SELECT VegStop.usiteid as site_id, localplant.lplantsym as plantsym, localplant.lplantname as plantname, vegetation.COVER as pct_cover
  FROM 
	(VegStop INNER JOIN vegetation ON VegStop.vegstopdbid = vegetation.vegstodbiidref)
	INNER JOIN localplant ON vegetation.lplantsym = localplant.lplantuid
	ORDER BY VegStop.usiteid ASC;"
  
	# setup connection to our pedon database
	channel <- odbcConnectAccess(dsn, readOnlyOptimize=TRUE)
	
	# exec query
	d <- sqlQuery(channel, q, stringsAsFactors=FALSE)
	
	# close connection
	odbcClose(channel)
	
	# done
	return(d)
}

