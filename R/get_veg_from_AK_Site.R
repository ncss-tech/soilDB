# gets all veg records per site ID


#' Retrieve Vegetation Data from an AK Site Database
#' 
#' Retrieve Vegetation Data from an AK Site Database
#' 
#' 
#' @param dsn file path the the AK Site access database
#' @return A data.frame with vegetation data in long format, linked to site ID.
#' @note This function currently works only on Windows.
#' @author Dylan E. Beaudette
#' @seealso \code{\link{get_hz_data_from_pedon_db}},
#' \code{\link{get_site_data_from_pedon_db}}
#' @keywords manip
#' @export get_veg_from_AK_Site
get_veg_from_AK_Site <- function(dsn) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
	# basic query
	q <- "SELECT VegStop.usiteid as site_id, VegStop.fECOSITE as ecosite, localplant.lplantsym as plantsym, localplant.lplantname as plantname, vegetation.COVER as pct_cover
  FROM 
	(VegStop INNER JOIN vegetation ON VegStop.vegstopdbid = vegetation.vegstodbiidref)
	INNER JOIN localplant ON vegetation.lplantsym = localplant.lplantuid
	ORDER BY VegStop.usiteid ASC;"
  
	# setup connection to the AK Site database
	channel <- RODBC::odbcConnectAccess(dsn, readOnlyOptimize=TRUE)
	
	# exec query
	d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
	
	# close connection
	RODBC::odbcClose(channel)
	
	# done
	return(d)
}

