# gets all veg records per site ID


#' Get Vegetation Data from an AK Site Database
#' 
#' Get Vegetation Data from an AK Site Database
#' 
#' 
#' @param dsn file path the the AK Site access database
#' @return A data.frame with vegetation data in long format, linked to site ID.
#' @author Dylan E. Beaudette
#' @seealso \code{\link{get_hz_data_from_pedon_db}},
#' \code{\link{get_site_data_from_pedon_db}}
#' @keywords manip
#' @export get_veg_from_AK_Site
get_veg_from_AK_Site <- function(dsn) {
  # must have odbc installed
  if(!requireNamespace('odbc'))
    stop('please install the `odbc` package', call.=FALSE)
  
	# basic query
	q <- "SELECT VegStop.usiteid as site_id, 
	             VegStop.fECOSITE as ecosite, 
	             localplant.plantsym as plantsym, 
	             localplant.lplantname as plantname, 
	             vegetation.COVER as pct_cover
          FROM 
        	(VegStop INNER JOIN vegetation ON VegStop.vegstopdbid = vegetation.vegstodbiidref)
        	INNER JOIN localplant ON vegetation.plantsym = localplant.plantiidref
        	ORDER BY VegStop.usiteid ASC;"
  
	# setup connection to our pedon database
	channel <- DBI::dbConnect(odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", dsn))
	
	# exec query
	d <- DBI::dbGetQuery(channel, q)
	
	# close connection
	DBI::dbDisconnect(channel)
	
	# done
	return(d)
}

