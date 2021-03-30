#' Extract species-level Data from a Montana RangeDB database
#' 
#' Get species-level data from a Montana RangeDB database.
#' 
#' This function currently works only on Windows.
#' 
#' @param dsn The name of the Montana RangeDB front-end database connection
#' (see details).
#' @return A data.frame.
#' @author Jay M. Skovlin
#' @seealso \code{\link{get_veg_from_MT_veg_db}},
#' \code{\link{get_veg_other_from_MT_veg_db}}
#' @keywords manip
#' @export get_veg_species_from_MT_veg_db
get_veg_species_from_MT_veg_db <- function(dsn) {
  # must have odbc installed
  if(!requireNamespace('odbc'))
    stop('please install the `odbc` package', call.=FALSE)
  
	#pull site and plot data and total production value
	q <- "SELECT tblESD_DK.PlotKey, tblESD_DK.DKKey, tblSites.SiteID AS site_id, SpeciesSymbol, Spec.CommonName as species_common_name, Spec.ScientificName as species_scientific, DKClass, Production 
  FROM (
	(
	(tblSites LEFT OUTER JOIN tblPlots ON tblSites.SiteKey = tblPlots.SiteKey) 
	LEFT OUTER JOIN tblESD_DK ON tblPlots.PlotKey = tblESD_DK.PlotKey) 
	LEFT OUTER JOIN (SELECT * FROM tblSpecies) AS Spec ON Spec.SpeciesCode = tblESD_DK.SpeciesSymbol)
	WHERE (DKClass IS NOT NULL)
	ORDER BY tblESD_DK.PlotKey, Production DESC;"
	
	# setup connection to our pedon database
	channel <- dbConnect(odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", dsn))
	
	# exec query
	d <- DBI::dbGetQuery(channel, q)
	
	# close connection
	DBI::dbDisconnect(channel)
	
	# done
	return(d)
}

