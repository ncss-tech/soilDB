
#' Get Logic Errors in NASIS/PedonPC Pedon Horizon  
#' @param strict how strict should horizon boundaries be checked for
#' consistency: TRUE=more | FALSE=less
#' @param SS fetch data from the currently loaded selected set in NASIS or from
#' the entire local database (default: TRUE)
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: NULL
#' @return A data.frame containing problematic records with columns:
#' 'peiid','upedonid','hzdept','hzdepb','hzname'
#' @export 
getHzErrorsNASIS <- function(strict = TRUE, SS = TRUE, dsn = NULL) {

  
  # check if NASIS local DB instance/ODBC data source is available
  .soilDB_test_NASIS_connection(dsn = dsn)
  
	# get data
	site_data <- get_site_data_from_NASIS_db(SS = SS, dsn = dsn)
	site_data$upedonid <- NULL
	hz_data <- get_hz_data_from_NASIS_db(SS = SS, dsn = dsn)

	if (nrow(site_data) == 0) {
	  message("No Site records in NASIS database")
  	return(data.frame(upedonid = character(0), valid = logical(0)))
	}

	if (nrow(hz_data) == 0) {
	  message("No Pedon Horizon records in NASIS database")
	  return(data.frame(upedonid = character(0), valid = logical(0)))
	}

	# combine pieces
	f <- merge(hz_data, site_data, by = "peiid", all.x = TRUE, sort = FALSE)

	f.test <- aqp::checkHzDepthLogic(f, hzdepths = c('hzdept', 'hzdepb'), idname = 'peiid', fast = TRUE)

	# find bad ones
	bad.pedon.ids <- as.character(f.test$upedonid[which(!f.test$valid)])

	# now describe the problems
	b <- f[which(f$pedon_id %in% bad.pedon.ids), c('peiid', 'upedonid','hzdept','hzdepb','hzname')]

	return(b)

}
