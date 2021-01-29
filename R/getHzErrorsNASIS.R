#' Check pedon horizon table for logic errors
#'
#' @param strict how strict should horizon boundaries be checked for consistency: TRUE=more | FALSE=less
#' @param SS 	fetch data from the currently loaded selected set in NASIS or from the entire local database (default: TRUE)
#' @param static_path Optional: path to local SQLite database containing NASIS table structure; default: NULL
#' @return A data.frame containing problematic records with columns: 'peiid','pedon_id','hzdept','hzdepb','hzname' 
#' @export
#'
getHzErrorsNASIS <- function(strict = TRUE, SS = TRUE, static_path = NULL) {
  
  if (!local_NASIS_defined(static_path))
    stop('Local NASIS ODBC connection has not been setup. Please see `http://ncss-tech.github.io/AQP/soilDB/setup_local_nasis.html`.')
  
	# get data
	site_data <- get_site_data_from_NASIS_db(SS = SS, static_path = static_path)
	site_data$pedon_id <- NULL
	hz_data <- get_hz_data_from_NASIS_db(SS = SS, static_path = static_path)
	
	if (nrow(site_data) == 0) {
	  message("No Site records in NASIS database")
  	return(data.frame(pedon_id = character(0), hz_logic_pass = logical(0)))
	}
	
	if (nrow(hz_data) == 0) {
	  message("No Pedon Horizon records in NASIS database")
	  return(data.frame(pedon_id = character(0), hz_logic_pass = logical(0)))
	}
	
	# combine pieces
	f <- merge(hz_data, site_data, by = "peiid", all.x = TRUE, sort = FALSE)
	
	f.test <- do.call('rbind', lapply(split(f, f$peiid), function(d) {
	  res <- aqp::hzDepthTests(top = d[['hzdept']], bottom = d[['hzdepb']])
	  if (length(res) > 0)
	    return(data.frame(pedon_id = d$pedon_id, hz_logic_pass = all(!res)))
	  return(data.frame(pedon_id = character(0), hz_logic_pass = logical(0)))
	}))
	
	# find bad ones
	bad.pedon.ids <- as.character(f.test$pedon_id[which(f.test$hz_logic_pass == FALSE)])
	
	# now describe the problems
	b <- f[which(f$pedon_id %in% bad.pedon.ids), c('peiid', 'pedon_id','hzdept','hzdepb','hzname')]
	
	return(b) 
	
}
