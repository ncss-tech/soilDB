# updated to NASIS 6.2

# convenience function for loading most commonly used vegplot information from local NASIS database
fetchVegdata <- function(SS=TRUE, stringsAsFactors = default.stringsAsFactors()) {
	
  # test connection
  if(! 'nasis_local' %in% names(RODBC::odbcDataSources()))
    stop('Local NASIS ODBC connection has not been setup. Please see `http://ncss-tech.github.io/AQP/soilDB/setup_local_nasis.html`.')
	
	# 1. load data in pieces
	vegplot <- get_vegplot_from_NASIS_db(SS=SS, stringsAsFactors = stringsAsFactors)
	vegplotlocation <- get_vegplot_location_from_NASIS_db(SS=SS, stringsAsFactors = stringsAsFactors)
	vegplotrhi <- get_vegplot_trhi_from_NASIS_db(SS=SS, stringsAsFactors = stringsAsFactors)
	vegplotspecies <- get_vegplot_species_from_NASIS_db(SS=SS, stringsAsFactors = stringsAsFactors)
	vegtransect <- get_vegplot_transect_from_NASIS_db(SS=SS, stringsAsFactors = stringsAsFactors)
	vegtransplantsum <- get_vegplot_transpecies_from_NASIS_db(SS=SS, stringsAsFactors = stringsAsFactors)
	vegsiteindexsum <- get_vegplot_tree_si_summary_from_NASIS_db(SS=SS, stringsAsFactors = stringsAsFactors)
	vegsiteindexdet <- get_vegplot_tree_si_details_from_NASIS_db(SS=SS, stringsAsFactors = stringsAsFactors)
	vegplottext <- get_vegplot_textnote_from_NASIS_db(SS=SS, fixLineEndings=TRUE, stringsAsFactors = stringsAsFactors)


	# test to see if the selected set is loaded
	if (nrow(vegplot) == 0) message('your selected set is missing either the vegplot, pedon or site table, please load and try again :)')

	# done
	return(list(vegplot=vegplot,
		vegplotlocation=vegplotlocation,
		vegplotrhi=vegplotrhi,
		vegplotspecies=vegplotspecies,
		vegtransect=vegtransect,
		vegtransplantsum=vegtransplantsum,
		vegsiteindexsum=vegsiteindexsum,
		vegsiteindexdet=vegsiteindexdet,
		vegplottext=vegplottext))
}	

