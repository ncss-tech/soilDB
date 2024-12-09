# updated to NASIS 6.2

# convenience function for loading most commonly used vegplot information from local NASIS database
#' Get vegetation plot data from local NASIS database
#' @aliases get_vegplot_from_NASIS_db
#' get_vegplot_location_from_NASIS_db get_vegplot_species_from_NASIS_db
#' get_vegplot_textnote_from_NASIS_db get_vegplot_transect_from_NASIS_db
#' get_vegplot_transpecies_from_NASIS_db
#' get_vegplot_tree_si_details_from_NASIS_db
#' get_vegplot_tree_si_summary_from_NASIS_db get_vegplot_trhi_from_NASIS_db
#'
#' @param SS fetch data from the currently loaded selected set in NASIS or from the entire local database (default: `TRUE`)
#' @param include_pedon Include pedon and transect data joined to site? (default: `TRUE`)
#' @param stringsAsFactors deprecated
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: `NULL`
#'
#' @return A named list containing: "vegplot", "vegplotlocation", "vegplotrhi", "vegplotspecies", "vegtransect", "vegtransplantsum", 'vegsiteindexsum', "vegsiteindexdet", "vegbasalarea", and  "vegplottext" tables
#'
#' @export
#'
fetchVegdata <- function(SS=TRUE, include_pedon = TRUE, stringsAsFactors = NULL, dsn = NULL) {

  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }


  # check if NASIS local DB instance/ODBC data source is available
  .soilDB_test_NASIS_connection(dsn = dsn)

	# 1. load data in pieces
	site <- get_site_data_from_NASIS_db(SS = SS, include_pedon = include_pedon, dsn = dsn)
  vegplot <- get_vegplot_from_NASIS_db(SS = SS, dsn =  dsn)
  vegplotlocation <- get_vegplot_location_from_NASIS_db(SS = SS,  dsn =  dsn)
  vegplotrhi <-  get_vegplot_trhi_from_NASIS_db(SS = SS, dsn =  dsn)
  vegplotspecies <- get_vegplot_species_from_NASIS_db(SS = SS, dsn =  dsn)
  vegtransect <- get_vegplot_transect_from_NASIS_db(SS = SS, dsn =  dsn)
  vegtransplantsum <- get_vegplot_transpecies_from_NASIS_db(SS = SS, dsn =  dsn)
  vegtranspoint <- get_vegplot_transpoints_from_NASIS_db(SS = SS, dsn = dsn)
  vegprodquadrat <- get_vegplot_prodquadrats_from_NASIS_db(SS = SS, dsn =  dsn)
  vegsiteindexsum <- get_vegplot_tree_si_summary_from_NASIS_db(SS = SS, dsn =  dsn)
  vegsiteindexdet <- get_vegplot_tree_si_details_from_NASIS_db(SS = SS, dsn =  dsn)
  vegbasalarea <- get_vegplot_speciesbasalarea_from_NASIS(SS = SS, dsn =  dsn)
  vegplottext <-  get_vegplot_textnote_from_NASIS_db(SS = SS, fixLineEndings = TRUE, dsn =  dsn)


	# test to see if the selected set is loaded
	if (nrow(site) == 0 || nrow(vegplot) == 0) {
	  message('Selected set is missing either the vegplot, pedon or site table, please load and try again :)')
	}

  # add ecosite id, corrdate, selection method to vegplot
  vegplot <- merge(site[,c("siteiid", "ecositeid", "peiid", "ecositecorrdate", "siteecositehistory.classifier", "es_selection_method")],
                   vegplot, by = "siteiid", all.x = TRUE, sort = FALSE)

	# done
	return(list(
	    vegplot = vegplot,
	    vegplotlocation = vegplotlocation,
	    vegplotrhi = vegplotrhi,
	    vegplotspecies = vegplotspecies,
	    vegtransect = vegtransect,
	    vegtransplantsum = vegtransplantsum,
	    vegtranspoint = vegtranspoint,
	    vegprodquadrat = vegprodquadrat,
	    vegsiteindexsum = vegsiteindexsum,
	    vegsiteindexdet = vegsiteindexdet,
	    vegbasalarea = vegbasalarea,
	    vegplottext = vegplottext,
	    site = site
	  ))
}

