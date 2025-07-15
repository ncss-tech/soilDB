#' Get lab pedon data from a local NASIS Database
#' 
#' Get lab pedon-level data from a local NASIS database.
#' 
#' @param SS fetch data from the currently loaded selected set in NASIS or from
#' the entire local database (default: TRUE)
#' 
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: `NULL`
#' 
#' @return A data.frame.
#' 
#' @note This function queries KSSL laboratory site/horizon data from a local
#' NASIS database from the lab pedon data table.
#' 
#' @author Jay M. Skovlin and Dylan E. Beaudette
#' @seealso \code{\link{get_lablayer_data_from_NASIS_db}}
#' @keywords manip
#' @export get_labpedon_data_from_NASIS_db
get_labpedon_data_from_NASIS_db <- function(SS = TRUE, dsn = NULL) {

  .soilDB_warn_deprecated_aliases(c(ncsspedonlabdataiid = "labpeiid"))
  
  q.ncsslabpedon <- "SELECT peiidref AS peiid, upedonid, descname, taxonname, taxclname, ncsspedonlabdata_View_1.pedlabsampnum, psctopdepth, pscbotdepth, noncarbclaywtavg, claytotwtavg, le0to100, wf0175wtavgpsc, volfractgt2wtavg, cec7clayratiowtavg, labdatasheeturl, ncsspedonlabdataiid, ncsspedonlabdataiid AS labpeiid
  FROM (ncsspedonlabdata_View_1 LEFT OUTER JOIN pedon_View_1 ON ncsspedonlabdata_View_1.peiidref = pedon_View_1.peiid);"

  channel <- dbConnectNASIS(dsn)
  
  if (inherits(channel, 'try-error'))
    return(data.frame())

  # handle Views/selected set argument
  if (!SS)
    q.ncsslabpedon <- gsub(q.ncsslabpedon, pattern = "_View_1", replacement = "")

  d.labpedon <- dbQueryNASIS(channel, q.ncsslabpedon)

	# return a list of results
	return(d.labpedon)
}
