#' Get Site Ecological Site History 
#' 
#' Gets the Site Ecological Site History data from local NASIS database. Used by [get_extended_data_from_NASIS_db()].
#' 
#' @param best Should the "best" ecological site correlation be chosen? Creates field called `es_selection_method` with `"most recent"` or `"least missing data"` for resolving many:1 relationships in site history.
#' @param SS Use selected set? Default: `TRUE`
#' @param es_classifier Optional: character. Vector of classifier names (and corresponding records) to retain in final result. 
#' @param dsn Path to SQLite data source, or a `DBIConnection` to database with NASIS schema.
#'
#' @seealso [get_extended_data_from_NASIS_db()]
#' 
#' @return a `data.frame`, or `NULL` on error
#' @export
get_ecosite_history_from_NASIS_db <- function(best = TRUE, SS = TRUE, es_classifier = NULL, dsn = NULL) {
  
  .SD <- NULL
  
  # ecological site
  q.ecosite <- "SELECT siteiidref AS siteiid, ecositeid, ecositenm, ecositecorrdate, seh.recwlupdated,
                       classifier AS [siteecositehistory.classifier]
  FROM siteecositehistory_View_1 AS seh
  LEFT OUTER JOIN ecologicalsite AS es ON es.ecositeiid=seh.ecositeiidref
  ORDER BY siteiid;"
  
  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.ecosite <- gsub(pattern = '_View_1', replacement = '', x = q.ecosite, fixed = TRUE)
  }
  
  channel <- dbConnectNASIS(dsn)
  
  if (inherits(channel, 'try-error'))
    return(NULL)
  
  ecositehistory <- dbQueryNASIS(channel, q.ecosite)
  
  if (!best) {
    # possibly contains multiple records per siteiid
    return(ecositehistory)
  }
  
  # load "best" siteecositehistory records: creates column 'es_selection_method' w/ "most recent" or "least missing data"
  as.data.frame(data.table::as.data.table(ecositehistory)[, .pickBestEcosite(.SD, es_classifier = es_classifier), by = list(siteiid = ecositehistory$siteiid)])
}
