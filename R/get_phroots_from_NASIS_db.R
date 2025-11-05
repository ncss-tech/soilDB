

#' @title Get pedon horizon roots data from a local NASIS Database
#' 
#' @description This function returns records from the `phroots` table of a local NASIS database. Pedon and pedon horizon record IDs are also included for linking back to related records, typically queried via [fetchNASIS()].
#'
#' @param SS logical, limit query to the selected set
#'
#' @param dsn optional path or _DBIConnection_ to \link[=NASISLocalDatabase]{local database containing NASIS table structure}; default: `NULL`
#' 
#' @return A `data.frame`
#' 
#' @export
#' 
get_phroots_from_NASIS_db <- function(SS = TRUE, dsn = NULL) {
  
  # pedon horizon roots
  # unique-ness enforced via peiid (pedon-level) and phiid (horizon-level)
  q <- "SELECT peiid, phiid,
  phroots_View_1.seqnum, rootsquantity, rootsquantityclass, rootssize, rootslocation
  FROM
  pedon_View_1
  INNER JOIN phorizon_View_1 ON pedon_View_1.peiid = phorizon_View_1.peiidref
  INNER JOIN phroots_View_1 ON phorizon_View_1.phiid = phroots_View_1.phiidref
  ORDER BY phiid, phroots_View_1.seqnum;"
  
  channel <- dbConnectNASIS(dsn)
  
  # error condition, NULL
  if (inherits(channel, 'try-error')) {
    return(NULL)
  }
  
  # toggle selected set vs. local DB
  if (isFALSE(SS)) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }
  
  # exec queries
  d <- dbQueryNASIS(channel, q)
  
  # convert coded -> text/factor representation of values
  d <- uncode(d, dsn = dsn)
  
  # encode ordered factors
 d$rootsquantityclass <- NASISChoiceList(d["rootsquantityclass"])
  
  
  # done
  return(d)
}
