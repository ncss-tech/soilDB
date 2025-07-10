## TODO: merge with other vegplot functions



#' Get vegetation data from a local NASIS Database
#' 
#' Get vegetation data from a local NASIS Database. Result includes two data.frames corresponding to the "Plot Plant Inventory" and "Vegetation Transect" child tables of "Vegetation Plot".
#' 
#' @param SS get data from the currently loaded Selected Set in NASIS or from
#' the entire local database (default: `TRUE`)
#' 
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: `NULL`
#' 
#' @return A list of data.frame 
#' @author Jay M. Skovlin and Dylan E. Beaudette
#' @keywords manip
#' @examples
#' 
#' \donttest{
#' if(local_NASIS_defined()) {
#'  # query text note data
#'  v <- try(get_veg_from_NASIS_db())
#' 
#'  # show contents veg data returned
#'  str(v)
#' }
#' }
#' 
#' @export get_veg_data_from_NASIS_db
get_veg_data_from_NASIS_db <- function(SS=TRUE, dsn = NULL) {
  
  .soilDB_warn_deprecated_aliases(c("vegtransplantsummiid" = "vstpiid"))
  
  # existing veg
  # NOTE: left join of siteobs:vegplot ensures that all site/siteobs are in this result, 
  #       records will be NA filled when no vegplot available
  q.veg <- "SELECT siteiid, vegplotiid, vegplotid, vegplotname, obsdate, primarydatacollector, datacollectionpurpose, assocuserpedonid, ppi.seqnum, plantsym, plantsciname, plantnatvernm, orderofdominance, speciescancovpct, speciescancovclass
  FROM site_View_1 AS s
  INNER JOIN siteobs_View_1 AS so ON so.siteiidref = s.siteiid
  LEFT JOIN vegplot_View_1 AS v on v.siteobsiidref = so.siteobsiid
  LEFT JOIN plotplantinventory_View_1 AS ppi ON ppi.vegplotiidref = v.vegplotiid
  LEFT OUTER JOIN plant ON plant.plantiid = ppi.plantiidref;"

  # toggle selected set vs. local DB
  if (isFALSE(SS)) {
    q.veg <- gsub(pattern = '_View_1', replacement = '', x = q.veg, fixed = TRUE)
  }
  
  # veg transect
  q.vegtransect <- "SELECT siteiid, vegplotiid, vegplotid, vegplotname, obsdate, primarydatacollector, datacollectionpurpose, assocuserpedonid, vegtransectid, vegtransplantsummiid AS vtpsiid, vegtransplantsummiid, transectlength, plantsym, plantsciname, plantnatvernm
  
    FROM site_View_1 AS s
    INNER JOIN siteobs ON siteobs.siteiidref=s.siteiid
    INNER JOIN vegplot_View_1 AS v on v.siteobsiidref=siteobs.siteobsiid
    LEFT JOIN vegtransect AS vt ON vt.vegplotiidref=v.vegplotiid
    LEFT JOIN vegtransectplantsummary AS vtps ON vtps.vegtransectiidref=vt.vegtransectiid
    INNER JOIN plant ON plant.plantiid=vtps.plantiidref
    ORDER BY s.siteiid;"
  
  # toggle selected set vs. local DB
  if (isFALSE(SS)) {
    q.vegtransect <- gsub(pattern = '_View_1', replacement = '', x = q.vegtransect, fixed = TRUE)
  }
  
  channel <- dbConnectNASIS(dsn)
  
  if (inherits(channel, 'try-error'))
    return(list(veg = data.frame(), vegtransect = data.frame()))
  
  # exec queries
  d.veg <-  dbQueryNASIS(channel, q.veg, close = FALSE)  
  d.vegtransect <- dbQueryNASIS(channel, q.vegtransect) 
  
  # return a list of results
  return(list(veg = d.veg,
              vegtransect = d.vegtransect))
}
