## TODO: merge with other vegplot functions



#' Extract veg data from a local NASIS Database
#' 
#' Extract veg data from a local NASIS Database.
#' 
#' This function currently works only on Windows.
#' 
#' @param SS get data from the currently loaded Selected Set in NASIS or from
#' the entire local database (default: `TRUE`)
#' 
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: `NULL`
#' 
#' @return A list with the results.
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

# warning to use NASIS query to load related vegplot data for this to work
warning("In order to query this data you'll need to load all related vegplots to your sites and pedons in NASIS.", call. = FALSE)

  ## does this require tables that standard site/pedons queries don't hit?
  # https://github.com/ncss-tech/soilDB/issues/49
  # existing veg
  q.veg <- "SELECT siteiid, vegplotid, vegplotname, obsdate, primarydatacollector, datacollectionpurpose, assocuserpedonid, ppi.seqnum, plantsym, plantsciname, plantnatvernm, orderofdominance, speciescancovpct, speciescancovclass

  FROM site_View_1 AS s
  INNER JOIN siteobs_View_1 AS so ON so.siteiidref = s.siteiid
  LEFT JOIN vegplot_View_1 AS v on v.siteobsiidref = so.siteobsiid
  LEFT JOIN plotplantinventory_View_1 AS ppi ON ppi.vegplotiidref = v.vegplotiid
  -- note: plant table not managed by SS
  LEFT OUTER JOIN plant ON plant.plantiid = ppi.plantiidref;"

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q.veg <- gsub(pattern = '_View_1', replacement = '', x = q.veg, fixed = TRUE)
  }

  # existing veg
  #q.inventory <- "SELECT siteiid, vegplotid, vegplotname, obsdate, primarydatacollector, datacollectionpurpose, assocuserpedonid, #plotplantinventory.seqnum, plantsym, plantsciname, plantnatvernm, orderofdominance, speciescancovpct, speciescancovclass
  #
  #  FROM site_View_1 AS s
  #  INNER JOIN siteobs ON siteobs.siteiidref=s.siteiid
  #  LEFT JOIN vegplot_View_1 AS v on v.siteobsiidref=siteobs.siteobsiid
  #  LEFT JOIN plotplantinventory ON plotplantinventory.vegplotiidref=v.vegplotiid
  #  INNER JOIN plant ON plant.plantiid=plotplantinventory.plantiidref
  #  ORDER BY s.siteiid, plotplantinventory.seqnum;"
  
  # existing veg
  q.vegtransect <- "SELECT siteiid, vegplotid, vegplotname, obsdate, primarydatacollector, datacollectionpurpose, assocuserpedonid, vegtransectid, vegtransplantsummiid vtpsiid, transectlength, plantsym, plantsciname, plantnatvernm
  
    FROM site_View_1 AS s
    INNER JOIN siteobs ON siteobs.siteiidref=s.siteiid
    INNER JOIN vegplot_View_1 AS v on v.siteobsiidref=siteobs.siteobsiid
    LEFT JOIN vegtransect AS vt ON vt.vegplotiidref=v.vegplotiid
    LEFT JOIN vegtransectplantsummary AS vtps ON vtps.vegtransectiidref=vt.vegtransectiid
    INNER JOIN plant ON plant.plantiid=vtps.plantiidref
    ORDER BY s.siteiid;"
  
  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.vegtransect <- gsub(pattern = '_View_1', replacement = '', x = q.vegtransect, fixed = TRUE)
  }
  
  #q.plant <- "SELECT plantiid, plantsym
  #  FROM plant_View_1;"
  
  channel <- dbConnectNASIS(dsn)
  
  if (inherits(channel, 'try-error'))
    return(data.frame())
  
  # exec queries
  d.veg <-  dbQueryNASIS(channel, q.veg, close = FALSE)  
  d.vegtransect <- dbQueryNASIS(channel, q.vegtransect) 
  
  # return a list of results
  return(list(veg = d.veg,
              vegtransect = d.vegtransect))
}
