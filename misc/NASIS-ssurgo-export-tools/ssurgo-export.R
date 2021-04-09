library(soilDB)

### SETUP
## In NASIS, go to Exports Explorer menu and Add New Export...
## 
## Tab #1: Criteria
## - Select Export Target: SSURGO
## - Choose desired map units, data map units, and components
## 
## Tabs #2 and #3: Interpretations and Notes
## - Select Interpretations to include in export
## - Select Text Notes to include in export
## 
## Tab #4: Run Export 
##  - Enter file name for ZIP
##  - Export run on server and result emailed to user
## 

#' Get Interpretation Rating "Reasons" from MS Access SSURGO Export
#'
#' @param dsn Path to access database template, pointed at NASIS export files.
#' @param mrulename Rule name of interpretation 
#' @param n Number of reasons to return
#'
#' @return A `data.frame` containing columns: "lmapunitiid", "coiid", "mrulename", "cokeyref", "Reasons", "liid", "muiid", "corriid", "dmuiid", "areasymbol", "musym", "compname", "comppct_r", "interphr", "interphrc","mukey"  
#' @examples 
#' 
#' library(data.table)
#' 
#' # dsn: path to MS Access template with a local SSURGO export (containing cointerp table for new mapunits)
#' dsn <- "E:/workspace/Cochran_InterpCompare/SWRtemplate/SWRtemplate.mdb"
#' 
#' # mrulename: select a rule name (must exist in export referenced by dsn)
#' mrulename <-  "WMS - Pond Reservoir Area" #"FOR - Mechanical Site Preparation (Surface)"
#' 
#' result <- data.table(.get_SSURGO_export_interp_reasons_by_mrulename(dsn, mrulename))
#' 
#' result_ssurgo <- get_SDA_interpretation(mrulename, 
#'                                         method = "NONE", 
#'                                         areasymbols = c("CA630","CA649"))
#' 
#' # result_dominant <- result[result[, .I[comppct_r == max(comppct_r)],by="mukey"]$V1,]
#' result_dominant <- result
#' 
#' x1 <- data.table(mukey = result_dominant$mukey, 
#'                  compname = result_dominant$compname,
#'                  rating_new = result_dominant$interphr,
#'                  rating_class = result_dominant$interphrc,
#'                  rating_new_reason = result_dominant$Reason)
#'                  
#' x2 <- data.table(mukey = result_ssurgo$MUKEY, 
#'                  compname = result_ssurgo$compname,
#'                  rating_old = result_ssurgo$rating,
#'                  rating_class = result_ssurgo$class)
#'                  
#' combined_result <- x2[x1, on = c("mukey","compname")]
#' 
#' combined_result$CHECK <- round(combined_result$rating_new, 2) == round(combined_result$rating_old, 2)
#' 
#' View(combined_result)       
#'          
.get_SSURGO_export_interp_reasons_by_mrulename <- function(dsn, mrulename, n = 2) {
  # based on VBA Function in Report Functions module of above .mdb
  # GetInterpReasons(strCokey As String, 
  #                  strMRuleName As String, 
  #                  intReasonCount As Integer) As Variant
  channel <- DBI::dbConnect(odbc::odbc(),
      .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", dsn))
  
  q <- sprintf("SELECT * FROM cointerp
                WHERE [mrulename] = '%s' AND [ruledepth] = 0
                ORDER BY [interphr] DESC", mrulename)
  cointerpbase <- data.table::as.data.table(DBI::dbGetQuery(channel, q))
  
  # TODO: allow extend reasons to rules with ruledepth > 1?
  q <- sprintf("SELECT cokey, interphrc FROM cointerp
                WHERE [mrulename] = '%s' AND [ruledepth] = 1 
                ORDER BY [interphr] DESC", mrulename)
  res <- data.table::as.data.table(DBI::dbGetQuery(channel, q))
  
  # identify the key components of the cointerp table to relate to NASIS
  cointerpkey <- data.frame(do.call('rbind', strsplit(cointerpbase$cointerpkey, ":")))
  colnames(cointerpkey) <- c("lmapunitiid","coiid","mrulekey","seqnum")
  
  # unique "cokey" is a composition of mukey (SSURGO) plus coiid (NASIS)
  cointerpbase$lmapunitiid <- as.integer(gsub("(\\d+):.*", "\\1", cointerpbase$cokey))
  res$lmapunitiid <- as.integer(gsub("(\\d+):.*", "\\1", res$cokey))
  cointerpbase$coiid <- as.integer(gsub(".*:(\\d+)", "\\1", cointerpbase$cokey))
  res$coiid <- as.integer(gsub(".*:(\\d+)", "\\1", res$cokey))
  
  # internal function to get the full set of ids from NASIS to alias to export
  .get_SSURGO_export_iid_table <- function(coiids) {  
    dbQueryNASIS(NASIS(), sprintf("
      SELECT liid, lmapunitiid, muiid, corriid, dmuiid, coiid, 
             areasymbol, musym, muname, compname, comppct_r 
        FROM area
        INNER JOIN legend ON legend.areaiidref = area.areaiid
        INNER JOIN lmapunit ON lmapunit.liidref = legend.liid
        INNER JOIN mapunit ON mapunit.muiid = lmapunit.muiidref
        INNER JOIN correlation ON correlation.muiidref = mapunit.muiid
        INNER JOIN datamapunit ON datamapunit.dmuiid = correlation.dmuiidref
        INNER JOIN component ON component.dmuiidref = datamapunit.dmuiid 
      WHERE component.coiid IN %s", format_SQL_in_statement(coiids)))
  }
  
  # get lookup table
  res2 <- .get_SSURGO_export_iid_table(cointerpkey$coiid)
  
  # extract the "high represenative" rating and class for 0th level rule
  high_rep_rating_class <- cointerpbase[,c("lmapunitiid","coiid","interphr","interphrc")]
  colnames(high_rep_rating_class) <- c("lmapunitiid","coiid","interphr","interphrc")
  
  # flatten the reasons so they are 1:1 with component, join to lookup tables 
  result <- as.data.frame(res[, list(mrulename = unique(mrulename),
                           cokeyref = unique(cokey), 
                           Reasons = paste0(.SD[["interphrc"]][1:pmin(.N, n)], collapse = "; ")),
                    by = c("lmapunitiid", "coiid")][res2, 
                                              on = c("lmapunitiid", "coiid")][high_rep_rating_class, 
                                                                        on = c("lmapunitiid","coiid")])
  
  # add mukey:lmapunitiid alias for convenience
  result$mukey <- result$lmapunitiid
  
  return(result)
}
