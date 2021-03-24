#' Get the foreign key by table name
#'
#' @param tables character vector of table names
#'
#' @return The foreign key column name for the specified table name
#'
#' @examples
#' \dontrun{
#' .get_NASIS_table_fkey_by_name(c("site","phorizon_View_1","not_a_table"))
#' }
.get_NASIS_table_fkey_by_name <- function(tables) {
  fkeys <- list(
      site = "siteiid",
      siteaoverlap = "siteiid",
      sitebedrock = "siteiid",
      siteecositehistory = "siteiid",
      sitegeomordesc = "siteiid",
      sitepm = "siteiid",
      sitetext = "siteiid",
      siteobs = "siteiid",
      siteerosionacc = "siteobsiid",
      siteexistveg = "siteobsiid",
      siteobstext = "siteobsiid",
      sitesoilmoist = "siteobsiid",
      sitesoiltemp = "siteobsiid",
      sitesurffrags = "siteobsiid",
      transect = "tsectiid",
      transectestcomposition = "tsectiid",
      transecttext = "tsectiid",
      siteassoc = "siteassociid",
      siteassocsite = "siteiid",
      siteassocsoi = "siteiid",
      siteassoctext = "siteassociid",
      pedon = "peiid",
      pediagfeatures = "peiid",
      pefmp = "peiid",
      pehydricfieldindicator = "peiid",
      pepenetrationresistance = "peiid",
      perestrictions = "peiid",
      pesoilstability = "peiid",
      petaxhistory = "peiid",
      petext = "peiid",
      peinfiltrationsummary = "peiid",
      petaxhistfmmin = "pedtaxhistoryiid",
      petxhistfmother = "pedtaxhistoryiid",
      petaxhistmoistcl = "pedtaxhistoryiid",
      peinfiltrationch = "peinfilsumiid",
      peinfiltrationfh = "peinfilsumiid",
      peinfiltrationchdata = "peinfilconstheadiid",
      peinfiltrationfhdata = "peinfilfallheadiid",
      phorizon = "peiid",
      phcemagent = "phiid",
      phcolor = "phiid",
      phconcs = "phiid",
      phcracks = "phiid",
      phdesgnsuffix = "phiid",
      phfeatures = "phiid",
      phfmp = "phiid",
      phfrags = "phiid",
      phhuarts = "phiid",
      phmottles = "phiid",
      phpvsf = "phiid",
      phpores = "phiid",
      phrdxfeatures = "phiid",
      phroots = "phiid",
      phsample = "phiid",
      phstructure = "phiid",
      phtext = "phiid",
      phtexture = "phiid",
      phdb = "phiid",
      phdbcompliantcavity = "phbulkdensityiid",
      phdbcore = "phbulkdensityiid",
      phdbscoop = "phbulkdensityiid",
      phdbcorereading = "phbulkdencoreiid",
      phdbscoopreading = "phbulkdenscoopiid",
      phconccolor = "phconceniid",
      phfeatcolor = "phfeatsiid",
      phpvsfcolor = "phpvsfiid",
      phredoxfcolor = "phrdxfiid",
      phtexturemod = "phtiid"
    )
  
  # the same foreign keys are used in the selected set
  tables_search <- gsub("_View_1", "", tables)
  res <- fkeys[match(tables_search, names(fkeys))]
  res <- unlist(lapply(res, function(x) if(length(x) == 0) { return(NA) } else { return(x) }), recursive = FALSE)
  names(res) <- tables
  return(res)
}
