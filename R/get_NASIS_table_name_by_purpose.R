#' Return NASIS 7 Physical Table Names 
#' 
#' Draft method generalizing concepts of NASIS 7 data model to group tables by "purpose." Not intended for typical user-level, and not exported at this point. Most of our more complex queries rely on tables from one or more purposes, so individual higher-level functions might call a function like this to identify the relevant tables from some data source.
#' 
#' 
#' @param purpose character. One or more of: "metadata", "lookup", "site", "pedon", "transect", "datamapunit", "component", "vegetation"
#' @param SS append "_View_1" on appropriate tables? Default: FALSE
#'
#' @return character vector of table names
#' @seealso createStaticNASIS
#'
#' @examples
#' \dontrun{
#' # get the "site" table names
#' .get_NASIS_table_name_by_purpose("site")
#' 
#' # get the pedon table names
#' .get_NASIS_table_name_by_purpose("pedon", SS = TRUE)
#' 
#' # metadata and lookup not affected by SS argument, but site and pedon are
#' .get_NASIS_table_name_by_purpose(c("metadata", "lookup",
#'                                    "site", "pedon"), SS = TRUE)
#' }
#' 
.get_NASIS_table_name_by_purpose <- function(purpose = c("metadata", "lookup", "site", 
                                                         "pedon", "transect", "component", 
                                                         "vegetation", "project", 
                                                         "techsoilservice", "area",
                                                         "soilseries", "legend",
                                                         "mapunit", "datamapunit"),
                                             SS = FALSE) {
  
  # TODO: store as .rda?
  table_groups <- list(
    metadata = c(
      "MetadataDomainDetail",
      "MetadataDomainMaster",
      "MetadataTableColumn"
    ),
    lookup = c("ecologicalsite", "plant",
               "geomorfeat", "geomorfeattype"),
    site = c(
      "site",
      "siteaoverlap",
      "sitebedrock",
      "siteecositehistory",
      "sitegeomordesc",
      "sitepm",
      "sitetext",
      "siteobs",
      "siteerosionacc",
      "siteobstext",
      "sitesoilmoist",
      "sitesoiltemp",
      "sitesurffrags",
      "siteassoc",
      "siteassocsite",
      "siteassocsoi",
      "siteassoctext"
    ),
    pedon = c(
      "pedon",
      "pediagfeatures",
      "pefmp",
      "pehydricfieldindicator",
      "pepenetrationresistance",
      "perestrictions",
      "pesoilstability",
      "petaxhistory",
      "petext",
      "peinfiltrationsummary",
      "petaxhistfmmin",
      "petxhistfmother",
      "petaxhistmoistcl",
      "peinfiltrationch",
      "peinfiltrationfh",
      "peinfiltrationchdata",
      "peinfiltrationfhdata",
      "phorizon",
      "phcemagent",
      "phcolor",
      "phconcs",
      "phcracks",
      "phdesgnsuffix",
      "phfeatures",
      "phfmp",
      "phfrags",
      "phhuarts",
      "phmottles",
      "phpvsf",
      "phpores",
      "phrdxfeatures",
      "phroots",
      "phsample",
      "phstructure",
      "phtext",
      "phtexture",
      "phdb",
      "phdbcompliantcavity",
      "phdbcore",
      "phdbscoop",
      "phdbcorereading",
      "phdbscoopreading",
      "phconccolor",
      "phfeatcolor",
      "phpvsfcolor",
      "phredoxfcolor",
      "phtexturemod"
    ),
    transect = c("transect", "transectestcomposition",
                 "transecttext"),
    component = c(
      "component",
      "chorizon",
      "chtexturegrp",
      "chstructgrp",
      "cogeomordesc",
      "copmgrp",
      "copm",
      "coothvegclass",
      "othvegclass",
      "coecosite",
      "codiagfeatures",
      "corestrictions",
      "cocanopycover",
      "cocropyld",
      "coeplants",
      "coerosionacc",
      "coforprod",
      "coforprodo",
      "cohydcrit",
      "comonth",
      "copedon",
      "copwindbreak",
      "cosoilmoist",
      "cosoiltemp",
      "cosurffrags",
      "cosurfmorphgc",
      "cosurfmorphhpp",
      "cosurfmorphmr",
      "cosurfmorphss",
      "cotaxfmmin",
      "cotaxmoistcl",
      "cotext",
      "cotreestomng",
      "cotxfmother",
      "cousfsecoclass",
      "cousfsgrndcvr",
      "cousfsinterp",
      "cousfsirestrict"
    ),
    vegetation = c(
      "vegplot",
      "plotplantinventory",
      "vegtransect",
      "vegplottext",
      "vegtransectplantsummary",
      "plottreesiteindexsummary",
      "plottreesiteindexdetails",
      "comparativeyielddata",
      "comparativeyieldrefquadrats"
    ),
    project = c(
      "project",
      "projectconcern",
      "projectconcerntype",
      "projectcorrelation",
      "projectdataneed",
      "projectdatatype",
      "projectecologicalsite",
      "projectfieldreview",
      "projectlandcatbreakdown",
      "projectmappinggoal",
      "projectmappingprogress",
      "projectmapunit",
      "projectmilestone",
      "projectmilestoneprogress",
      "projectproduct",
      "projectstaff",
      "projecttext",
      "projecttype"
    ),
    techsoilservice = c(
      "techsoilservice",
      "techsoilservicetext",
      "techsoilservicesite"
    ),
    area = c("area","areatype"),
    soilseries = "soilseries",
    legend = c("legend", "ltext", "lmapunit"),
    mapunit = c("mapunit", "correlation", "mutext"),
    datamapunit = "datamapunit"
  )
  
  uses_View_1 <- list("metadata" = FALSE, 
                      "lookup" = FALSE, 
                      "site" = TRUE, 
                      "pedon" = TRUE, 
                      "transect" = TRUE, 
                      "component" = TRUE, 
                      "vegetation" = TRUE,
                      "project" = TRUE,
                      "techsoilservice" = TRUE, 
                      "area" = TRUE,
                      "soilseries" = FALSE,
                      "legend" = TRUE, 
                      "mapunit" = TRUE, 
                      "datamapunit" = TRUE)
  
  purpose <- match.arg(purpose, names(table_groups), several.ok = TRUE)
  
  res <- table_groups[purpose]
  
  res <- lapply(seq_along(res), function(i) {
      ni <- names(res)[i]
      if (uses_View_1[[ni]] && SS) 
        return(paste0(res[[ni]], "_View_1"))
      else return(res[[ni]])
    })
  
  return(as.character(unlist(res)))
}
