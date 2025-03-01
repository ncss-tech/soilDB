# Based on ssurgoOnDemand by chad ferguson and jason nemecek
# SDA_pmgroupname.R: translation of SDA_pmgrpname_dom_comp.py into soilDB-style R function by andrew brown
# last update: 2021/04/03

#' Get map unit parent material group information from Soil Data Access
#'
#'@details Default `method` is `"Dominant Component"` to get the dominant component (highest percentage). Use `"Dominant Condition"` or dominant parent material condition (similar conditions aggregated across components). Use `"None"` for no aggregation (one record per component).
#'
#' @param areasymbols _character_. Vector of soil survey area symbols
#' @param mukeys _integer_. Vector of map unit keys
#' @param WHERE _character_. SQL WHERE clause specified in terms of fields in `legend`, `mapunit`, `component`, or `copmgrp` tables, used in lieu of `mukeys` or `areasymbols`
#' @param method _character_. One of: `"Dominant Component"`, `"Dominant Condition"`, `"None"`
#' @param simplify _logical_. Group into generalized parent material groups? Default `TRUE`
#' @param include_minors logical. Include minor components? Default: `TRUE`.
#' @param miscellaneous_areas _logical_. Include miscellaneous areas (non-soil components) in results? Default: `FALSE`. 
#' @param query_string Default: `FALSE`; if `TRUE` return a character string containing query that would be sent to SDA via `SDA_query`
#' @param dsn Path to local SQLite database or a DBIConnection object. If `NULL` (default) use Soil Data Access API via `SDA_query()`.
#' @author Jason Nemecek, Chad Ferguson, Andrew Brown
#' @return a data.frame
#' @export
get_SDA_pmgroupname <- function(areasymbols = NULL,
                                mukeys = NULL,
                                WHERE = NULL,
                                method = "DOMINANT COMPONENT",
                                simplify = TRUE,
                                include_minors = TRUE,
                                miscellaneous_areas = FALSE,
                                query_string = FALSE,
                                dsn = NULL) {


        method <- match.arg(toupper(method), c("DOMINANT COMPONENT", "DOMINANT CONDITION", "NONE"))

        if (is.null(mukeys) && is.null(areasymbols) && is.null(WHERE)) {
          stop("Please specify one of the following arguments: mukeys, areasymbols, WHERE", call. = FALSE)
        }

        if (!is.null(mukeys)) {
          WHERE <- paste("mapunit.mukey IN", format_SQL_in_statement(as.integer(mukeys)))
        } else if (!is.null(areasymbols)) {
          WHERE <- paste("legend.areasymbol IN", format_SQL_in_statement(areasymbols))
        }

        case_pmgroupname <- "
             CASE WHEN pmgroupname LIKE '%Calcareous loess%' THEN 'Eolian Deposits (nonvolcanic)'
             WHEN pmgroupname LIKE '%Eolian deposits%' THEN 'Eolian Deposits (nonvolcanic)'
             WHEN pmgroupname LIKE '%Eolian sands%' THEN 'Eolian Deposits (nonvolcanic)'
             WHEN pmgroupname LIKE '%Loess%' THEN 'Eolian Deposits (nonvolcanic)'
             WHEN pmgroupname LIKE '%Noncalcareous loess%' THEN 'Eolian Deposits (nonvolcanic)'
             WHEN pmgroupname LIKE '%Ablation till%' THEN 'Glacial and Periglacial Deposits'
             WHEN pmgroupname LIKE '%Basal till%' THEN 'Glacial and Periglacial Deposits'
             WHEN pmgroupname LIKE '%Cryoturbate%' THEN 'Glacial and Periglacial Deposits'
             WHEN pmgroupname LIKE '%Drift%' THEN 'Glacial and Periglacial Deposits'
             WHEN pmgroupname LIKE '%Flow till%' THEN 'Glacial and Periglacial Deposits'
             WHEN pmgroupname LIKE '%Glaciofluvial deposits%' THEN 'Glacial and Periglacial Deposits'
             WHEN pmgroupname LIKE '%Glaciolacustrine deposits%' THEN 'Glacial and Periglacial Deposits'
             WHEN pmgroupname LIKE '%Glaciomarine deposits%' THEN 'Glacial and Periglacial Deposits'
             WHEN pmgroupname LIKE '%Lodgment till%' THEN 'Glacial and Periglacial Deposits'
             WHEN pmgroupname LIKE '%Melt-out till%' THEN 'Glacial and Periglacial Deposits'
             WHEN pmgroupname LIKE '%Outwash%' THEN 'Glacial and Periglacial Deposits'
             WHEN pmgroupname LIKE '%Solifluction deposits%' THEN 'Glacial and Periglacial Deposits'
             WHEN pmgroupname LIKE '%Subglacial till%' THEN 'Glacial and Periglacial Deposits'
             WHEN pmgroupname LIKE '%Supraglacial meltout till%' THEN 'Glacial and Periglacial Deposits'
             WHEN pmgroupname LIKE '%Supraglacial till%' THEN 'Glacial and Periglacial Deposits'
             WHEN pmgroupname LIKE '%Till%' THEN 'Glacial and Periglacial Deposits'
             WHEN pmgroupname LIKE '%Bauxite%' THEN 'In-Place Deposits (nontransported)'
             WHEN pmgroupname LIKE '%Grus%' THEN 'In-Place Deposits (nontransported)'
             WHEN pmgroupname LIKE '%Residuum%' THEN 'In-Place Deposits (nontransported)'
             WHEN pmgroupname LIKE '%Saprolite%' THEN 'In-Place Deposits (nontransported)'
             WHEN pmgroupname LIKE '%Colluvium%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Complex landslide deposits%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Creep deposits%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Debris avalanche deposits%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Debris flow deposits%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Debris slide deposits%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Debris spread deposits%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Debris topple deposits%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Earth spread deposits%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Earthflow deposits%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Flow deposits%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Lateral spread deposits%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Mass movement deposits%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Mudflow deposits%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Rock spread deposits%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Rock topple deposits%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Rockfall avalanche deposits%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Rockfall deposits%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Rotational earth slide deposits%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Rotational slide deposits%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Sand flow deposits%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Scree%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Slide deposits%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Talus%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Topple deposits%' THEN 'Mass Movement Deposits'
             WHEN pmgroupname LIKE '%Diamicton%' THEN 'Miscellaneous Deposits'
             WHEN pmgroupname LIKE '%Coprogenic material%' THEN 'Organic Deposits'
             WHEN pmgroupname LIKE '%Grassy organic material%' THEN 'Organic Deposits'
             WHEN pmgroupname LIKE '%Herbaceous organic material%' THEN 'Organic Deposits'
             WHEN pmgroupname LIKE '%Mossy organic material%' THEN 'Organic Deposits'
             WHEN pmgroupname LIKE '%Organic material%' THEN 'Organic Deposits'
             WHEN pmgroupname LIKE '%Woody organic material%' THEN 'Organic Deposits'
             WHEN pmgroupname LIKE '%Acidic volcanic ash%' THEN 'Volcanic Deposits (unconsolidated; eolian and mass movement)'
             WHEN pmgroupname LIKE '%Andesitic volcanic ash%' THEN 'Volcanic Deposits (unconsolidated; eolian and mass movement)'
             WHEN pmgroupname LIKE '%Ash flow%' THEN 'Volcanic Deposits (unconsolidated; eolian and mass movement)'
             WHEN pmgroupname LIKE '%Basaltic volcanic ash%' THEN 'Volcanic Deposits (unconsolidated; eolian and mass movement)'
             WHEN pmgroupname LIKE '%Basic volcanic ash%' THEN 'Volcanic Deposits (unconsolidated; eolian and mass movement)'
             WHEN pmgroupname LIKE '%Cinders%' THEN 'Volcanic Deposits (unconsolidated; eolian and mass movement)'
             WHEN pmgroupname LIKE '%Lahar deposits%' THEN 'Volcanic Deposits (unconsolidated; eolian and mass movement)'
             WHEN pmgroupname LIKE '%Pumice%' THEN 'Volcanic Deposits (unconsolidated; eolian and mass movement)'
             WHEN pmgroupname LIKE '%Pyroclastic flow%' THEN 'Volcanic Deposits (unconsolidated; eolian and mass movement)'
             WHEN pmgroupname LIKE '%Pyroclastic surge%' THEN 'Volcanic Deposits (unconsolidated; eolian and mass movement)'
             WHEN pmgroupname LIKE '%Scoria%' THEN 'Volcanic Deposits (unconsolidated; eolian and mass movement)'
             WHEN pmgroupname LIKE '%Tephra%' THEN 'Volcanic Deposits (unconsolidated; eolian and mass movement)'
             WHEN pmgroupname LIKE '%tuff%' THEN 'Volcanic Deposits (unconsolidated; eolian and mass movement)'
             WHEN pmgroupname LIKE '%tuff-breccia%' THEN 'Volcanic Deposits (unconsolidated; eolian and mass movement)'
             WHEN pmgroupname LIKE '%Volcanic ash%' THEN 'Volcanic Deposits (unconsolidated; eolian and mass movement)'
             WHEN pmgroupname LIKE '%alluvium%' THEN 'Waterlaid (or Transported) Deposits'
             WHEN pmgroupname LIKE '%Alluvium%' THEN 'Waterlaid (or Transported) Deposits'
             WHEN pmgroupname LIKE '%Backswamp deposits%' THEN 'Waterlaid (or Transported) Deposits'
             WHEN pmgroupname LIKE '%Beach sand%' THEN 'Waterlaid (or Transported) Deposits'
             WHEN pmgroupname LIKE '%Diatomaceous earth%' THEN 'Waterlaid (or Transported) Deposits'
             WHEN pmgroupname LIKE '%Estuarine deposits%' THEN 'Waterlaid (or Transported) Deposits'
             WHEN pmgroupname LIKE '%Fluviomarine deposits%' THEN 'Waterlaid (or Transported) Deposits'
             WHEN pmgroupname LIKE '%Greensands%' THEN 'Waterlaid (or Transported) Deposits'
             WHEN pmgroupname LIKE '%Lacustrine deposits%' THEN 'Waterlaid (or Transported) Deposits'
             WHEN pmgroupname LIKE '%Lagoonal deposits%' THEN 'Waterlaid (or Transported) Deposits'
             WHEN pmgroupname LIKE '%Marine deposits%' THEN 'Waterlaid (or Transported) Deposits'
             WHEN pmgroupname LIKE '%Marl%' THEN 'Waterlaid (or Transported) Deposits'
             WHEN pmgroupname LIKE '%Overbank deposits%' THEN 'Waterlaid (or Transported) Deposits'
             WHEN pmgroupname LIKE '%Pedisediment%' THEN 'Waterlaid (or Transported) Deposits'
             WHEN pmgroupname LIKE '%Slope alluvium%' THEN 'Waterlaid (or Transported) Deposits'
             WHEN pmgroupname LIKE '%Valley side alluvium%' THEN 'Waterlaid (or Transported) Deposits'
             WHEN pmgroupname LIKE '%Coal extraction mine spoil%' THEN 'Anthropogenic Deposits'
             WHEN pmgroupname LIKE '%Dredge spoils%' THEN 'Anthropogenic Deposits'
             WHEN pmgroupname LIKE '%Human-transported material%' THEN 'Anthropogenic Deposits'
             WHEN pmgroupname LIKE '%Metal ore extraction mine spoil%' THEN 'Anthropogenic Deposits'
             WHEN pmgroupname LIKE '%Mine spoil or earthy fill%' THEN 'Anthropogenic Deposits'
             WHEN pmgroupname LIKE '%aa%' THEN 'Miscoded - should be pmorigin'
             WHEN pmgroupname LIKE '%breccia-basic%' THEN 'Miscoded - should be pmorigin'
             WHEN pmgroupname LIKE '%conglomerate%' THEN 'Miscoded - should be pmorigin'
             WHEN pmgroupname LIKE '%dolomite%' THEN 'Miscoded - should be pmorigin'
             WHEN pmgroupname LIKE '%igneous%' THEN 'Miscoded - should be pmorigin'
             WHEN pmgroupname LIKE '%limestone%' THEN 'Miscoded - should be pmorigin'
             WHEN pmgroupname LIKE '%limestone-shale%' THEN 'Miscoded - should be pmorigin'
             WHEN pmgroupname LIKE '%metamorphic%' THEN 'Miscoded - should be pmorigin'
             WHEN pmgroupname LIKE '%quartzite%' THEN 'Miscoded - should be pmorigin'
             WHEN pmgroupname LIKE '%sandstone%' THEN 'Miscoded - should be pmorigin'
             WHEN pmgroupname LIKE '%sedimentary%' THEN 'Miscoded - should be pmorigin'
             WHEN pmgroupname LIKE '%serpentine%' THEN 'Miscoded - should be pmorigin'
             WHEN pmgroupname LIKE '%shale%' THEN 'Miscoded - should be pmorigin'
             WHEN pmgroupname LIKE '%shale-calcareous%' THEN 'Miscoded - should be pmorigin'
             WHEN pmgroupname LIKE '%siltstone%' THEN 'Miscoded - should be pmorigin'
             WHEN pmgroupname LIKE '%mixed%' THEN 'Miscellaneous Deposits'
             WHEN pmgroupname LIKE '%NULL%' THEN NULL ELSE NULL END AS pmgroupname"

        if (!simplify) {
                case_pmgroupname <- "pmgroupname"
        }


        if (method %in% c("DOMINANT COMPONENT", "DOMINANT CONDITION")) {
                dcq <- sprintf("SELECT c1.cokey FROM component AS c1
                                INNER JOIN mapunit AS mu1 ON c1.mukey = mu1.mukey AND c1.mukey = mapunit.mukey %s %s
                                ORDER BY c1.comppct_r DESC, c1.cokey ",
                               ifelse(include_minors, "", " AND c1.majcompflag = 'Yes'"),
                               ifelse(miscellaneous_areas, "", " AND NOT c1.compkind = 'Miscellaneous area'"))
                comp_selection <- sprintf("AND component.cokey = (%s)", .LIMIT_N(dcq, n = 1, sqlite = !is.null(dsn)))
        } else {
                comp_selection <- ""
        }

        if (method == "DOMINANT CONDITION") {
                dcq <- sprintf("SELECT pmgroupname FROM mapunit AS mu
                INNER JOIN component AS c1 ON c1.mukey = mapunit.mukey AND mapunit.mukey = mu.mukey %s %s
                INNER JOIN copmgrp ON copmgrp.cokey = component.cokey 
                GROUP BY pmgroupname, comppct_r ORDER BY SUM(comppct_r) OVER (PARTITION BY pmgroupname) DESC",
                               ifelse(include_minors, "", " AND c1.majcompflag = 'Yes'"),
                               ifelse(miscellaneous_areas, "", " AND NOT c1.compkind = 'Miscellaneous area'"))
                pm_selection <- sprintf("AND pmgroupname = (%s)", .LIMIT_N(dcq, n = 1, sqlite = !is.null(dsn)))
        } else {
                pm_selection <- ""
        }

        q <- sprintf(
                paste0("SELECT DISTINCT
                         mapunit.mukey,
                         legend.areasymbol AS areasymbol,
                         mapunit.musym AS musym,
                         mapunit.muname AS muname,",
                         ifelse(method == "DOMINANT CONDITION", "", "compname, compkind, comppct_r, majcompflag,"),
                         "%s
                         FROM legend
                         INNER JOIN mapunit ON mapunit.lkey = legend.lkey AND %s
                         LEFT JOIN component ON component.mukey = mapunit.mukey %s %s %s
                         LEFT JOIN copmgrp ON copmgrp.cokey = component.cokey %s"),
                case_pmgroupname,
                WHERE,
                comp_selection, 
                ifelse(include_minors, "", " AND component.majcompflag = 'Yes'"),
                ifelse(miscellaneous_areas, "", " AND NOT component.compkind = 'Miscellaneous area'"),
                pm_selection
        )

   if (query_string) {
           return(q)
   }

   # execute query
   res <- SDA_query(q, dsn = dsn)
   
   # return if bad
   if (inherits(res, 'try-error')) {
     return(res)
   }

   return(res)
}
