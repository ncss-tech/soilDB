# Based on ssurgoOnDemand by chad ferguson and jason nemecek
# SDA_pmgroupname.R: translation of SDA_pmgrpname_dom_comp.py into soilDB-style R function by andrew brown
# last update: 2021/04/03

#' Get map unit parent material group information from Soil Data Access
#'
#' Uses "Dominant Component" aggregation method.
#'
#' @param areasymbols vector of soil survey area symbols
#' @param mukeys vector of map unit keys
#' @param simplify logical; group into generalized parent material groups? Default `TRUE`
#' @author Jason Nemecek, Chad Ferguson, Andrew Brown
#' @return a data.frame
#' @export
#' @importFrom soilDB format_SQL_in_statement SDA_query
get_SDA_pmgroupname <- function(areasymbols = NULL, mukeys = NULL, simplify = TRUE) {

        stopifnot(!is.null(areasymbols) | !is.null(mukeys))

        if (!is.null(areasymbols))
                areasymbols <- soilDB::format_SQL_in_statement(areasymbols)

        if (!is.null(mukeys))
                mukeys <- soilDB::format_SQL_in_statement(mukeys)


        where_clause <- switch(as.character(is.null(areasymbols)),
                               "TRUE" = sprintf("mu.mukey IN %s", mukeys),
                               "FALSE" = sprintf("l.areasymbol IN %s", areasymbols))

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
             WHEN pmgroupname LIKE '%mixed%' THEN 'Miscellaneous Deposits'
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
             WHEN pmgroupname LIKE '%NULL%' THEN 'NULL' ELSE 'NULL' END"

        if (simplify) {
                q <- sprintf(
                        "SELECT
             sacatalog.areasymbol AS areasymbol,
             mu.mukey AS mukey,
             mu.musym AS musym,
             mu.muname AS muname,
             compname,
             comppct_r,
             %s AS pmgroupname
             FROM sacatalog
             INNER JOIN legend AS l ON l.areasymbol = sacatalog.areasymbol
             INNER JOIN mapunit AS mu ON mu.lkey = l.lkey AND %s
             INNER JOIN component AS c ON c.mukey = mu.mukey AND c.cokey =
             (SELECT TOP 1 c1.cokey FROM component AS c1
             INNER JOIN mapunit AS mu1 ON c1.mukey=mu1.mukey AND c1.mukey=mu.mukey ORDER BY c1.comppct_r DESC, c1.cokey )
             INNER JOIN copmgrp ON copmgrp.cokey=c.cokey",
                        case_pmgroupname,
                        where_clause
                )

        } else {
                q <- sprintf(
                        "SELECT
                     sacatalog.areasymbol AS areasymbol,
                     mu.mukey AS mukey,
                     mu.musym AS musym,
                     mu.muname AS muname,
                     compname,
                     comppct_r,
                     pmgroupname

                     FROM sacatalog
                     INNER JOIN legend l ON l.areasymbol = sacatalog.areasymbol
                     INNER JOIN mapunit mu ON mu.lkey = l.lkey AND %s
                     INNER JOIN component AS c ON c.mukey = mu.mukey AND c.cokey =
                     (SELECT TOP 1 c1.cokey FROM component AS c1
                     INNER JOIN mapunit AS mu1 ON c1.mukey=mu1.mukey AND c1.mukey=mu.mukey ORDER BY c1.comppct_r DESC, c1.cokey )
                     INNER JOIN copmgrp ON copmgrp.cokey=c.cokey",
                        where_clause
                )
        }

   # execute query
   res <- soilDB::SDA_query(q)

   # stop if bad
   if (inherits(res, 'try-error')) {
     warnings()
     stop(attr(res, 'condition'))
   }

   return(res)
}
