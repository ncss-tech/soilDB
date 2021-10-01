# Based on ssurgoOnDemand by chad ferguson and jason nemecek
# SDA_hydric.R: translation of SDA_hydric.py into soilDB-style R function by andrew brown
# last update: 2021/04/03

#' Get map unit hydric soils information from Soil Data Access
#'
#' Assess the hydric soils composition of a map unit.
#'
#' The default classes for `method="MAPUNIT"` are as follows:
#' - `'Nonhydric'` - no hydric components
#' - `'Hydric'` - all hydric components
#' - `'Predominantly Hydric'` - hydric component percentage is 50% or more
#' - `'Partially Hydric'` - one or more of the major components is hydric
#' - `'Predominantly Nonhydric'` - hydric component percentage is less than 50%
#'
#' The default result will also include the following summaries of component percentages: `total_comppct`, `hydric_majors` and `hydric_inclusions`.
#'
#' @details Default `method` `"Mapunit"` produces aggregate summaries of all components in the mapunit. Use `"Dominant Component"` and `"Dominant Condition"` to get the dominant component (highest percentage) or dominant hydric condition (similar conditions aggregated across components), respectively. Use `"None"` for no aggregation (one record per component).
#'
#' @param areasymbols vector of soil survey area symbols
#' @param mukeys vector of map unit keys
#' @param method One of: `"Mapunit"`, `"Dominant Component"`, `"Dominant Condition"`, `"None"`
#' @param query_string Default: `FALSE`; if `TRUE` return a character string containing query that would be sent to SDA via `SDA_query`
#' @author Jason Nemecek, Chad Ferguson, Andrew Brown
#' @return a data.frame
#' @export
#' @importFrom soilDB format_SQL_in_statement SDA_query
get_SDA_hydric <- function(areasymbols = NULL, mukeys = NULL, method = "MAPUNIT", query_string = FALSE) {

        stopifnot(!is.null(areasymbols) | !is.null(mukeys))

        if (!is.null(areasymbols)) {
                areasymbols <- soilDB::format_SQL_in_statement(areasymbols)
        }

        if (!is.null(mukeys)) {
                mukeys <- soilDB::format_SQL_in_statement(mukeys)
        }

        method <- match.arg(toupper(method), c("MAPUNIT", "DOMINANT COMPONENT", "DOMINANT CONDITION", "NONE"))

        where_clause <- switch(as.character(is.null(areasymbols)),
                               "TRUE" = sprintf("mu.mukey IN %s", mukeys),
                               "FALSE" = sprintf("l.areasymbol IN %s", areasymbols))

        q <- sprintf("SELECT areasymbol,
        musym,
        muname,
        mu.mukey/1 AS mukey,
        (SELECT TOP 1 ISNULL(SUM(comppct_r), 0)
        FROM mapunit
        INNER JOIN component ON component.mukey = mapunit.mukey AND mapunit.mukey = mu.mukey) AS total_comppct,
        (SELECT TOP 1 ISNULL(SUM(comppct_r), 0)
        FROM mapunit
        INNER JOIN component ON component.mukey = mapunit.mukey AND mapunit.mukey = mu.mukey
        AND majcompflag = 'Yes') AS count_maj_comp,
        (SELECT TOP 1 ISNULL(SUM(comppct_r), 0)
        FROM mapunit
        INNER JOIN component ON component.mukey = mapunit.mukey AND mapunit.mukey = mu.mukey
        AND hydricrating = 'Yes' ) AS all_hydric,
        (SELECT TOP 1 ISNULL(SUM(comppct_r), 0)
        FROM mapunit
        INNER JOIN component ON component.mukey = mapunit.mukey AND mapunit.mukey = mu.mukey
        AND majcompflag = 'Yes' AND hydricrating = 'Yes') AS hydric_majors,
        (SELECT TOP 1 ISNULL(SUM(comppct_r), 0)
        FROM mapunit
        INNER JOIN component ON component.mukey = mapunit.mukey AND mapunit.mukey = mu.mukey
        AND majcompflag = 'Yes' AND hydricrating != 'Yes') AS maj_not_hydric,
         (SELECT TOP 1 ISNULL(SUM(comppct_r), 0)
        FROM mapunit
        INNER JOIN component ON component.mukey=mapunit.mukey AND mapunit.mukey = mu.mukey
        AND majcompflag != 'Yes' AND hydricrating  = 'Yes' ) AS hydric_inclusions,
        (SELECT TOP 1 ISNULL(SUM(comppct_r), 0)
        FROM mapunit
        INNER JOIN component ON component.mukey = mapunit.mukey AND mapunit.mukey = mu.mukey
        AND hydricrating  != 'Yes') AS all_not_hydric,
         (SELECT TOP 1 ISNULL(SUM(comppct_r), 0)
        FROM mapunit
        INNER JOIN component ON component.mukey = mapunit.mukey AND mapunit.mukey = mu.mukey
        AND hydricrating  IS NULL ) AS hydric_null
        INTO #main_query
        FROM legend AS l
        INNER JOIN mapunit AS mu ON mu.lkey = l.lkey AND %s

        SELECT areasymbol, mukey, musym, muname,
               total_comppct AS total_comppct,
               hydric_majors AS hydric_majors,
               hydric_inclusions AS hydric_inclusions,
        CASE WHEN total_comppct = all_not_hydric + hydric_null THEN 'Nonhydric'
        WHEN total_comppct = all_hydric THEN 'Hydric'
        WHEN hydric_majors + hydric_inclusions >= total_comppct / 2 THEN 'Predominantly Hydric'
        WHEN hydric_majors > 0 THEN 'Partially Hydric'
        WHEN hydric_majors + hydric_inclusions < total_comppct / 2 THEN 'Predominantly Nonhydric'
        ELSE 'Error' END AS HYDRIC_RATING
        FROM #main_query", where_clause)

        comp_selection <- ""
        hyd_selection <- ""
        if (method != "MAPUNIT") {
           if (method %in% c("DOMINANT COMPONENT", "DOMINANT CONDITION")) {
                   comp_selection <- "AND c.cokey =
                (SELECT TOP 1 c1.cokey FROM component AS c1
                 INNER JOIN mapunit AS mu1 ON c1.mukey = mu1.mukey AND c1.mukey = mu.mukey
                 ORDER BY c1.comppct_r DESC, c1.cokey)"
           }

           if (method == "DOMINANT CONDITION") {
                   hyd_selection <-
                           "AND hydricrating = (SELECT TOP 1 hydricrating FROM mapunit
                INNER JOIN component ON component.mukey = mapunit.mukey AND mapunit.mukey = mu.mukey
                GROUP BY hydricrating, comppct_r
                ORDER BY SUM(comppct_r) over(partition by hydricrating) DESC)"
           }

           q <- sprintf(paste0("SELECT areasymbol,
                   musym,
                   muname,
                   mu.mukey, ",
                   ifelse(method == "DOMINANT CONDITION", "", "cokey, compname, comppct_r, majcompflag, "),
                   "hydricrating
                 FROM legend l
           INNER JOIN mapunit mu ON mu.lkey = l.lkey
           INNER JOIN component c ON c.mukey = mu.mukey %s %s
           WHERE %s"), comp_selection, hyd_selection, where_clause)
   }

   if (query_string) {
           return(q)
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
