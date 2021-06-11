# Based on ssurgoOnDemand by chad ferguson and jason nemecek
# SDA_hydric.R: translation of SDA_hydric.py into soilDB-style R function by andrew brown
# last update: 2021/04/03

#' Get map unit hydric soils information from Soil Data Access
#' @param areasymbols vector of soil survey area symbols
#' @param mukeys vector of map unit keys
#' @author Jason Nemecek, Chad Ferguson, Andrew Brown
#' @return a data.frame
#' @export
#' @importFrom soilDB format_SQL_in_statement SDA_query
get_SDA_hydric <- function(areasymbols = NULL, mukeys = NULL) {

stopifnot(!is.null(areasymbols) | !is.null(mukeys))

if (!is.null(areasymbols))
        areasymbols <- soilDB::format_SQL_in_statement(areasymbols)

if (!is.null(mukeys))
        mukeys <- soilDB::format_SQL_in_statement(mukeys)


where_clause <- switch(as.character(is.null(areasymbols)),
                       "TRUE" = sprintf("mu.mukey IN %s", mukeys),
                       "FALSE" = sprintf("l.areasymbol IN %s", areasymbols))

q <- sprintf("SELECT areasymbol,
        musym,
        muname,
        mu.mukey/1 AS mukey,
        (SELECT TOP 1 COUNT_BIG(*)
        FROM mapunit
        INNER JOIN component ON component.mukey = mapunit.mukey AND mapunit.mukey = mu.mukey) AS comp_count,
        (SELECT TOP 1 COUNT_BIG(*)
        FROM mapunit
        INNER JOIN component ON component.mukey = mapunit.mukey AND mapunit.mukey = mu.mukey
        AND majcompflag = 'Yes') AS count_maj_comp,
        (SELECT TOP 1 COUNT_BIG(*)
        FROM mapunit
        INNER JOIN component ON component.mukey = mapunit.mukey AND mapunit.mukey = mu.mukey
        AND hydricrating = 'Yes' ) AS all_hydric,
        (SELECT TOP 1 COUNT_BIG(*)
        FROM mapunit
        INNER JOIN component ON component.mukey = mapunit.mukey AND mapunit.mukey = mu.mukey
        AND majcompflag = 'Yes' AND hydricrating = 'Yes') AS maj_hydric,
        (SELECT TOP 1 COUNT_BIG(*)
        FROM mapunit
        INNER JOIN component ON component.mukey = mapunit.mukey AND mapunit.mukey = mu.mukey
        AND majcompflag = 'Yes' AND hydricrating != 'Yes') AS maj_not_hydric,
         (SELECT TOP 1 COUNT_BIG(*)
        FROM mapunit
        INNER JOIN component ON component.mukey=mapunit.mukey AND mapunit.mukey = mu.mukey
        AND majcompflag != 'Yes' AND hydricrating  = 'Yes' ) AS hydric_inclusions,
        (SELECT TOP 1 COUNT_BIG(*)
        FROM mapunit
        INNER JOIN component ON component.mukey = mapunit.mukey AND mapunit.mukey = mu.mukey
        AND hydricrating  != 'Yes') AS all_not_hydric,
         (SELECT TOP 1 COUNT_BIG(*)
        FROM mapunit
        INNER JOIN component ON component.mukey = mapunit.mukey AND mapunit.mukey = mu.mukey
        AND hydricrating  IS NULL ) AS hydric_null
        INTO #main_query
        FROM legend AS l
        INNER JOIN mapunit AS mu ON mu.lkey = l.lkey AND %s


        SELECT areasymbol, mukey, musym, muname,
        CASE WHEN comp_count = all_not_hydric + hydric_null THEN  'Nonhydric'
        WHEN comp_count = all_hydric  THEN 'Hydric'
        WHEN comp_count != all_hydric AND count_maj_comp = maj_hydric THEN 'Predominantly Hydric'
        WHEN hydric_inclusions >= 0.5 AND  maj_hydric < 0.5 THEN  'Predominantly Nonhydric'
        WHEN maj_not_hydric >= 0.5  AND  maj_hydric >= 0.5 THEN 'Partially Hydric' ELSE 'Error' END AS HYDRIC_RATING
        FROM #main_query", where_clause)

   # execute query
   res <- soilDB::SDA_query(q)

   # stop if bad
   if (inherits(res, 'try-error')) {
     warnings()
     stop(attr(res, 'condition'))
   }

   return(res)
}
