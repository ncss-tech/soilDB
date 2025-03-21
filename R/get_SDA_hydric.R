# Based on ssurgoOnDemand by chad ferguson and jason nemecek
# SDA_hydric.R: translation of SDA_hydric.py into soilDB-style R function by andrew brown
# last update: 2025/03/21

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
#' @param WHERE character containing SQL WHERE clause specified in terms of fields in `legend`, `mapunit`, or `component` tables, used in lieu of `mukeys` or `areasymbols`
#' @param method One of: `"Mapunit"`, `"Dominant Component"`, `"Dominant Condition"`, `"None"`
#' @param include_minors logical. Include minor components? Default: `TRUE`.
#' @param miscellaneous_areas _logical_. Include miscellaneous areas (non-soil components) in results? Default: `TRUE`. 
#' @param query_string Default: `FALSE`; if `TRUE` return a character string containing query that would be sent to SDA via `SDA_query()`
#' @param dsn Path to local SQLite database or a DBIConnection object. If `NULL` (default) use Soil Data Access API via `SDA_query()`.
#' @author Jason Nemecek, Chad Ferguson, Andrew Brown
#' @return a data.frame
#' @export
get_SDA_hydric <- function(areasymbols = NULL,
                           mukeys = NULL,
                           WHERE = NULL,
                           method = "MAPUNIT",
                           include_minors = TRUE,
                           miscellaneous_areas = TRUE,
                           query_string = FALSE,
                           dsn = NULL) {
  
    method <- match.arg(toupper(method), c("MAPUNIT", "DOMINANT COMPONENT", "DOMINANT CONDITION", "NONE"))

    if (is.null(mukeys) && is.null(areasymbols) && is.null(WHERE)) {
      stop("Please specify one of the following arguments: mukeys, areasymbols, WHERE", call. = FALSE)
    }

    if (!is.null(mukeys)) {
      WHERE <- paste("mapunit.mukey IN", format_SQL_in_statement(as.integer(mukeys)))
    } else if (!is.null(areasymbols)) {
      WHERE <- paste("legend.areasymbol IN", format_SQL_in_statement(areasymbols))
    }
    
    .h0 <- function(w) .LIMIT_N(paste(sprintf("SELECT ISNULL(SUM(comppct_r), 0) 
                                       FROM mapunit AS mu 
                                       INNER JOIN component AS c ON c.mukey = mu.mukey AND mapunit.mukey = mu.mukey %s %s",
                                       ifelse(miscellaneous_areas, "", " AND NOT c.compkind = 'Miscellaneous area'"),
                                       ifelse(include_minors, "", " AND c.majcompflag = 'Yes'")), w),
                                n = 1, sqlite = !is.null(dsn))

    q <- paste0("WITH main_query AS (SELECT mapunit.mukey, areasymbol, musym, mapunit.muname, 
              (", .h0(""), ") AS total_comppct,
              (", .h0("AND majcompflag = 'Yes'"), ") AS count_maj_comp,
              (", .h0("AND hydricrating = 'Yes'"), ") AS all_hydric,
              (", .h0("AND majcompflag = 'Yes' AND hydricrating = 'Yes'"), ") AS hydric_majors,
              (", .h0("AND majcompflag = 'Yes' AND hydricrating != 'Yes'"), ") AS maj_not_hydric,
              (", .h0("AND majcompflag != 'Yes' AND hydricrating = 'Yes'"), ") AS hydric_inclusions,
              (", .h0("AND hydricrating != 'Yes'"), ") AS all_not_hydric,
              (", .h0("AND hydricrating IS NULL"), ") AS hydric_null
            FROM legend
            INNER JOIN mapunit ON mapunit.lkey = legend.lkey AND ", WHERE, ")
            SELECT mukey, areasymbol, musym, muname,
               total_comppct AS total_comppct,
               hydric_majors AS hydric_majors,
               hydric_inclusions AS hydric_inclusions,
            CASE WHEN total_comppct = all_not_hydric + hydric_null THEN 'Nonhydric'
            WHEN total_comppct = all_hydric THEN 'Hydric'
            WHEN hydric_majors + hydric_inclusions >= total_comppct / 2 THEN 'Predominantly Hydric'
            WHEN hydric_majors > 0 THEN 'Partially Hydric'
            WHEN hydric_majors + hydric_inclusions < total_comppct / 2 THEN 'Predominantly Nonhydric'
            ELSE 'Error' END AS HYDRIC_RATING
            FROM main_query")
            # TODO: refactor out the temp table and CASE WHEN for HYDRIC_RATING calculated in R

    comp_selection <- ""
    hyd_selection <- ""
    
    if (method != "MAPUNIT") {
       if (method %in% c("DOMINANT COMPONENT", "DOMINANT CONDITION")) {
           comp_selection <- sprintf("AND component.cokey = (%s)", .LIMIT_N(sprintf("SELECT c1.cokey FROM component AS c1
         INNER JOIN mapunit AS mu1 ON c1.mukey = mu1.mukey AND c1.mukey = mapunit.mukey %s %s
         ORDER BY c1.comppct_r DESC, c1.cokey", ifelse(miscellaneous_areas, "", " AND NOT c1.compkind = 'Miscellaneous area'"),
                                                ifelse(include_minors, "", " AND c1.majcompflag = 'Yes'")), 
                                            n = 1, sqlite = !is.null(dsn)))
       }

       if (method == "DOMINANT CONDITION") {
           hyd_selection <- sprintf("AND hydricrating = (%s)", 
             .LIMIT_N(sprintf("SELECT hydricrating FROM mapunit AS mu
                      INNER JOIN component ON component.mukey = mapunit.mukey %s %s
                       GROUP BY hydricrating, comppct_r
                       ORDER BY SUM(comppct_r) OVER (PARTITION BY hydricrating) DESC",
                              ifelse(miscellaneous_areas, "", " AND NOT component.compkind = 'Miscellaneous area'"),
                              ifelse(include_minors, "", " AND component.majcompflag = 'Yes'")),
                      n = 1, sqlite = !is.null(dsn)))
       }

       q <- sprintf(paste0("SELECT areasymbol, musym, muname, mapunit.mukey, ",
           ifelse(method == "DOMINANT CONDITION", "", "cokey, compname, compkind, comppct_r, majcompflag, "),
           "hydricrating
             FROM legend
             INNER JOIN mapunit ON mapunit.lkey = legend.lkey
             INNER JOIN component ON component.mukey = mapunit.mukey %s %s %s %s
             WHERE %s"), 
           comp_selection, 
           hyd_selection, 
           ifelse(miscellaneous_areas, "", " AND NOT component.compkind = 'Miscellaneous area'"),
           ifelse(include_minors, "", " AND component.majcompflag = 'Yes'"),
           WHERE)
   }

   if (!is.null(dsn)) {
     q <- gsub("ISNULL", "IFNULL", q)
   }

   if (query_string) {
    return(q)
   }

   # execute query
   res <- SDA_query(q, dsn = dsn)

   return(res)
}
