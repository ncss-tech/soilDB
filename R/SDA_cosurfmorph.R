## TODO: evaluate efficiency of these queries
##  these queries are not very efficient because of the use of WHERE UPPER(compname) IN ", in.statement, "doesn't properly utilize existing indexes
##  related issue: # https://github.com/ncss-tech/sharpshootR/issues/12


#' Get Geomorphic/Surface Morphometry Data from Soil Data Access 
#' 
#' Get Geomorphic/Surface Morphometry Data from Soil Data Access or a local SSURGO data source and summarize by counts and proportions ("probabilities").
#'
#' @param WHERE WHERE clause added to SQL query. For example: `areasymbol = 'CA067'`
#' @param by Grouping variable. Default: `"compname"`
#' @param dsn Path to local SSURGO database SQLite database. Default `NULL` uses Soil Data Access.
#' @param table Target table to summarize. Default: `"cosurfmorphgc"` (3D Geomorphic Component). Alternate choices include `cosurfmorphhpp` (2D Hillslope Position) and `cosurfmorphss` (Surface Shape).
#' @param query_string Return query instead of sending to Soil Data Access / local database. Default: `FALSE`.
#'
#' @return a `data.frame` containing the grouping variable (`by`) and tabular summaries of counts and proportions of geomorphic records. 
#' @details Default `table="cosurfmorphgc"` summarizes columns `geomposmntn`, `geomposhill`, `geomposflats`, and `geompostrce`.  
#'          `table="cosurfmorphhpp"` summarizes `"hillslopeprof"` and `table="cosurfmorphss"` summarizes `shapeacross` and `shapedown`
#' 
#'  Queries are a generalization of now-deprecated functions from {sharpshootR} by Dylan Beaudette: `geomPosMountainProbability()`, `geomPosHillProbability()`, `surfaceShapeProbability()`, `hillslopeProbability()`
#'  
#'  Similar summaries of SSURGO component surface morphometry data by series name can be found in `fetchOSD(, extended=TRUE)` or downloaded from \url{https://github.com/ncss-tech/SoilWeb-data}
#'  Full component data including surface morphometry summaries at the "site" level can be obtained with `fetchSDA()`.
#'  
#'  @seealso `fetchSDA()` `get_SDA_pmgroupname()`
#' @export
#' @author Dylan E. Beaudette, Andrew Brown
#' @examples
#' \dontrun{
#'  get_SDA_cosurfmorph("areasymbol = 'CA630'")
#'  
#'  get_SDA_cosurfmorph("areasymbol = 'CA630'", by = 'areasymbol')
#'  
#'  get_SDA_cosurfmorph("areasymbol = 'CA630'", table = 'cosurfmorphhpp')
#' }
get_SDA_cosurfmorph <- function(WHERE = NULL,
                                by = "compname",
                                table = "cosurfmorphgc",
                                dsn = NULL,
                                query_string = FALSE) {

  vars <- switch(table,
                 "cosurfmorphgc" = c("geomposmntn", "geomposhill", "geomposflats", "geompostrce"),
                 "cosurfmorphhpp" = "hillslopeprof",
                 "cosurfmorphss" = c("shapeacross", "shapedown")) 
                                   # TODO: surfaceshape: CONCAT() with "/" (combined across+down) 
  
  .SELECT_STATEMENT0 <- function(v) {
    paste0(paste0(v, ", ", paste0(v, "_n"), ", ", paste0(paste0("round(", v, "_n / total, 2) AS p_", v)), collapse = ", "))
  }
  
  .SELECT_STATEMENT1 <- function(v) {
    res <- paste0(paste0(v, ", ", paste0(paste0("CAST(COUNT(", v, ") AS numeric) AS ", v, "_n")), collapse = ", "))
    # TODO:
    # if (table == "cosurfmorphss") {
    #   res <- paste0("CONCAT(shapeacross, '/', shapedown) AS surfaceshape, CAST(COUNT(surfaceshape) AS numeric) AS surfaceshape_n, ", res)
    # }
    res
  }
  
  .JOIN_TABLE <- function(x) {
    switch(x, 
           "cosurfmorphgc" = "LEFT JOIN cosurfmorphgc ON cogeomordesc.cogeomdkey = cosurfmorphgc.cogeomdkey",
           "cosurfmorphss" = "LEFT JOIN cosurfmorphss on cogeomordesc.cogeomdkey = cosurfmorphss.cogeomdkey",
           "cosurfmorphhpp" = "LEFT JOIN cosurfmorphhpp on cogeomordesc.cogeomdkey = cosurfmorphhpp.cogeomdkey") 
  }
  
  .NULL_FILTER <- function(v) {
    paste0(paste0(v, collapse = " IS NOT NULL OR "), " IS NOT NULL")
  }
  
  .ORDER_COLUMNS <- function(v) {
    paste0(paste0(paste0("p_", v), collapse = " DESC, "), " DESC")
  }
  
  q <- paste0("SELECT a.[BYVAR], 
                 ", .SELECT_STATEMENT0(vars), ",
                 total
               FROM (
                 SELECT [BYVAR], 
                 ", .SELECT_STATEMENT1(vars), "
                 FROM legend
                   INNER JOIN mapunit mu ON mu.lkey = legend.lkey
                   INNER JOIN component AS co ON mu.mukey = co.mukey
                   LEFT JOIN cogeomordesc ON co.cokey = cogeomordesc.cokey
                 ", .JOIN_TABLE(table), "
                 WHERE legend.areasymbol != 'US'
                   AND (", .NULL_FILTER(vars), ")
                   AND ", WHERE, "
                 GROUP BY [BYVAR], ", paste0(vars, collapse = ", "), "
               ) AS a JOIN (SELECT [BYVAR], CAST(count([BYVAR]) AS numeric) AS total
                 FROM legend
                   INNER JOIN mapunit AS mu ON mu.lkey = legend.lkey
                   INNER JOIN component AS co ON mu.mukey = co.mukey
                   LEFT JOIN cogeomordesc ON co.cokey = cogeomordesc.cokey
                 ", .JOIN_TABLE(table), "
                 WHERE legend.areasymbol != 'US'
                   AND (", .NULL_FILTER(vars), ")
                   AND ", WHERE, "
                 GROUP BY [BYVAR]) AS b
               ON a.[BYVAR] = b.[BYVAR]
               ORDER BY [BYVAR], ", .ORDER_COLUMNS(vars))
  
  # insert grouping variable
  qsub <- gsub("[BYVAR]", by, q, fixed = TRUE)
  
  if (query_string) {
    return(qsub)
  }
  
  res <- SDA_query(qsub)
  
  # TODO: convert from long->wide format
  #   y <- dcast(x, compname ~ q_param, value.var='p', drop=FALSE)
  
  # TODO: optionally convert NA to 0
  #   if(replaceNA) for(i in 1:nrow(y)) y[i, ][which(is.na(y[i, ]))] <- 0
  
  res
}
 
#   # perform query
#   x <- SDA_query(q)
#   
#   # re-level
#   x$q_param <- factor(x$q_param, levels=c('Mountaintop', 'Mountainflank', 'Upper third of mountainflank', 'Center third of mountainflank', 'Lower third of mountainflank', 'Mountainbase'))
#   
#   # convert from long-wide format
#   y <- dcast(x, compname ~ q_param, value.var='p', drop=FALSE)
#   
#   # optionally convert NA to 0
#   if(replaceNA) {
#     for(i in 1:nrow(y)) {
#       idx <- which(is.na(y[i, ]))
#       y[i, ][idx] <- 0
#     }
#   }
#   
#   return(y)
# }
# 
# 
# # s: vector of soil series names
# # replaceNA: convert missing categories into 0 probabilities
# geomPosHillProbability <- function(s, replaceNA=TRUE) {
#   
#   .Deprecated(msg = 'This function is now deprecated, consider using soilDB::fetchSDA() or soilDB::fetchOSD(..., extended = TRUE)')
#   
#   # format IN statement, convert to upper case for comp name normalization
#   in.statement <- format_SQL_in_statement(toupper(s))
#   
#   # format query
#   q <- paste("
#              SELECT a.compname, q_param, q_param_n, total, round(q_param_n / total, 2) AS p
#              FROM
#              (
#              SELECT UPPER(compname) AS compname, geomposhill as q_param, CAST(count(geomposhill) AS numeric) AS q_param_n
#              FROM legend
#              INNER JOIN mapunit mu ON mu.lkey = legend.lkey
#              INNER JOIN component AS co ON mu.mukey = co.mukey 
#              LEFT JOIN cogeomordesc ON co.cokey = cogeomordesc.cokey
#              LEFT JOIN cosurfmorphgc on cogeomordesc.cogeomdkey = cosurfmorphgc.cogeomdkey
#              WHERE 
#              legend.areasymbol != 'US'
#              AND UPPER(compname) IN ", in.statement, "
#              AND geomposhill IS NOT NULL
#              GROUP BY UPPER(compname), geomposhill
#              ) AS a
#              JOIN
#              (
#              SELECT UPPER(compname) AS compname, CAST(count(compname) AS numeric) AS total
#              FROM legend
#              INNER JOIN mapunit mu ON mu.lkey = legend.lkey
#              INNER JOIN component AS co ON mu.mukey = co.mukey
#              LEFT JOIN cogeomordesc ON co.cokey = cogeomordesc.cokey
#              LEFT JOIN cosurfmorphgc on cogeomordesc.cogeomdkey = cosurfmorphgc.cogeomdkey
#              WHERE 
#              legend.areasymbol != 'US'
#              AND UPPER(compname) IN ", in.statement, "
#              AND geomposhill IS NOT NULL
#              GROUP BY UPPER(compname)
#              ) AS b
#              ON a.compname = b.compname
#              ORDER BY compname, p DESC;", sep='')
#   
#   # perform query
#   x <- SDA_query(q)
#   
#   # re-level
#   x$q_param <- factor(x$q_param, levels=c('Interfluve', 'Crest', 'Head Slope', 'Nose Slope', 'Side Slope', 'Base Slope'))
#   
#   # convert from long-wide format
#   y <- dcast(x, compname ~ q_param, value.var='p', drop=FALSE)
#   
#   # optionally convert NA to 0
#   if(replaceNA) {
#     for(i in 1:nrow(y)) {
#       idx <- which(is.na(y[i, ]))
#       y[i, ][idx] <- 0
#     }
#   }
#   
#   return(y)
# }
# 
# 
# 
# # s: vector of soil series names
# # replaceNA: convert missing categories into 0 probabilities
# surfaceShapeProbability <- function(s, replaceNA=TRUE) {
#   
#   .Deprecated(msg = 'This function is now deprecated, consider using soilDB::fetchSDA() or soilDB::fetchOSD(..., extended = TRUE)')
#   
#   # format IN statement, convert to upper case for comp name normalization
#   in.statement <- format_SQL_in_statement(toupper(s))
#   
#   # format query
#   q <- paste("
#              SELECT a.compname, q_param, q_param_n, total, round(q_param_n / total, 2) AS p
#              FROM
#              (
#              SELECT UPPER(compname) AS compname, shapeacross + '/' + shapedown as q_param, CAST(count(shapeacross + '/' + shapedown) AS numeric) AS q_param_n
#              FROM legend
#              INNER JOIN mapunit AS mu ON mu.lkey = legend.lkey
#              INNER JOIN component AS co ON mu.mukey = co.mukey 
#              LEFT JOIN cogeomordesc ON co.cokey = cogeomordesc.cokey
#              LEFT JOIN cosurfmorphss on cogeomordesc.cogeomdkey = cosurfmorphss.cogeomdkey
#              WHERE 
#              legend.areasymbol != 'US'
#              AND UPPER(compname) IN ", in.statement, "
#              AND shapeacross IS NOT NULL
#              AND shapedown IS NOT NULL
#              GROUP BY UPPER(compname), shapeacross + '/' + shapedown
#              ) AS a
#              JOIN
#              (
#              SELECT UPPER(compname) AS compname, CAST(count(compname) AS numeric) AS total
#              FROM legend
#              INNER JOIN mapunit AS mu ON mu.lkey = legend.lkey
#              INNER JOIN component AS co ON mu.mukey = co.mukey
#              LEFT JOIN cogeomordesc ON co.cokey = cogeomordesc.cokey
#              LEFT JOIN cosurfmorphss on cogeomordesc.cogeomdkey = cosurfmorphss.cogeomdkey
#              WHERE 
#              legend.areasymbol != 'US'
#              AND UPPER(compname) IN ", in.statement, "
#              AND shapeacross IS NOT NULL
#              AND shapedown IS NOT NULL
#              GROUP BY UPPER(compname)
#              ) AS b
#              ON a.compname = b.compname
#              ORDER BY compname, p DESC;", sep='')
#   
#   # perform query
#   x <- SDA_query(q)
#   
#   # re-level
#   x$q_param <- factor(x$q_param, levels=c('Convex/Convex', 'Linear/Convex', 'Convex/Linear', 'Concave/Convex', 'Linear/Linear', 'Concave/Linear', 'Convex/Concave', 'Linear/Concave', 'Concave/Concave'))
#   
#   # convert from long-wide format
#   y <- dcast(x, compname ~ q_param, value.var='p', drop=FALSE)
#   
#   # optionally convert NA to 0
#   if(replaceNA) {
#     for(i in 1:nrow(y)) {
#       idx <- which(is.na(y[i, ]))
#       y[i, ][idx] <- 0
#     }
#   }
#   
#   return(y)
# }
# 
# 
# 
# # 's' is a vector of soil series names
# hillslopeProbability <- function(s, replaceNA=TRUE) {	
#   
#   # format IN statement, convert to upper case for comp name normalization
#   in.statement <- format_SQL_in_statement(toupper(s))
#   
#   # format query
#   q <- paste("
#              SELECT a.compname, q_param, q_param_n, total, round(q_param_n / total, 2) AS p
#              FROM
#              (
#              SELECT UPPER(compname) AS compname, hillslopeprof as q_param, CAST(count(hillslopeprof) AS numeric) AS q_param_n
#              FROM legend
#              INNER JOIN mapunit AS mu ON mu.lkey = legend.lkey
#              INNER JOIN component AS co ON mu.mukey = co.mukey 
#              LEFT JOIN cogeomordesc ON co.cokey = cogeomordesc.cokey
#              LEFT JOIN cosurfmorphhpp on cogeomordesc.cogeomdkey = cosurfmorphhpp.cogeomdkey
#              WHERE 
#              legend.areasymbol != 'US'
#              AND UPPER(compname) IN ", in.statement, "
#              AND hillslopeprof IS NOT NULL
#              GROUP BY UPPER(compname), hillslopeprof
#              ) AS a
#              JOIN
#              (
#              SELECT UPPER(compname) AS compname, CAST(count(compname) AS numeric) AS total
#              FROM legend
#              INNER JOIN mapunit AS mu ON mu.lkey = legend.lkey
#              INNER JOIN component AS co ON mu.mukey = co.mukey
#              LEFT JOIN cogeomordesc ON co.cokey = cogeomordesc.cokey
#              LEFT JOIN cosurfmorphhpp on cogeomordesc.cogeomdkey = cosurfmorphhpp.cogeomdkey
#              WHERE 
#              legend.areasymbol != 'US'
#              AND UPPER(compname) IN ", in.statement, "
#              AND hillslopeprof IS NOT NULL
#              GROUP BY UPPER(compname)
#              ) AS b
#              ON a.compname = b.compname
#              ORDER BY compname, p DESC;", sep='')
#   
#   # perform query
#   x <- SDA_query(q)
#   
#   # re-level hillslope positions
#   x$q_param <- factor(x$q_param, levels=c('Toeslope', 'Footslope', 'Backslope', 'Shoulder', 'Summit'))
#   
#   # convert from long-wide format
#   y <- dcast(x, compname ~ q_param, value.var='p', drop=FALSE)
#   
#   # optionally convert NA to 0
#   if(replaceNA) {
#     for(i in 1:nrow(y)) {
#       idx <- which(is.na(y[i, ]))
#       y[i, ][idx] <- 0
#     }
#   }
#   return(y)
# }
# 
# 
