## TODO: evaluate efficiency of these queries
##  these queries are not very efficient because of the use of WHERE UPPER(compname) IN ", in.statement, "doesn't properly utilize existing indexes
##  related issue: # https://github.com/ncss-tech/sharpshootR/issues/12

#' Get Geomorphic/Surface Morphometry Data from Soil Data Access
#'
#' Get Geomorphic/Surface Morphometry Data from Soil Data Access or a local SSURGO data source and summarize by counts and proportions ("probabilities").
#'
#' @param table Target table to summarize. Default: `"cosurfmorphgc"` (3D Geomorphic Component). Alternate choices include `cosurfmorphhpp` (2D Hillslope Position), `cosurfmorphss` (Surface Shape), and  `cosurfmorphmr` (Microrelief).
#' @param by Grouping variable. Default: `"mapunit.mukey"`
#' @param areasymbols A vector of soil survey area symbols (e.g. `'CA067'`)
#' @param mukeys A vector of map unit keys (e.g. `466627`)
#' @param WHERE WHERE clause added to SQL query. For example: `areasymbol = 'CA067'`
#' @param method _character_. One of: `"ByGroup"`, `"None"`
#' @param include_minors logical. Include minor components? Default: `TRUE`.
#' @param miscellaneous_areas logical. Include miscellaneous areas (non-soil components) in results? Default: `FALSE`. 
#' @param representative_only logical. Include only representative Component Parent Material Groups? Default: `TRUE`.
#' @param db Either `'SSURGO'` (default) or `'STATSGO'`. If `'SSURGO'` is specified `areasymbol = 'US'` records are excluded. If `'STATSGO'` only `areasymbol = 'US'` records are included.
#' @param dsn Path to local SSURGO database SQLite database. Default `NULL` uses Soil Data Access.
#' @param query_string Return query instead of sending to Soil Data Access / local database. Default: `FALSE`.
#'
#' @return a `data.frame` containing the grouping variable (`by`) and tabular summaries of counts and proportions of geomorphic records.
#' @details Default `table="cosurfmorphgc"` summarizes columns `geomposmntn`, `geomposhill`, `geomposflats`, and `geompostrce`.
#'          `table="cosurfmorphhpp"` summarizes `"hillslopeprof"`,  `table="cosurfmorphss"` summarizes `shapeacross` and `shapedown`, and `table="cosurfmorphmr"` summarizes `geomicrorelief`.
#'
#'  Queries are a generalization of now-deprecated functions from sharpshootR package by Dylan Beaudette: `geomPosMountainProbability()`, `geomPosHillProbability()`, `surfaceShapeProbability()`, `hillslopeProbability()`
#'
#'  Similar summaries of SSURGO component surface morphometry data by series name can be found in `fetchOSD(, extended=TRUE)` or downloaded from \url{https://github.com/ncss-tech/SoilWeb-data}
#'  Full component data including surface morphometry summaries at the "site" level can be obtained with `fetchSDA()`.
#'
#' @seealso `fetchSDA()` `get_SDA_pmgroupname()`
#' @export
#' @author Dylan E. Beaudette, Andrew G. Brown
#' @examples
#' \dontrun{
#'  # Summarize by 3D geomorphic components by component name (default `by='compname'`)
#'  get_SDA_cosurfmorph(WHERE = "areasymbol = 'CA630'")
#'
#'  # Whole Soil Survey Area summary (using `by = 'areasymbol'`)
#'  get_SDA_cosurfmorph(by = 'areasymbol', WHERE = "areasymbol = 'CA630'")
#'
#'  # 2D Hillslope Position summary(using `table = 'cosurfmorphhpp'`)
#'  get_SDA_cosurfmorph('cosurfmorphhpp', WHERE = "areasymbol = 'CA630'")
#'
#'  # Surface Shape summary (using `table = 'cosurfmorphss'`)
#'  get_SDA_cosurfmorph('cosurfmorphss', WHERE = "areasymbol = 'CA630'")
#'
#'  # Microrelief summary (using `table = 'cosurfmorphmr'`)
#'  get_SDA_cosurfmorph('cosurfmorphmr', WHERE = "areasymbol = 'CA630'")
#' }
get_SDA_cosurfmorph <- function(table = c("cosurfmorphgc", "cosurfmorphhpp", "cosurfmorphss", "cosurfmorphmr"),
                                by = "mapunit.mukey",
                                areasymbols = NULL,
                                mukeys = NULL,
                                WHERE = NULL,
                                method = c("bygroup", "none"),
                                include_minors = TRUE,
                                miscellaneous_areas = FALSE,
                                representative_only = TRUE,
                                db = c('SSURGO', 'STATSGO'),
                                dsn = NULL,
                                query_string = FALSE) {

  if (is.null(mukeys) && is.null(areasymbols) && is.null(WHERE)) {
    stop("Please specify one of the following arguments: mukeys, areasymbols, WHERE", call. = FALSE)
  }
  
  method <- match.arg(toupper(method), c("BYGROUP", "NONE"))

  if (!is.null(mukeys)) {
    WHERE <- paste("mapunit.mukey IN", format_SQL_in_statement(as.integer(mukeys)))
  } else if (!is.null(areasymbols)) {
    WHERE <- paste("legend.areasymbol IN", format_SQL_in_statement(areasymbols))
  }

  db <- match.arg(toupper(db), choices = c('SSURGO', 'STATSGO'))
  table <- match.arg(tolower(table), choices = c("cosurfmorphgc", "cosurfmorphhpp", "cosurfmorphss", "cosurfmorphmr"))
  statsgo_filter <- switch(db, SSURGO = "legend.areasymbol != 'US'", STATSGO = "legend.areasymbol == 'US'", "1=1")

  vars <- switch(table,
                 "cosurfmorphgc" = c("geomposmntn", "geomposhill", "geomposflats", "geompostrce"),
                 "cosurfmorphhpp" = "hillslopeprof",
                 "cosurfmorphss" = c("shapeacross", "shapedown", "surfaceshape"),
                 # NOTE: surfaceshape is calculated CONCAT(shapeacross, '/', shapedown)
                 "cosurfmorphmr" = "geomicrorelief")

  # TODO: weight probabilities by component percentage? needs refactor
  .SELECT_STATEMENT0 <- function(v) {
    paste0(paste0(v, ", ", paste0(v, "_n"), ", ", paste0(paste0("round(", v, "_n / total, 2) AS p_", v)), collapse = ", "))
  }

  .SELECT_STATEMENT1 <- function(v) {
    res <- paste0(paste0(v, ", ", paste0(paste0("CAST(COUNT(", v, ") AS numeric) AS ", v, "_n")), collapse = ", "))

    # custom calculated column `surfaceshape` and `surfaceshape_n`
    if (table == "cosurfmorphss") {
      res <- paste0("CONCAT(shapeacross, '/', shapedown) AS surfaceshape,
                     CAST(COUNT(CONCAT(shapeacross, '/', shapedown)) AS numeric) AS surfaceshape_n, ", res)
      if (!is.null(dsn)) {
        res <- gsub("CONCAT(shapeacross, '/', shapedown)", "shapeacross || '/' || shapedown", res, fixed = TRUE)
      }
    }

    res
  }

  .NULL_FILTER <- function(v, miscellaneous_areas = FALSE) {
    if (miscellaneous_areas) return("1=1")
    paste0(paste0(v, collapse = " IS NOT NULL OR "), " IS NOT NULL")
  }
  
  .ORDER_COLUMNS <- function(v) {
    paste0(paste0(paste0("p_", v), collapse = " DESC, "), " DESC")
  }

  # excludes custom calculated columns (e.g. surfaceshape concatenated from across/down)
  vars_default <- vars[!grepl("surfaceshape", vars)]
  
  if (method == "BYGROUP") {
      q <- paste0("SELECT a.[BYVARNAME] AS [BYVARNAME],
                 ", .SELECT_STATEMENT0(vars), ",
                 total
               FROM (
                 SELECT [BYVAR], [BYVAR] AS BYVAR,
                 ", .SELECT_STATEMENT1(vars_default), "
                 FROM legend
                   INNER JOIN mapunit ON mapunit.lkey = legend.lkey
                   INNER JOIN component ON mapunit.mukey = component.mukey ", 
                   ifelse(include_minors, "", "AND majcompflag = 'Yes'") ,"
                   ", ifelse(miscellaneous_areas, "", " AND NOT component.compkind = 'Miscellaneous area'"),"
                   LEFT JOIN cogeomordesc ON component.cokey = cogeomordesc.cokey
                   ", ifelse(representative_only, "AND rvindicator = 'Yes'", ""), "
                   ", sprintf("INNER JOIN %s ON cogeomordesc.cogeomdkey = %s.cogeomdkey", table, table), "
                 WHERE ", statsgo_filter, "
                   AND (", .NULL_FILTER(vars_default, miscellaneous_areas), ")
                   AND ", WHERE, "
                 GROUP BY [BYVAR], ", paste0(vars_default, collapse = ", "), "
               ) AS a JOIN (SELECT [BYVAR] AS BYVAR, CAST(count([BYVAR]) AS numeric) AS total
                 FROM legend
                   INNER JOIN mapunit ON mapunit.lkey = legend.lkey
                   INNER JOIN component ON mapunit.mukey = component.mukey ", 
                   ifelse(include_minors,"", "AND majcompflag = 'Yes'") ,"
                   ", ifelse(miscellaneous_areas, "", " AND NOT component.compkind = 'Miscellaneous area'"),"
                   LEFT JOIN cogeomordesc ON component.cokey = cogeomordesc.cokey
                   ", ifelse(representative_only, "AND rvindicator = 'Yes'", ""),
                   sprintf("LEFT JOIN %s ON cogeomordesc.cogeomdkey = %s.cogeomdkey", table, table), "
                 WHERE ", statsgo_filter, "
                   AND (", .NULL_FILTER(vars_default, miscellaneous_areas), ")
                   AND ", WHERE, "
                 GROUP BY [BYVAR]) AS b
               ON a.BYVAR = b.BYVAR
               ORDER BY [BYVARNAME], ", .ORDER_COLUMNS(vars_default))
  
  } else if (method == "NONE") {
    
    if (!missing(by)) {
      message("NOTE: `by` argument is ignored when method='none'")
    }
    
    q <- paste0("SELECT mapunit.mukey, component.cokey, compname, compkind, comppct_r, majcompflag, cogeomordesc.rvindicator,",
                paste0(vars, collapse = ", "), "
                FROM legend
                  INNER JOIN mapunit ON mapunit.lkey = legend.lkey
                  INNER JOIN component ON mapunit.mukey = component.mukey ", 
                            ifelse(include_minors, "", "AND majcompflag = 'Yes'") ,"
                  ", ifelse(miscellaneous_areas, "", " AND NOT component.compkind = 'Miscellaneous area'"),"
                  LEFT JOIN cogeomordesc ON component.cokey = cogeomordesc.cokey
                  ", sprintf("LEFT JOIN %s ON cogeomordesc.cogeomdkey = %s.cogeomdkey", table, table), "
                  WHERE ", statsgo_filter, "
                  AND (", .NULL_FILTER(vars_default, miscellaneous_areas), ")
                  AND ", WHERE, "")
  }
  
  # insert grouping variable
  byname <- gsub("(.*\\.)?(.*)", "\\2", by)
  qsub <-  gsub("[BYVARNAME]", byname, gsub("[BYVAR]", by, q, fixed = TRUE), fixed = TRUE)

  
  if (query_string) {
    return(qsub)
  }

  res <- SDA_query(qsub, dsn = dsn)

  res
}
