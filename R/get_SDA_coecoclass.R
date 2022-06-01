#' Get mapunit ecological sites from Soil Data Access 
#' 
#' @details When `method="Dominant Condition"` an additional field `ecoclasspct_r` is returned in the result with the sum of `comppct_r` that have the dominant condition `ecoclassid`. The component with the greatest `comppct_r` is returned for the `component` and `coecosite` level information. 
#' 
#' Note that if there are multiple `coecoclasskey` per `ecoclassid` there may be more than one record per component.
#' 
#' @param method aggregation method. One of: "Dominant Component", "Dominant Condition", "None". If "None" is selected one row will be returned per component, otherwise one row will be returned per map unit.
#' @param areasymbols vector of soil survey area symbols
#' @param mukeys vector of map unit keys
#' @param WHERE character containing SQL WHERE clause specified in terms of fields in `legend`, `mapunit`, `component` or `coecosite` tables, used in lieu of `mukeys` or `areasymbols`
#' @param query_string Default: `FALSE`; if `TRUE` return a character string containing query that would be sent to SDA via `SDA_query`
#' @param ecoclassref Default: `"Ecological Site Description Database"`. If `NULL` no constraint on `ecoclassref` is used in the query.
#' @param not_rated_value Default: `"Not assigned"`
#' @param miscellaneous_areas Include miscellaneous areas (non-soil components)?
#' @param dsn Path to local SQLite database or a DBIConnection object. If `NULL` (default) use Soil Data Access API via `SDA_query()`.
get_SDA_coecoclass <- function(method = "None",
                               areasymbols = NULL, mukeys = NULL, WHERE = NULL,
                               query_string = FALSE, 
                               ecoclassref = "Ecological Site Description Database",
                               not_rated_value = "Not assigned",
                               miscellaneous_areas = TRUE,
                               dsn = NULL) {
  method <- match.arg(toupper(method), c('NONE', "DOMINANT COMPONENT", "DOMINANT CONDITION"))
  
  if (!is.null(ecoclassref)) {
    ecoclassref <- soilDB::format_SQL_in_statement(ecoclassref)
  }
  
  base_query <- "SELECT legend.areasymbol, legend.lkey, mapunit.muname, mapunit.mukey, component.cokey, coecoclasskey, 
                  comppct_r, majcompflag, compname, compkind, ecoclassid, ecoclassname, ecoclassref FROM legend
   LEFT JOIN mapunit ON legend.lkey = mapunit.lkey
   LEFT JOIN component ON mapunit.mukey = component.mukey %s
   LEFT JOIN coecoclass ON component.cokey = coecoclass.cokey
   WHERE %s"
  
  include_misc <- ifelse(miscellaneous_areas, "", " AND compkind != 'miscellaneous area'")
  
  include_src <- ifelse(is.null(ecoclassref), "", sprintf("(coecoclass.ecoclassref IS NULL OR coecoclass.ecoclassref IN %s)", ecoclassref))
  
  if (is.null(mukeys) && is.null(areasymbols) && is.null(WHERE)) {
    stop("Please specify one of the following arguments: mukeys, areasymbols, WHERE", call. = FALSE)
  }

  if (!is.null(mukeys)) {
    WHERE <- paste("mapunit.mukey IN", format_SQL_in_statement(as.integer(mukeys)))
  } else if (!is.null(areasymbols)) {
    WHERE <- paste("legend.areasymbol IN", format_SQL_in_statement(areasymbols))
  } 
  
  if (include_src != "") {
    WHERE <- paste(WHERE, "AND", include_src)
  }
  
  q <- sprintf(base_query, include_misc, WHERE)
  
  if (query_string)
    return(q)
  
  if (is.null(dsn)) {
    res <- SDA_query(q)
  } else {
    if (!inherits(dsn, 'DBIConnection')) {
      dsn <- dbConnect(RSQLite::SQLite(), dsn)
      on.exit(DBI::dbDisconnect(dsn), add = TRUE)
    } 
    res <- dbGetQuery(dsn, q)
  }
  
  if (length(res) == 0) {
    stop('query returned no results', call. = FALSE)
  }
  
  .I <- NULL
  idx <- NULL
  comppct_r <- NULL
  ecoclasspct_r <- NULL
  
  if (method == "NONE") {
    return(res)
  } else if (method == "DOMINANT COMPONENT") {
    # dominant component
    idx1 <- data.table::data.table(res)[, .I[which.max(comppct_r)], by = "mukey"]$V1
    return(res[idx1, ])
  } 
  
  res$ecoclassid[is.na(res$ecoclassid)] <- not_rated_value
  res$ecoclassname[is.na(res$ecoclassname)] <- not_rated_value
  res$ecoclassref[is.na(res$ecoclassref)] <- not_rated_value

  idx2 <- data.table::data.table(res)[, list(idx = .I[which.max(comppct_r)],
                                             ecoclasspct_r = sum(comppct_r)), 
                                      by = c("mukey", "ecoclassid")][,
                                        list(idx = idx[which.max(ecoclasspct_r)],
                                             ecoclasspct_r = ecoclasspct_r[which.max(ecoclasspct_r)]), 
                                      by = "mukey"]
  res2 <- res[idx2$idx, ]
  res2$ecoclasspct_r <- idx2$ecoclasspct_r
  res2
}

