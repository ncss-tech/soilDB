#' Get mapunit ecological sites from Soil Data Access 
#' 
#' @details When `method="Dominant Condition"` an additional field `ecoclasspct_r` is returned in the result with the sum of `comppct_r` that have the dominant condition `ecoclassid`. The component with the greatest `comppct_r` is returned for the `component` and `coecosite` level information. 
#' 
#' Note that if there are multiple `coecoclasskey` per `ecoclassid` there may be more than one record per component.
#' 
#' @param method aggregation method. One of: "Dominant Component", "Dominant Condition", "None". If "None" is selected one row will be returned per component, otherwise one row will be returned per map unit.
#' @param areasymbols vector of soil survey area symbols
#' @param mukeys vector of map unit keys
#' @param query_string Default: `FALSE`; if `TRUE` return a character string containing query that would be sent to SDA via `SDA_query`
#' @param ecoclassref Default: `"Ecological Site Description Database"`. If `NULL` no constraint on `ecoclassref` is used in the query.
#' @param not_rated_value Default: `"Not assigned"`
#' @param miscellaneous_areas Include miscellaneous areas (non-soil components)?
get_SDA_coecoclass <- function(method = "None",
                               areasymbols = NULL, mukeys = NULL,
                               query_string = FALSE, 
                               ecoclassref = "Ecological Site Description Database",
                               not_rated_value = "Not assigned",
                               miscellaneous_areas = TRUE) {
  method <- match.arg(toupper(method), c('NONE', "DOMINANT COMPONENT", "DOMINANT CONDITION"))
  
  stopifnot(!is.null(areasymbols) || !is.null(mukeys))
  
  if (!is.null(areasymbols)) {
    areasymbols <- soilDB::format_SQL_in_statement(areasymbols)
  }
  
  if (!is.null(mukeys)) {
    mukeys <- soilDB::format_SQL_in_statement(mukeys)
  }
  
  if (!is.null(ecoclassref)) {
    ecoclassref <- soilDB::format_SQL_in_statement(ecoclassref)
  }
  
  base_query <- "SELECT mu.mukey, c.cokey, coecoclasskey, 
                  comppct_r, majcompflag, compname, compkind, 
                  ecoclassid, ecoclassname FROM legend l
   LEFT JOIN mapunit mu ON l.lkey = mu.lkey
   LEFT JOIN component c ON mu.mukey = c.mukey %s
   LEFT JOIN coecoclass ce ON c.cokey = ce.cokey %s
   WHERE %s"
  
  include_misc <- ifelse(miscellaneous_areas, "", " AND compkind != 'miscellaneous area'")
  
  include_src <- ifelse(is.null(ecoclassref), "", sprintf(" AND ecoclassref IN %s", ecoclassref))
  
  where_clause <- switch(as.character(is.null(areasymbols)),
                         "TRUE" = sprintf("mu.mukey IN %s", mukeys),
                         "FALSE" = sprintf("l.areasymbol IN %s", areasymbols))
  
  q <- sprintf(base_query, include_misc, include_src, where_clause)
  
  if (query_string)
    return(q)
  
  res <- SDA_query(q)
  
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

