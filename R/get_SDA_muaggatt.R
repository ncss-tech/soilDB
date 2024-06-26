# Based on ssurgoOnDemand by chad ferguson and jason nemecek
# SDA_muaggatt.R: translation of muaggatt.py into soilDB-style R function by andrew brown
# last update: 2021/10/08

#' Get map unit aggregate attribute information from Soil Data Access
#'
#' @param areasymbols vector of soil survey area symbols
#' @param mukeys vector of map unit keys
#' @param WHERE character containing SQL WHERE clause specified in terms of fields in `legend`, `mapunit`, or `muaggatt` tables, used in lieu of `mukeys` or `areasymbols`
#' @param query_string Default: `FALSE`; if `TRUE` return a character string containing query that would be sent to SDA via `SDA_query`
#' @param dsn Path to local SQLite database or a DBIConnection object. If `NULL` (default) use Soil Data Access API via `SDA_query()`.
#' @author Jason Nemecek, Chad Ferguson, Andrew Brown
#' @return a data.frame
#' @export
get_SDA_muaggatt <- function(areasymbols = NULL, mukeys = NULL, WHERE = NULL, query_string = FALSE, dsn = NULL) {

  
  if (is.null(mukeys) && is.null(areasymbols) && is.null(WHERE)) {
    stop("Please specify one of the following arguments: mukeys, areasymbols, WHERE", call. = FALSE)
  }
  
  if (!is.null(mukeys)) {
    WHERE <- paste("mapunit.mukey IN", format_SQL_in_statement(as.integer(mukeys)))
  } else if (!is.null(areasymbols)) {
    WHERE <- paste("legend.areasymbol IN", format_SQL_in_statement(areasymbols))
  } 

  q <- sprintf("SELECT muaggatt.mukey, muaggatt.*
                FROM legend 
                INNER JOIN mapunit ON mapunit.lkey = legend.lkey
                INNER JOIN muaggatt ON mapunit.mukey = muaggatt.mukey
                WHERE %s", WHERE)

  if (query_string) {
    return(q)
  }
  
  # execute query
  res <- SDA_query(q, dsn = dsn)
  
  # return if bad
  if (inherits(res, 'try-error')) {
    return(res)
  }
  
  # remove duplicated mukey column
  res[[ncol(res)]] <- NULL

  return(res)
}
