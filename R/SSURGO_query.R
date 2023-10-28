#' Query arbitrary data sources that use the SSURGO data model
#'
#' This is a simple wrapper function allowing queries to be passed to a variety of database drivers. It is assumed the database generally follows the SSURGO schema, regardless of the driver being used. 
#' 
#' @param x An SQL query. If `dsn` is NULL `x` is in T-SQL dialect. If `dsn` is a _character_ (file path), the SQLite dialect is used. If `dsn` is a `DBIConnection`, any SQL dialect compatible with the DBI source can be used.
#' @param dsn Default: `NULL` uses Soil Data Access remote data source via REST API. Alternately `dsn` may be a _character_ file path to an SQLite database, or a `DBIConnection` that has already been created.
#'
#' @details No processing of the query string is performed by this function, so all values of `x` must be adjusted according to the value of `dsn`.
#'
#' @return A _data.frame_, or _try-error_ on error
#' @noRd
.SSURGO_query <- function(x, dsn = NULL) {
  if (is.null(dsn)) {
    return(SDA_query(x))
  } else {
    if (inherits(dsn, 'DBIConnection')) {
      return(DBI::dbGetQuery(con, x))
    } else if (file.exists(dsn)) {
      if (requireNamespace("RSQLite")) {
        con <- try(RSQLite::dbConnect(RSQLite::SQLite(), dsn))
        if (!inherits(con, 'try-error')) {
          return(RSQLite::dbGetQuery(con, x))
        } else {
          return(invisible(con))
        }
      }
    } else {
      stop("Invalid data source name: ", dsn, call. = FALSE)
    }
  }
}
