#' Query a NASIS DBIConnection
#'
#' Send queries to a NASIS DBIConnection
#'
#'
#' @param conn A \code{DBIConnection} object, as returned by \code{DBI::dbConnect()}.
#' @param q A statement to execute using \code{DBI::dbGetQuery}; or a (named) vector containing multiple statements to evaluate separately
#' @param close Close connection after query? Default: \code{TRUE}
#' @param ... Additional arguments to \code{DBI::dbGetQuery}
#' @return Result of \code{DBI::dbGetQuery}
#' @export dbQueryNASIS
dbQueryNASIS <- function(conn, q, close = TRUE, ...) {

  if (inherits(conn, 'try-error'))
    stop("Failed to connect to NASIS database!")

  if (close) {
    # don't close connections we tagged as user-defined
    isUserDefined <- attr(conn, 'isUserDefined')
    if(!is.null(isUserDefined) && isUserDefined) {
      close <- FALSE
    }
  }
  
  # vectorize queries (return a [possibly named] list)
  if(length(q) > 1) {
    # recursively call dbQueryNASIS(close=FALSE)
    res <- lapply(q, function(x) dbQueryNASIS(conn, x, close = FALSE))
    names(res) <- names(q)
    dd <- res
  } else {
    ## exec query
    d <- DBI::dbGetQuery(conn, q, ...)

    dd <- data.frame(d)

  }

  ## close connection if needed
  if (close) {
    DBI::dbDisconnect(conn)
  }
  return(dd)
}



#' Create local NASIS database connection
#'
#' Create a connection to a local NASIS database with `DBI`
#'
#' @aliases NASIS
#' @param dsn Optional: path to SQLite database containing NASIS table
#' structure; Default: \code{NULL}
#' @return A \code{DBIConnection} object, as returned by
#' \code{DBI::dbConnect()}. If `dsn` is a `DBIConnection`, the attribute `isUserDefined` of the result is set to `TRUE`. If the `DBIConnection` is created by the internal NASIS connection process, `isUserDefined` is set to `FALSE.`
#' @export dbConnectNASIS
dbConnectNASIS <- function(dsn = NULL) {
  
  # allow users to set their custom DBI connection with dsn argument
  isUserDefined <- inherits(dsn, 'DBIConnection')
  
  if (isUserDefined) {
    attr(dsn, 'isUserDefined') <- TRUE
    return(dsn)
  }
  
  # default connection uses DBI/odbc, alternately RSQLite
  res <- .openNASISchannel(dsn)
  attr(res, 'isUserDefined') <- FALSE
  
  return(res)
}

# shorthand
#' @export
NASIS <- function(dsn = NULL) dbConnectNASIS(dsn = dsn)
