#' Send queries to a NASIS DBIConnection
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
  
  # vectorize queries (return a [possibly named] list)
  if(length(q) > 1) {
    # recursively call dbQueryNASIS(close=FALSE)
    res <- lapply(q, function(x) dbQueryNASIS(conn, x, close = FALSE))
    names(res) <- names(q)
    dd <- res
  } else {
    ## exec query
    d <- DBI::dbGetQuery(conn, q, ...)
    # res <- DBI::dbSendQuery(conn, q)
    # d <- DBI::dbFetch(res)
    # d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
    
    dd <- data.frame(d)
    
  }

  ## close connection if needed
  if (close) {
    DBI::dbDisconnect(conn)
  }
  return(dd)
}



#' Create a connection to a local NASIS database
#' 
#' Create a connection to a local NASIS database with `DBI`
#' 
#' @aliases NASIS
#' @param static_path Optional: path to SQLite database containing NASIS table
#' structure; Default: \code{NULL}
#' @return A \code{DBIConnection} object, as returned by
#' \code{DBI::dbConnect()}.
#' @export dbConnectNASIS
dbConnectNASIS <- function(static_path = NULL) {
  # TODO: NASIS sqlite snapshot connection via DBI/RSQLite
  
  # default connection uses DBI/odbc (historically RODBC)
  res <- .openNASISchannel(static_path)
  
  return(res)
}

NASIS <- function(static_path = NULL) dbConnectNASIS(static_path = static_path)
