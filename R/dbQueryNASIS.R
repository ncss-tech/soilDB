#' Send queries to a NASIS DBIConnection
#'
#' @param conn A \code{DBIConnection} object, as returned by \code{DBI::dbConnect()}.
#' @param q A statement to execute using \code{DBI::dbGetQuery}
#' @param close Close connection after query? Default: \code{TRUE}
#' @param ... Additional arguments to \code{DBI::dbGetQuery}
#'
#' @return Result of \code{DBI::dbGetQuery}
#' @export
#'
#' @importFrom DBI dbGetQuery, dbDisconnect
dbQueryNASIS <- function(conn, q, close = TRUE, ...) {

  if (inherits(conn, 'try-error'))
    stop("Failed to connect to NASIS database!")
  
  ## exec query
  d <- DBI::dbGetQuery(conn, q, ...)
  # res <- DBI::dbSendQuery(conn, q)
  # d <- DBI::dbFetch(res)
  # d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  
  dd <- data.frame(d)
  ## close connection
  if (close == TRUE)
    DBI::dbDisconnect(conn)
  # RODBC::odbcClose(channel)
  
  return(dd)
}

#' Create a connection to a local NASIS database
#'
#' @param static_path Optional: path to SQLite database containing NASIS table structure; Default: \code{NULL}
#'
#' @return A \code{DBIConnection} object, as returned by \code{DBI::dbConnect()}.
#' 
#' @export
#'
dbConnectNASIS <- function(static_path = NULL) {
  # TODO: NASIS sqlite snapshot connection via DBI/RSQLite
  
  # default connection uses DBI/odbc (historically RODBC)
  res <- .openNASISchannel(static_path)
  
  return(res)
}
