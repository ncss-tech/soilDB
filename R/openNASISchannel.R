# internal method for opening a connection to local nasis database using credentials

.openNASISchannel <- function(dsn = NULL) {

  use_sqlite <- !is.null(dsn)

  if (is.null(getOption('soilDB.NASIS.credentials')))
    stop("soilDB.NASIS.credentials not set")

  if (!use_sqlite) {
    
    # assuming that default connection uses ODBC
    if (!requireNamespace("odbc"))
      stop("package `odbc` is required", call. = FALSE)
    
    # setup connection local NASIS 
     
    ## old style connection
    # suppressWarnings(odbcDriverConnect(connection = getOption('soilDB.NASIS.credentials')))
    
    credentials <- gsub("^.*\\=(.*)","\\1", strsplit(getOption('soilDB.NASIS.credentials'), ";")[[1]])
    channel <- try(DBI::dbConnect(odbc::odbc(),
                                  DSN = credentials[1],
                                  UID = credentials[2],
                                  PWD = credentials[3]))
  } else {
    
    if (!requireNamespace("RSQLite"))
      stop("package `RSQLite` is required", call. = FALSE)
    
    channel <- try(DBI::dbConnect(RSQLite::SQLite(), dsn))
  }

  # every method that uses .openNASISchannel must handle possibility of
  # not having NASIS db for themselves. most return empty data.frame.
  # hypothetically a more complex empty structure could be returned
  if (inherits(channel, 'try-error')) {
    warning("no local NASIS database available", call. = FALSE)
  }

  return(channel)
}



#' Check for presence of \code{nasis_local} ODBC data source
#' 
#' Check for presence of \code{nasis_local} ODBC data source
#' 
#' 
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: NULL
#' @return logical
#' @examples
#' 
#' 
#' if(local_NASIS_defined()) {
#'   # use fetchNASIS or some other lower-level fetch function
#' } else {
#'   message('could not find `nasis_local` ODBC data source')
#' }
#' 
#' @export local_NASIS_defined
local_NASIS_defined <- function(dsn = NULL) {
  
  if (is.null(dsn)) {
    
    # assuming that default connection uses ODBC
    if (!requireNamespace("odbc"))
      stop("package `odbc` is required ", call. = FALSE)
    
    if ('nasis_local' %in% odbc::odbcListDataSources()$name) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    
    if (!requireNamespace("RSQLite"))
      stop("package `RSQLite` is required", call. = FALSE)
    
    return(RSQLite::dbCanConnect(RSQLite::SQLite(), dsn))
  }
}
