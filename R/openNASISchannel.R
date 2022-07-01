# internal method for opening a connection to local nasis database using credentials

.openNASISchannel <- function(dsn = NULL) {

  use_sqlite <- !is.null(dsn)


  if (!use_sqlite) {
    # assuming that default connection uses ODBC
    if (!requireNamespace("odbc"))
      stop("package `odbc` is required", call. = FALSE)
    
    if (is.null(getOption('soilDB.NASIS.credentials')))
      stop("soilDB.NASIS.credentials not set")
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
    
    channel <- try(DBI::dbConnect(RSQLite::SQLite(), dsn, extended_types = TRUE))
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
#' Check for presence of a NASIS data source. This function _always_ returns `FALSE` when the `odbc` package is not available (regardless of whether you have an ODBC data source properly set up).
#' 
#' If `dsn` is specified as a character vector it is assumed to refer to a SQLite data source. The result will be `TRUE` or `FALSE` depending on the result of `RSQLite::dbCanConnect()`.
#' 
#' If `dsn` is specified as a `DBIConnection` the function returns the value of `DBI::dbExistsTable("MetadataDomainMaster")`
#' 
#' @param dsn Optional: path to local SQLite database, or a DBIConnection, containing NASIS table structure; default: NULL
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
#' @export
#' @importFrom DBI dbExistsTable
local_NASIS_defined <- function(dsn = NULL) {
  
  if (is.null(dsn)) {
    
    # assuming that default connection uses ODBC
    if (!requireNamespace("odbc", quietly = TRUE)) {
      return(FALSE)
    }
    
    if ("nasis_local" %in% odbc::odbcListDataSources()$name) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else if (inherits(dsn, "DBIConnection")) {
    
    # check for metadata domain table as indicator of dsn NASIS origins
    return(DBI::dbExistsTable(dsn, "MetadataDomainMaster") ||
             DBI::dbExistsTable(dsn, "metadatadomainmaster") )
    
  } else if (is.character(dsn)) {
    
    if (!requireNamespace("RSQLite", quietly = TRUE)) {
      stop("package `RSQLite` is required to use character path as `dsn` argument", call. = FALSE)
    }
    return(RSQLite::dbCanConnect(RSQLite::SQLite(), dsn, extended_types = TRUE))
  }
  FALSE
}
