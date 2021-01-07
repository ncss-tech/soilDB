# internal method for opening a connection to local nasis database using credentials

.openNASISchannel <- function(static_path = NULL) {

  use_sqlite <- !is.null(static_path)
  
  if (is.null(getOption('soilDB.NASIS.credentials')))
    stop("soilDB.NASIS.credentials not set")

  if (!use_sqlite) {
    # setup connection local NASIS
    #suppressWarnings(RODBC::odbcDriverConnect(connection = getOption('soilDB.NASIS.credentials')))
    credentials <- gsub("^.*\\=(.*)","\\1", strsplit(getOption('soilDB.NASIS.credentials'), ";")[[1]])
    channel <- try(DBI::dbConnect(odbc::odbc(),
                                  DSN = credentials[1],
                                  UID = credentials[2],
                                  PWD = credentials[3]))
  } else {
    channel <- try(DBI::dbConnect(RSQLite::SQLite(), static_path))
  }

  # every method that uses .openNASISchannel must handle possibility of
  # not having NASIS db for themselves. most return empty data.frame.
  # hypothetically a more complex empty structure could be returned
  if (inherits(channel, 'try-error')) {
    warning("no local NASIS database available", call. = FALSE)
  }

  return(channel)
}

#' Check for presence of `nasis_local` ODBC data source
#'
#' @return logical
#' @export local_NASIS_defined
#'
#' @examples
#'
#' if(local_NASIS_defined()) {
#'   # use fetchNASIS or some other lower-level fetch function
#' } else {
#'   message('could not find `nasis_local` ODBC data source')
#' }
#' @importFrom odbc odbcListDataSources
#' @importFrom RSQLite dbCanConnect SQLite
local_NASIS_defined <- function(static_path = NULL) {
  if (is.null(static_path)) {
    if ('nasis_local' %in% odbc::odbcListDataSources()$name) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else { 
    return(RSQLite::dbCanConnect(RSQLite::SQLite(), static_path))
  }
}
