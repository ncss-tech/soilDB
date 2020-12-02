# internal method for opening a connection to local nasis database using credentials
.openNASISchannel <- function(use_sqlite = TRUE) {

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
    channel <- try(DBI::dbConnect(RSQLite::SQLite(), "C:/Geodata/soils/NASIS-data.sqlite"))
  }

  # every method that uses .openNASISchannel must handle possibility of
  # not having NASIS db for themselves. most return empty data.frame.
  # hypothetically a more complex empty structure could be returned
  # if  (channel == -1)
  #   warning("no local NASIS database available", call.=FALSE)
  if (inherits(channel, 'try-error')) {
    warning("no local NASIS database available", call. = FALSE)
    # channel <- -1
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
#'
local_NASIS_defined <- function() {
  # check for user-defined
  if(!requireNamespace("RODBC"))
    stop("package `RODBC` is required", call. = FALSE)
  if('nasis_local' %in% names(RODBC::odbcDataSources())) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
