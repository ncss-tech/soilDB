# this function does all the checking to catch different NASIS errors
#  before being able to safely run RODBC::sqlQuery
#  **remember to close with RODBC::odbcClose()

.openNASISchannel <- function() {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)

  if(is.null(getOption('soilDB.NASIS.credentials')))
    stop("soilDB.NASIS.credentials not set")

  # setup connection local NASIS
  channel <- suppressWarnings(RODBC::odbcDriverConnect(connection = getOption('soilDB.NASIS.credentials')))

  # every method that uses .openNASISchannel must handle possibility of
  # not having NASIS db for themselves. most return empty data.frame.
  # hypothetically a more complex empty structure could be returned
  if  (channel == -1)
    warning("no local NASIS database available", call.=FALSE)

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
