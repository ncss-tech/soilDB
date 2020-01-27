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
  channel <- suppressWarnings(RODBC::odbcDriverConnect(connection=getOption('soilDB.NASIS.credentials')))
  
  if(channel == -1)
    stop("no local NASIS database available", call.=FALSE)
  
  return(channel)
}

# check for presence of `nasis_local` ODBC data source
# primarily used in test suite
.local_NASIS_defined <- function() {
  # check for user-defined
  if('nasis_local' %in% names(RODBC::odbcDataSources())) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
