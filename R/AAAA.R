
# setup a new environment to store error messages, etc.
soilDB.env <- new.env(hash=TRUE, parent = parent.frame())

# safely register some options at package load time
.onLoad <- function(libname, pkgname) {
  # function verbosity
  options(soilDB.verbose=FALSE)
  
  # set default local nasis authentication
  options(soilDB.NASIS.credentials="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  # update according to win 7 or 10
  si <- Sys.info()
  if( grepl('windows', si['sysname'], ignore.case = TRUE) & grepl('8|10', si['release'], ignore.case = TRUE) ) {
    options(soilDB.NASIS.credentials="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y365")
  }
}



