
# setup a new environment to store error messages, etc.
soilDB.env <- new.env(hash=TRUE, parent = parent.frame())

# safely register some options at package load time
.onLoad <- function(libname, pkgname) {

  # deprecated: soilDB 2.5.9 CRAN release
  options(.soilDB_testNetworkFunctions = TRUE)
  
  # function verbosity
  options(soilDB.verbose = FALSE)
  
  # set default local nasis authentication
  options(soilDB.NASIS.credentials = "DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  # update according to win 7 or 10
  si <- Sys.info()
  if (grepl('windows', si['sysname'], ignore.case = TRUE) & grepl('8|10', si['release'], ignore.case = TRUE)) {
    options(soilDB.NASIS.credentials = "DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y365")
  }
}

.soilDB_test_NASIS_connection <- function(dsn) {
  # test connection
  if (!local_NASIS_defined(dsn) & !inherits(dsn, 'DBIConnection')) {
    if (!requireNamespace("odbc", quietly = TRUE)) {
      stop("Package `odbc` is required to connect to a local NASIS MSSQL Express database", call. = FALSE)
    }
    stop('Local NASIS ODBC connection has not been set up. Please see `http://ncss-tech.github.io/AQP/soilDB/setup_local_nasis.html`.', call. = FALSE)
  }
}

#' @importFrom curl new_handle curl_download
.soilDB_curl_handle <- function(timeout = 300, ssl_verifyhost = 0, ...) {
  curl::new_handle(timeout = timeout, ssl_verifyhost = 0, ...)
}
