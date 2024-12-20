# setup a new environment to store error messages, etc.
#' @export
#' @rdname get_soilDB_env
soilDB.env <- new.env(hash = TRUE)

# safely register some options at package load time
.onLoad <- function(libname, pkgname) {
  
  # function verbosity
  options(soilDB.verbose = FALSE,
          soilDB.timeout = 300,
          soilDB.ssl_verifyhost = 0,
          soilDB.alias_warnings = TRUE)
  
  # set default local nasis authentication
  options(soilDB.NASIS.credentials = "DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  # update according to win 7 or 10
  # NOTE: required NASIS credentials depend more on NASIS/SQL Server version than Windows version
  si <- Sys.info()
  if (grepl('windows', si['sysname'], ignore.case = TRUE) & grepl('8|10', si['release'], ignore.case = TRUE)) {
    options(soilDB.NASIS.credentials = "DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y365")
  }
}

#' Get the soilDB environment used for storing error messages and quality control output
#' 
#' The soilDB package uses an environment to store variables that are created as side effects of various data access and processing routines. 
#' `get_soilDB_env()` provides a method to access this environment from the global (user) environment.
#' @aliases soilDB.env
#' @return a `environment` object
#' @export
#' @examples
#' get_soilDB_env()
get_soilDB_env <- function() {
  soilDB.env
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

#' @importFrom curl new_handle has_internet
.soilDB_curl_handle <- function(timeout = getOption("soilDB.timeout", default = 300), 
                                ssl_verifyhost = getOption("soilDB.verify_host", default = 0), ...) {
  curl::new_handle(timeout = timeout, ssl_verifyhost = ssl_verifyhost, ...)
}

#' @importFrom curl curl_download
.soilDB_curl_get_JSON <- function(x, gzip = FALSE, FUN = jsonlite::fromJSON, quiet = TRUE, ...) {
  tf <- tempfile()
  
  dl <- try(curl::curl_download(
      x,
      tf,
      quiet = quiet,
      mode = ifelse(gzip, "wb", "w"),
      handle = .soilDB_curl_handle()
    ), silent = TRUE)
  
  if (inherits(dl, 'try-error')) {
    if (!quiet) {
      message(dl[1])
    }
    return(NULL)
  }
  
  if (gzip) {
    tf <- gzfile(tf)
  }
  
  res <- FUN(tf, ...)
  unlink(tf)
  res
}

.soilDB_warn_deprecated_aliases <- function(aliases, FUN = deparse(sys.calls()[[sys.nframe() - 1]][[1]])) {
  if (.soilDB_warn_deprecated(FUN) && length(aliases) > 0) {
    message("------------------------------------------")
    message("NOTE: `", FUN, "()` column aliases will be removed in the next soilDB release.")
    message("Please replace use of the following column names with NASIS physical column name:")
    sapply(seq(aliases), function(i) message("\t - ", aliases[i], " => ", names(aliases)[i]))
    message("Set `options(soilDB.alias_warnings=FALSE)` to prevent this message from displaying in future sessions.")
    message("------------------------------------------")
  }
}

.soilDB_warn_deprecated <- function(x) {
  if (isFALSE(getOption("soilDB.alias_warnings", default = TRUE)) || x %in% soilDB.env$soilDB.alias_warnings_fnlist) {
    return(FALSE)
  } else {
    soilDB.env$soilDB.alias_warnings_fnlist <- c(unique(soilDB.env$soilDB.alias_warnings_fnlist), x)
    return(TRUE)
  }
}
