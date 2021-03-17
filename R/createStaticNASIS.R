#' Method for "dumping" contents of an entire NASIS table
#'
#' Method for "dumping" contents of an entire NASIS table
#'
#'
#' @param table_name Character name of table.
#' @param static_path Optional: path to SQLite database containing NASIS table
#' structure; Default: \code{NULL}
#' @return A data.frame or other result of \code{DBI::dbGetQuery}
#' @export .dump_NASIS_table
.dump_NASIS_table <- function(table_name, static_path = NULL) {
  
  # connect to NASIS, identify columns
  con <- dbConnectNASIS(static_path)
  allcols <- "*"

  # handling for MSSQL/ODBC weirdness
  if (is.null(static_path)) {
    
    # assuming that default connection uses ODBC
    if (!requireNamespace("odbc"))
      stop("package `odbc` is required ", call. = FALSE)
    
    columns <- odbc::odbcConnectionColumns(con, table_name)

    # re-arrange VARCHAR(MAX) columns
    longcols <- subset(columns, columns$field.type == "varchar" & columns$column_size == 0)$name
    allcols <- columns$name

    if (length(longcols) > 0) {
      allcols[which(allcols %in% longcols)] <- NA
      allcols <- c(na.omit(allcols), longcols)
    }
  }

  # construct query and return result
  q <- sprintf("SELECT %s FROM %s", paste(allcols, collapse = ", "), table_name)
  return(dbQueryNASIS(con, q))
}



#' Create a memory or file-based instance of NASIS database (for selected
#' tables)
#'
#' Create a memory or file-based instance of NASIS database (for selected
#' tables)
#'
#'
#' @param tables Character vector of target tables. Default: \code{NULL} is all
#' tables meeting the following criteria.
#' @param SS Logical. Include "selected set" tables (ending with suffix
#' \code{"_View1"}). Default: \code{FALSE}
#' @param systables Logical. Include "system" tables (starting with prefix
#' \code{"system"}). Default: \code{FALSE}
#' @param static_path Optional: path to SQLite database containing NASIS table
#' structure; Default: \code{NULL}
#' @param output_path Optional: path to new/existing SQLite database to write
#' tables to. Default: \code{NULL} returns table results as named list.
#' @return A named list of results from calling \code{dbQueryNASIS} for all
#' columns in each NASIS table.
#' @examples
#'
#'
#' \dontrun{
#'  str(createStaticNASIS(tables = c("calculation","formtext")))
#' }
#'
#'
#' @export createStaticNASIS
createStaticNASIS <- function(tables = NULL, SS = FALSE, systables = FALSE,
                              static_path = NULL, output_path = NULL)  {
  # can make static DB from another static DB, or default is local NASIS install (static_path=NULL)
  con <- dbConnectNASIS(static_path = static_path)

  nasis_table_names <- NULL

  # explicit handling of the connection types currently allowed
  if (inherits(con, 'OdbcConnection')) {
    
    if (requireNamespace("odbc"))
      nasis_table_names <- odbc::dbListTables(con)

  } else if (inherits(con, 'SQLiteConnection')) {
    
    if (requireNamespace("RSQLite"))
      nasis_table_names <- RSQLite::dbListTables(con)
    
  } else {
    stop("Currently only OdbcConnection and SQLiteConnection are supported", call. = FALSE)
  }
  
  # must know names of tables in data source
  stopifnot(!is.null(nasis_table_names))

  # never pull the system table
  if (!systables) {
    systables <- grep("^system", nasis_table_names)

    if (length(systables) > 0) {
      nasis_table_names <- nasis_table_names[-systables]
    }
  }

  # keep only explicitly listed tables, if any
  if (!is.null(tables) & length(tables) > 0 & is.character(tables)) {
    nasis_table_names <- nasis_table_names[nasis_table_names %in% tables]
  }

  # remove selected set tables
  if (!SS) {
    sstables <- grep("_View1$", nasis_table_names)
    nasis_table_names <- nasis_table_names[!nasis_table_names %in% sstables]
  }

  # return list result if no output path
  if (is.null(output_path)) {

    # return named list of data.frames or try-error (one per table)
    res <- lapply(nasis_table_names, function(n) try(.dump_NASIS_table(n, static_path = static_path)))
    names(res) <- nasis_table_names
    return(res)

  # otherwise, we are writing SQLite to output_path
  } else {

    
    # assuming that default connection uses ODBC
    if (!requireNamespace("RSQLite"))
      stop("package `RSQLite` is required ", call. = FALSE)

    # create sqlite db
    outcon <- DBI::dbConnect(RSQLite::SQLite(), output_path)

    # returns TRUE, invisibly, or try-error (one per table)
    return(lapply(nasis_table_names, function(n) {
        return(try({
          DBI::dbWriteTable(conn = outcon, name =  n,
                            value = .dump_NASIS_table(n,
                                                      static_path = static_path),
                            overwrite = TRUE)
        }))
    }))

    # close output connection
    DBI::dbDisconnect(outcon)
  }

  # close input connection
  DBI::dbDisconnect(con)
}
