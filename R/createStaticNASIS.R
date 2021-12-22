# Method for "dumping" contents of an entire NASIS table
#
# Method for "dumping" contents of an entire NASIS table
#
#
# @param table_name Character name of table.
# @param dsn Optional: path to SQLite database containing NASIS table
# structure; Default: \code{NULL}
# @return A data.frame or other result of \code{DBI::dbGetQuery}
# @export .dump_NASIS_table
.dump_NASIS_table <- function(table_name, dsn = NULL) {

  # connect to NASIS, identify columns
  con <- dbConnectNASIS(dsn)
  allcols <- "*"

  columns <- NULL
  
  # handling for MSSQL/ODBC weirdness
  if (is.null(dsn) || inherits(con, 'OdbcConnection')) {

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
  res <- dbQueryNASIS(con, q)
  
  if (is.null(columns)) {
    columns <- data.frame(name = colnames(res))
  }
  
  # put back into original order from NASIS
  return(res[, match(colnames(res), columns$name)])
}

#' Create a memory or file-based instance of NASIS database
#'
#' Create a memory or file-based instance of NASIS database for selected
#' tables.
#'
#' @param tables Character vector of target tables. Default: \code{NULL} is whatever tables are listed by `DBI::dbListTables` for the connection typ being used.
#' @param SS Logical. Include "selected set" tables (ending with suffix \code{"_View_1"}). Default: \code{TRUE}
#' @param dsn Optional: path to SQLite database containing NASIS table structure; or a `DBIConnection`. Default: \code{NULL}
#' @param output_path Optional: path to new/existing SQLite database to write tables to. Default: \code{NULL} returns table results as named list.
#' @param new_names Optional: new table names (should match length of vector of matching `tables` in `dsn`)
#' @param verbose Show error messages from attempts to dump individual tables? Default `FALSE`
#'
#' @return A named list of results from calling \code{dbQueryNASIS} for all
#' columns in each NASIS table.
#'
#' @export createStaticNASIS
createStaticNASIS <- function(tables = NULL,
                              new_names = NULL,
                              SS = TRUE,
                              dsn = NULL, output_path = NULL,
                              verbose = FALSE)  {

  # can make static DB from another static DB, or default is local NASIS install (dsn=NULL)
  con <- dbConnectNASIS(dsn = dsn)

  nasis_table_names <- NULL

  # explicit handling of the connection types currently allowed
  if (missing(tables)) {
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
  }

  # keep only explicitly listed tables, if any
  if (!is.null(tables) & length(tables) > 0 & is.character(tables)) {
    nasis_table_names <- tables

    if (!is.null(new_names) && length(new_names) != length(nasis_table_names))
      stop(sprintf("new table names have length %s, but found only %s NASIS tables to rename",
                   length(new_names), length(nasis_table_names)), call. = FALSE)


  } else {
    stop("no tables in database or `tables=` argument", call. = FALSE)
  }

  # remove selected set tables if SS is false
  if (!SS) {
    sstables <- nasis_table_names[grep("_View_1$", nasis_table_names)]
    nasis_table_names <- nasis_table_names[!nasis_table_names %in% sstables]
  }

  if (length(nasis_table_names) == 0) {
    warning("length of vector of table names to query is zero. check `SS` argument")
    return(NULL)
  }

  # return list result if no output path
  if (is.null(output_path)) {

    # return named list of data.frames or try-error (one per table)
    res <- lapply(nasis_table_names, function(n) try(.dump_NASIS_table(n, dsn = dsn),
                                                     silent = verbose))
    if(!is.null(new_names))
      names(res) <- new_names
    else names(res) <- nasis_table_names

    return(res)

  # otherwise, we are writing SQLite to output_path
  } else {


    # assuming that default connection uses ODBC
    if (!requireNamespace("RSQLite"))
      stop("package `RSQLite` is required ", call. = FALSE)

    # create sqlite db
    # RSQLite 2.2.4+ supports `extended_types` argument
    # When TRUE columns of type DATE, DATETIME / TIMESTAMP, and TIME are mapped to corresponding R-classes, c.f. below for details. 
    if (!inherits(output_path, 'DBIConnection'))
       outcon <- DBI::dbConnect(RSQLite::SQLite(), output_path, extended_types = TRUE)
    else outcon <- output_path

    # returns TRUE, invisibly, or try-error (one per table)
    return(lapply(seq_along(nasis_table_names), function(i) {
        return(try({
          n <- nasis_table_names[i]

          if (!is.null(new_names))
            newname <- new_names[i]
          else newname <- n

          newdata <- try(.dump_NASIS_table(n, dsn = dsn), silent = verbose)
          
          # previously processed Date/Times -> character for output
          
          DBI::dbWriteTable(conn = outcon,
                            name =  newname,
                            value = newdata,
                            overwrite = TRUE)
        }))
    }))

    # close output connection
    DBI::dbDisconnect(outcon)
  }

  # close input connection
  DBI::dbDisconnect(con)
}
