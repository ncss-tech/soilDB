#' Generate chunk labels for splitting data
#'
#' @param ids vector of IDs
#' @param size chunk (group) size
#'
#' @return A numeric vector
#'
#' @examples
#'
#' # split the lowercase alphabet into 2 chunks
#'
#' aggregate(letters,
#'           by = list(makeChunks(letters, size=13)),
#'           FUN = paste0, collapse=",")
#'
#' @export
makeChunks <- function(ids, size=100) {
  n <- length(ids)
  chunk.id <- seq(from = 1, to = floor(n / size) + 1)
  chunk.ids <- rep(chunk.id, each = size)
  chunk.ids <- chunk.ids[1:n]
  return(chunk.ids)
}

#' Format vector of values into a string suitable for an SQL `IN` statement.
#' @description Concatenate a vector to SQL \code{IN}-compatible syntax: \code{letters[1:3]} becomes \code{('a','b','c')}. Values in \code{x} are first passed through \code{unique()}.
#' @note Only \code{character} output is supported.
#' @param x A character vector.
#' @return A character vector (unit length) containing concatenated group syntax for use in SQL \code{IN}, with unique value found in \code{x}.
#' @export
#' @examples
#' format_SQL_in_statement(c(2648889L, 2648890L))
format_SQL_in_statement <- function(x) {
  # there is no reason to preserve duplicates
  # and, plenty safe to perform a second time, in case this was done outside of the function call
  x <- unique(x)
	i <- paste(x, collapse = "','")
	i <- paste0("('", i, "')")
	return(i)
}


#' Query Soil Data Access
#'
#' @param q character. A valid T-SQL query surrounded by double quotes.
#' @param dsn character. Default: `NULL` uses Soil Data Access remote data source via REST API. Alternately, `dsn` may be a file path to an SQLite database using the SSURGO schema, or a `DBIConnection` that has already been created.
#'
#' @description Submit a query to the Soil Data Access (SDA) REST/JSON web-service and return the results as a data.frame. There is a 100,000 record and 32Mb JSON serialization limit per query. Queries should contain a WHERE clause or JOIN condition to limit the number of rows affected / returned. Consider wrapping calls to `SDA_query()` in a function that can iterate over logical chunks (e.g. areasymbol, mukey, cokey, etc.). The function `makeChunks()` can help with such iteration. All usages of `SDA_query()` should handle the possibility of a `try-error` result in case the web service connection is down or if an invalid query is passed to the endpoint.
#'
#' @details The SDA website can be found at \url{https://sdmdataaccess.nrcs.usda.gov} and query examples can be found at \url{https://sdmdataaccess.nrcs.usda.gov/QueryHelp.aspx}. A library of query examples can be found at \url{https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=SDA-SQL_Library_Home}.
#'
#' SSURGO (detailed soil survey) and STATSGO (generalized soil survey) data are stored together within SDA. This means that queries that don't specify an area symbol may result in a mixture of SSURGO and STATSGO records. See the examples below and the \href{http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.html}{SDA Tutorial} for details.
#'
#' @note This function requires the `httr`, `jsonlite`, and `xml2` packages
#' @return A data.frame result for queries that return a single table. A list of data.frame for queries that return multiple tables. `NULL` if result is empty, and `try-error` on error.
#' @author D.E. Beaudette, A.G Brown
#' @seealso [SDA_spatialQuery()]
#' @keywords manip
#' @export
#' @examplesIf curl::has_internet() && requireNamespace("httr", quietly = TRUE) && requireNamespace("wk", quietly = TRUE)
#' \donttest{
#'   ## get SSURGO export date for all soil survey areas in California
#'   # there is no need to filter STATSGO
#'   # because we are filtering on SSURGO area symbols
#'   q <- "SELECT areasymbol, saverest FROM sacatalog WHERE areasymbol LIKE 'CA%';"
#'   x <- SDA_query(q)
#'   head(x)
#'
#'
#'   ## get SSURGO component data associated with the
#'   ## Amador series / major component only
#'   # this query must explicitly filter out STATSGO data
#'   q <- "SELECT cokey, compname, comppct_r FROM legend
#'     INNER JOIN mapunit mu ON mu.lkey = legend.lkey
#'     INNER JOIN component co ON mu.mukey = co.mukey
#'     WHERE legend.areasymbol != 'US' AND compname = 'Amador';"
#'
#'   res <- SDA_query(q)
#'   str(res)
#'
#'   ## get component-level data for a specific soil survey area (Yolo county, CA)
#'   # there is no need to filter STATSGO because the query contains
#'   # an implicit selection of SSURGO data by areasymbol
#'   q <- "SELECT
#'     component.mukey, cokey, comppct_r, compname, taxclname,
#'     taxorder, taxsuborder, taxgrtgroup, taxsubgrp
#'     FROM legend
#'     INNER JOIN mapunit ON mapunit.lkey = legend.lkey
#'     LEFT OUTER JOIN component ON component.mukey = mapunit.mukey
#'     WHERE legend.areasymbol = 'CA113' ;"
#'
#'   res <- SDA_query(q)
#'   str(res)
#'
#'   ## get tabular data based on result from spatial query
#'   # there is no need to filter STATSGO because
#'   # SDA_Get_Mukey_from_intersection_with_WktWgs84() implies SSURGO
#'   p <- wk::as_wkt(wk::rct(-120.9, 37.7, -120.8, 37.8))
#'   q <- paste0("SELECT mukey, cokey, compname, comppct_r FROM component
#'       WHERE mukey IN (SELECT DISTINCT mukey FROM
#'       SDA_Get_Mukey_from_intersection_with_WktWgs84('", p,
#'        "')) ORDER BY mukey, cokey, comppct_r DESC")
#'
#'    x <- SDA_query(q)
#'    str(x)
#' }
SDA_query <- function(q, dsn = NULL) {
  if (is.null(dsn)) {
    res <- .SDA_query(q)
    if (inherits(res, 'try-error')) {
      message(res)
      return(invisible(res))
    }
    return(res)
  } else {
    if (inherits(dsn, 'DBIConnection')) {
      return(DBI::dbGetQuery(dsn, q))
    } else if (file.exists(dsn)) {
      if (requireNamespace("RSQLite")) {
        con <- try(RSQLite::dbConnect(RSQLite::SQLite(), dsn))
        on.exit(DBI::dbDisconnect(con), add = TRUE)
        if (!inherits(con, 'try-error')) {
          return(RSQLite::dbGetQuery(con, q))
        } else {
          return(invisible(con))
        }
      }
    } else {
      stop("Invalid data source name: ", dsn, call. = FALSE)
    }
  }
}

.SDA_query <- function(q) {

  # check for required packages
  if (!requireNamespace('httr', quietly = TRUE))
    stop('please install the `httr` package', call. = FALSE)

  if (!requireNamespace('xml2', quietly = TRUE))
    stop('please install the `xml2` package', call. = FALSE)

  if (!requireNamespace('jsonlite', quietly = TRUE))
    stop('please install the `jsonlite` package', call. = FALSE)

  if (length(q) > 1) {
    stop('Query vector must be length 1')
  }

  if (nchar(q, type = "bytes") > 2.5E6) {
    stop('Query string is too long (>2.5 million bytes), consider soilDB::makeChunks() to split inputs into several smaller queries and iterate', call. = FALSE)
  }
  
  ua <- c(
    libcurl = curl::curl_version()$version,
    `r-curl` = as.character(utils::packageVersion("curl")),
    httr = as.character(utils::packageVersion("httr")),
    soilDB = paste(as.character(utils::packageVersion('soilDB')), "(SDA_query)")
  )
  
  # submit request
  r <- try(httr::POST(
      url = "https://sdmdataaccess.sc.egov.usda.gov/tabular/post.rest",
      body = list(query = q, format = "json+columnname+metadata"),
      httr::add_headers(`User-Agent` = paste0(names(ua), "/", ua, collapse = " ")),
      encode = "form"
    ),
    silent = TRUE
  )
  
  if (inherits(r, 'try-error')) {
    message("Soil Data Access POST request failed, returning try-error.\n\n", r)
    return(invisible(r))
  }
  
  # check response content type
  h <- r$all_headers
  
  if (!is.null(h) && (length(h) == 0 
        || is.null(h[[1]]$headers$`content-type`)
        || !h[[1]]$headers$`content-type` %in% 
                            c("application/json; charset=utf-8", # data response
                              "text/xml; charset=utf-8") # error response (maybe not anymore?)
  )) {
    msg <- "Soil Data Access REST API is not currently available, please try again later."
    if (is.null(h[[1]]$headers$`content-type`)) {
      txt <- try(httr::content(r, as = "text", encoding = "UTF-8"), silent = TRUE)
      if (!inherits(txt, 'try-error')) {
        msg <- gsub("<[^>]*>", "", txt)
      }
    }
    r <- try(stop(msg, call. = FALSE), silent = TRUE)
  }
  
  if (inherits(r, 'try-error')) {
    message("Soil Data Access POST request failed, returning try-error.\n\n", r)
    return(invisible(r))
  }

  # trap errors, likely related to SQL syntax errors
  request.status <- try(httr::stop_for_status(r), silent = TRUE)

  # error message is encapsulated in XML, use xml2 library functions to extract
  if (inherits(request.status, 'try-error')) {
    
    # get the request response, this will contain an error message
    r.content <- try(httr::content(r, as = 'parsed', encoding = 'UTF-8'), silent = TRUE)
    
    if (!inherits(r.content, 'try-error')) {
      
      # parse the XML to get the error message
      error.msg <- try(xml2::xml_text(r.content), silent = TRUE)
      
      if (inherits(error.msg, 'try-error')) {
        error.msg <- "Unable to parse error message from XML response"
      }
      
      ## inject specific message into a try-error result
      request.status <- try(stop(attr(request.status, 'condition')$message, "\n", error.msg), silent = TRUE)
    }
    
    # return the error object so calling function/user can handle it
    return(invisible(request.status))
    
  }

  # the result is JSON:
  # list of character matrix, one for each "Table" returned
  # note: the data returned by SDA/JSON are all character class
  #       we "fix" this later on
  r.content <- try(httr::content(r, as = 'text', encoding = 'UTF-8'), silent = TRUE)
  
  if (inherits(r.content, 'try-error'))
    return(invisible(r.content))

  d <- try(jsonlite::fromJSON(r.content), silent = TRUE)

  if (inherits(d, 'try-error'))
    return(invisible(d))

  # number of results
  n.tables <- length(d)

  # no results, terminate here
  if (n.tables < 1) {
    message('empty result set')
    return(NULL)
  }

  # process list of tables
  d <- try(lapply(d, .post_process_SDA_result_set), silent = TRUE)

  if (inherits(d, 'try-error'))
    return(invisible(d))

  # keep track of SDA result set IDs
  SDA.ids <- names(d)
  for (i in 1:n.tables) {
    attr(d[[i]], 'SDA_id') <- SDA.ids[i]
  }


  if (n.tables > 1) {
    message('multi-part result set, returning a list')
    return(d)
  } else {
    # single result set, returng data.frame
    message('single result set, returning a data.frame')
    return(d[[1]])
  }

}


## See https://github.com/ncss-tech/soilDB/pull/191 for a list of possible data types

# note: empty strings and 'NA' are converted into <NA>
# convert the raw results from SDA into a proper data.frame
# no conversion of strings -> factors
.post_process_SDA_result_set <- function(i) {

  # the first line is always the field names
  colnames(i) <- i[1, ]

  # the second line contains field metadata
  m <- unlist(i[2, ])

  # remove lines 1:2
  i <- i[-c(1, 2), , drop = FALSE]

  # keep everything in memory, c/o Kyle Bockinsky
  df <- as.data.frame(i, stringsAsFactors = FALSE)

  # parse metadata to create colClasses argument
  cc <- sapply(m, function(j) {
    # extract the data type
    dt <- strsplit(j, split = ',', fixed = TRUE)[[1]][8]
    # known data types in SDA and appropriate classes in R
    # fall-back to "character" for unknown data types
    switch(
      dt,
      'DataTypeName=char' = 'character',
      'DataTypeName=nchar' = 'character',
      'DataTypeName=varchar' = 'character',
      'DataTypeName=nvarchar' = 'character',
      'DataTypeName=text' = 'character',
      'DataTypeName=ntext' = 'character',
      'DataTypeName=datetime' = 'character',
      'DataTypeName=datetime2' = 'character',
      'DataTypeName=timestamp' = 'character',
      'DataTypeName=bit' = 'integer',
      'DataTypeName=int' = 'integer',
      'DataTypeName=bigint' = 'integer',
      'DataTypeName=smallint' = 'integer',
      'DataTypeName=tinyint' = 'integer',
      'DataTypeName=numeric' = 'numeric',
      'DataTypeName=real' = 'numeric',
      'DataTypeName=float' = 'numeric',
      'DataTypeName=decimal' = 'numeric',
      'character'
    )
  })

  # convert each column that isn't character
  idx <- which(cc != 'character')
  for (f in idx) {
    df[, f] <- as(df[, f], cc[f])
  }
  
  return(df)
}



