#' Generate chunk labels for splitting data
#'
#' @param ids vector of IDs
#' @param size chunk (group) size
#'
#' @return A numeric vector
#' @export makeChunks
#'
#' @examples
#'
#' # split the lowercase alphabet into 2 chunks
#'
#' aggregate(letters,
#'           by = list(makeChunks(letters, size=13)),
#'           FUN = paste0, collapse=",")
#'
makeChunks <- function(ids, size=100) {
  n <- length(ids)
  chunk.id <- seq(from=1, to=floor(n / size)+1)
  chunk.ids <- rep(chunk.id, each=size)
  chunk.ids <- chunk.ids[1:n]
  return(chunk.ids)
}

#' @title Format vector of values into a string suitable for an SQL `IN` statement.
#'
#' @description Concatenate a vector to SQL \code{IN}-compatible syntax: \code{letters[1:3]} becomes \code{('a','b','c')}. Values in \code{x} are first passed through \code{unique()}.
#'
#' @note Only \code{character} output is supported.
#'
#' @param x A character vector.
#'
#' @return A character vector (unit length) containing concatenated group syntax for use in SQL \code{IN}, with unique value found in \code{x}.
#' @export format_SQL_in_statement
#'
#' @examples
#'
#' \donttest{
#'
#' library(aqp)
#'
#' # get some mukeys
#' q <- "select top(2) mukey from mapunit;"
#' mukeys <- SDA_query(q)
#'
#' # format for use in an SQL IN statement
#' mukey.inst <- format_SQL_in_statement(mukeys$mukey)
#' mukey.inst
#'
#' # make a more specific query: for component+horizon data, just for those mukeys
#' q2 <- sprintf("SELECT * FROM mapunit
#'                INNER JOIN component ON mapunit.mukey = component.mukey
#'                INNER JOIN chorizon ON component.cokey = chorizon.cokey
#'                WHERE mapunit.mukey IN %s;", mukey.inst)
#' # do the query
#' res <- SDA_query(q2)
#'
#' # build a SoilProfileCollection from horizon-level records
#' depths(res) <- cokey ~ hzdept_r + hzdepb_r
#'
#' # normalize mapunit/component level attributes to site-level for plot
#' site(res) <- ~ muname + mukey + compname + comppct_r + taxclname
#'
#' # make a nice label
#' res$labelname <- sprintf("%s (%s%s)", res$compname, res$comppct_r, "%")
#'
#' # major components only
#' res <- subset(res, comppct_r >= 85)
#'
#' # inspect plot of result
#' par(mar=c(0,0,0,0))
#' groupedProfilePlot(res, groups = "mukey", color = "hzname", cex.names=0.8,
#'                    id.style = "side", label = "labelname")
#'}
#'
#'
format_SQL_in_statement <- function(x) {
  # there is no reason to preserve duplicates
  # and, plenty safe to perform a second time, in case this was done outside of the function call
  x <- unique(x)
	i <- paste(x, collapse = "','")
	i <- paste("('", i, "')", sep = '')
	return(i)
}


#' Query Soil Data Access
#'
#' @param q A valid T-SQL query surrounded by double quotes
#'
#' @description Submit a query to the Soil Data Access (SDA) REST/JSON web-service and return the results as a data.frame. There is a 100,000 record limit and 32Mb JSON serializer limit, per query. Queries should contain a WHERE statement or JOIN condition to limit the number of rows affected / returned. Consider wrapping calls to \code{SDA_query} in a function that can iterate over logical chunks (e.g. areasymbol, mukey, cokey, etc.). The function \code{makeChunks} can help with such iteration.
#'
#' @details The SDA website can be found at \url{https://sdmdataaccess.nrcs.usda.gov} and query examples can be found at \url{https://sdmdataaccess.nrcs.usda.gov/QueryHelp.aspx}. A library of query examples can be found at \url{https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=SDA-SQL_Library_Home}.
#'
#' SSURGO (detailed soil survey) and STATSGO (generalized soil survey) data are stored together within SDA. This means that queries that don't specify an area symbol may result in a mixture of SSURGO and STATSGO records. See the examples below and the \href{http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.html}{SDA Tutorial} for details.
#'
#' @note This function requires the `httr`, `jsonlite`, and `XML` packages
#' @return a data.frame result (\code{NULL} if empty, try-error on error)
#' @export
#' @author D.E. Beaudette
#' @seealso \code{\link{mapunit_geom_by_ll_bbox}}
#' @keywords manip
#' @examples
#' \donttest{
#' if(requireNamespace("curl") &
#'    curl::has_internet()) {
#'
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
#'   #
#'   # requires raster and rgeos packages because raster is suggested
#'   # and rgeos is additional
#'   if(require(raster) & require(rgeos)) {
#'     # text -> bbox -> WKT
#'     # xmin, xmax, ymin, ymax
#'     b <- c(-120.9, -120.8, 37.7, 37.8)
#'     p <- writeWKT(as(extent(b), 'SpatialPolygons'))
#'     q <- paste0("SELECT mukey, cokey, compname, comppct_r FROM component
#'       WHERE mukey IN (SELECT DISTINCT mukey FROM
#'       SDA_Get_Mukey_from_intersection_with_WktWgs84('", p,
#'        "')) ORDER BY mukey, cokey, comppct_r DESC")
#'
#'     x <- SDA_query(q)
#'     str(x)
#'   }
#'  }
#' }

SDA_query <- function(q) {

  # check for required packages
  if (!requireNamespace('httr', quietly = TRUE) | !requireNamespace('jsonlite', quietly = TRUE))
    stop('please install the `httr` and `jsonlite` packages', call. = FALSE)

  # submit request
  r <- httr::POST(url = "https://sdmdataaccess.sc.egov.usda.gov/tabular/post.rest",
                  body = list(query = q,
                              format = "json+columnname+metadata"),
                  encode = "form")

  # trap errors, likely related to SQL syntax errors
  request.status <- try(httr::stop_for_status(r), silent = TRUE)

  # error message is encapsulated in XML, use xml2 library functions to extract
  if (class(request.status) == 'try-error') {

    # get the request response, this will contain an error message
    r.content <- httr::content(r, as = 'parsed', encoding = 'UTF-8')

    # parse the XML to get the error message
    error.msg <- xml_text(r.content)

    ## warning: bad result
    warning(error.msg, call. = FALSE)

    # return the error object so calling function/user can handle it
    return(invisible(request.status))
  }

  # the result is JSON:
  # list of character matrix, one for each "Table" returned
  # note: the data returned by SDA/JSON are all character class
  #       we "fix" this later on
  r.content <- try(httr::content(r, as = 'text', encoding = 'UTF-8'), silent = TRUE)

  if (inherits(r.content,'try-error'))
      return(invisible(r.content))

  d <- try(jsonlite::fromJSON(r.content))

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


## See https://github.com/ncss-tech/soilDB/pull/191 for a list of possibly data types

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
    switch(dt,
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
  for(f in idx) {
    df[, f] <- as(df[, f], cc[f])
  }


  ## strings resembling scientific notation are converted into numeric
  ## ex: type.convert("8E2") -> 800
  # https://github.com/ncss-tech/soilDB/issues/190

  # # attempt type conversion
  # # same result as writing to file and reading-in via read.table()
  # df <- type.convert(df,
  #                    na.strings = c('', 'NA'),
  #                    as.is = TRUE,
  #                    colClasses = cc
  #                    )

  ## TODO further error checking?

  # done
  return(df)
}



