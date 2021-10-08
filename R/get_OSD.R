#' Get Official Series Description Data from JSON, HTML or TXT sources
#'
#' @param series A character vector of Official Series names e.g. `"Chewacla"`
#' @param result Select `"json"`, `"html"`, or `"txt"` output
#'
#' @param base_url Optional: alternate JSON/HTML/TXT repository path. Default: `NULL` uses `"https://github.com/ncss-tech/SoilKnowledgeBase"` for `result="json"`
#'
#' @param verbose Print errors and warning messages related to HTTP requests? Default: `FALSE`
#'
#' @details The default `base_url` for `result="json"` is to JSON files stored in a GitHub repository  that is regularly updated from the official source of Series Descriptions. Using format: `https://raw.githubusercontent.com/ncss-tech/SoilKnowledgeBase/main/inst/extdata/OSD/{LETTER}/{SERIES}.json` for JSON. And `"https://soilseriesdesc.sc.egov.usda.gov/OSD_Docs/{LETTER}/{SERIES}.html` is for `result="html"` (official source).
#'
#' @return For JSON result: A `data.frame` with 1 row per series, and 1 column per "section" in the OSD as defined in National Soil Survey Handbook. For TXT or HTML result a list of character vectors containing OSD text with 1 element per series and one value per line.
#' @export
#' @aliases get_OSD_JSON
#' @examples
#'
#' \donttest{
#' if(requireNamespace("curl") &
#'    curl::has_internet()) {
#'
#' series <- c("Musick", "Hector", "Chewacla")
#' get_OSD(series)
#' }
#' }
get_OSD <- function(series, base_url = NULL, result = c("json","html","txt"), verbose = FALSE) {
  result <- match.arg(tolower(result), c("json","html","txt"))

  a_url <- NULL
  if (!is.null(base_url)) {
    a_url <- base_url
  }

  switch(result,
         "json" = { .get_OSD_JSON(series, base_url = a_url, verbose = verbose) },
         "html" = { .get_OSD_HTML(series, base_url = a_url, verbose = verbose) },
         "txt" =  { .get_OSD_TXT(series, verbose = verbose)  })
}


## DEB: there are funky white-space characters (a0 created by &nbsp;) between location / state on line 2
## DEB: there is no new-line between date and "XXX SERIES" because an <H1> element is used to create vertical space

.get_OSD_HTML <- function(series, base_url = NULL, verbose = FALSE) {
  if(!requireNamespace('rvest', quietly=TRUE))
    stop('please install the `rvest` package', call.=FALSE)

  if (missing(base_url) || is.null(base_url))
    base_url <- 'https://soilseriesdesc.sc.egov.usda.gov/OSD_Docs/'

  # get HTML content and strip blank / NA lines
  res <- sapply(.seriesNameToURL(series, base_url = base_url), function(x) {

    # if the URL is bad a warning with 404 will be generated
    u <- suppressWarnings(try(url(x, "rb"), silent = TRUE))
    if (inherits(u, 'try-error'))
      return(NULL)

    # HTML results as xml_document
    html.raw <- rvest::read_html(u, silent = !verbose)

    # close connection
    close(u)

    ## not quite right, this removes the title but is otherwise a better candidate for future
    # # 2021-09-13: explicit conversion of &nbsp; -> simple white space
    # textResult <- rvest::html_text2(html.raw, preserve_nbsp = FALSE)

    # NOTE: not enough line-breaks between Date and "<H1>XXX SERIES</H1>", lines are joined
    textResult <- rvest::html_text(html.raw)

    # remove extra line-breaks and other cruft
    textResult <- .stripOSDContents(readLines(textConnection(textResult)))

    # line 5 does not break after the date: "09/2021FRESNO SERIES"
    # repair it
    txt.part <- strsplit(textResult[5], "[0-9]+\\/[0-9]+")[[1]][2]
    date.part <- gsub(txt.part, '', textResult[5])

    # correct contents of line 5
    textResult[5] <- date.part

    # insert the correct contents of line 6, shift everything forwards
    c(textResult[1:5], txt.part, textResult[6:length(textResult)])

  })

  names(res) <- toupper(series)
  return(res)
}

.get_OSD_TXT <- function(series, base_url = "", verbose = FALSE) {
  sapply(series, function(x) {
    fp <- .seriesNameToURL(x, base_url = base_url, extension = "txt")

    if (!file.exists(fp))
      return(NULL)

    # remove empty lines and fix other markup
    try(.stripOSDContents(readLines(fp)), silent = !verbose)
  })
}

get_OSD_JSON <- function(series, base_url = NULL) {
  # .Deprecated("get_OSD")
  .get_OSD_JSON(series, base_url)
}

.get_OSD_JSON <- function(series, base_url = NULL, verbose = FALSE) {

  # http://github.com/ncss-tech/SoilKnowledgeBase is default JSON repository path
  if (missing(base_url) || is.null(base_url))
    base_url <- "https://raw.githubusercontent.com/ncss-tech/SoilKnowledgeBase/main/inst/extdata/OSD"

  if (!requireNamespace("jsonlite"))
    stop("package `jsonlite` is required", call. = FALSE)

  ## TODO convert to use .seriesNameToFileName()
  
  # convert series name to upper case and remove NA
  series <- toupper(na.omit(series))
  
  # remove white space on left / right side
  series <- trimws(series, which = 'both')
  
  # convert spaces -> underscore
  series <- gsub(pattern = ' ', replacement = '_', x = series, fixed = TRUE)

  ## TODO if not using convenience function, don't forget apos.
  
  # get first letter of each taxon (if any)
  if (length(series) > 0 && all(nchar(series) > 1)) {
    firstLetter <- substr(series, 0, 1)
  } else stop("argument `series` should be character vector of existing official series names", call. = FALSE)

  # construct URL
  path <- file.path(base_url, firstLetter, paste0(series, ".json"))

  # query, handle errors, return 'tidy' data.frame result
  data.frame(data.table::rbindlist(lapply(seq_along(path), function(i) {

    p <- path[i]
    jsp <- try(jsonlite::read_json(p), silent = TRUE)

    # warning will be generated for non-existent URL
    if (inherits(jsp, 'try-error'))
      return(NULL)

    jspn <- names(jsp)[!names(jsp) %in% c('SITE','HORIZONS')]
    res <- try({
      data.table::as.data.table(lapply(jspn, function(m) {
        x <- jsp[[m]]
        res2 <- x[[length(x)]]
        if (is.null(res2))
          res2 <- NA
        res2
      }))
    }, silent = FALSE)
    colnames(res) <- jspn

    jsp$SITE[[1]][[1]]$id <- i
    res$SITE <- list(data.frame(data.table::rbindlist(lapply(jsp$SITE[[1]], data.frame), fill = TRUE)))
    res$HORIZONS <- list(data.frame(data.table::rbindlist(lapply(jsp$HORIZONS[[1]], data.frame), fill = TRUE)))

    # replace NAs ( from toJSON(na='string') )
    # handle type conversion (mixed NA/numerics will be CHARACTER without conversion)
    res <- type.convert(res, na.strings = "NA", as.is = TRUE)
    res$SITE[[1]] <- type.convert(res$SITE[[1]], na.strings = "NA", as.is = TRUE)
    res$HORIZONS[[1]] <- type.convert(res$HORIZONS[[1]], na.strings = "NA", as.is = TRUE)

    # handles weird cases
    if (inherits(res, 'try-error'))
      return(NULL)

    return(res)
  })))

}

## Migrated / adapted from parseOSD repo
## 2021-06-21
## D.E. Beaudette

# generate a link to the OSD for a vector of series names
.seriesNameToURL <- function(s, base_url = 'http://soilseriesdesc.sc.egov.usda.gov/OSD_Docs/',
                             extension = 'html') {
  paste0(base_url, .seriesNameToFileName(s, extension = extension))
}

# prepare a file name and capitalized-first-letter folder based on a series name
.seriesNameToFileName <- function(s, extension = 'txt') {

  # remove any accidental wite space
  s <- trimws(s, which = 'both')
  
  # convert single space to underscore
  s <- gsub(pattern = ' ', replacement = '_', s, fixed = TRUE)

  # caps
  s <- toupper(s)
  
  # TODO: convert apostrophe
  
  res <- file.path(
    substr(s, 1, 1), 
    sprintf('%s.%s', s, extension)
  )
  
  return(res)
}

  # remove empty lines and NA strip double quotes by converting to " inches"
.stripOSDContents <- function(x) {
  x <- x[which(x != '')]
  x <- x[which(!is.na(x))]

  gsub('"', ' inches', x)

  # convert &nbsp; (\ua0) >- ' '
  res <- gsub('\ua0', ' ', x)

  return(res)
}
