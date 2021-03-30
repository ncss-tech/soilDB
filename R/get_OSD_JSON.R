#' Fetch Official Series Description Raw Data in JSON format
#'
#' @param series A character vector of Official Series names e.g. `"Chewacla"`
#' @param base_url Optional: alternate JSON repository path. Default: `NULL` uses \url{https://github.com/ncss-tech/SoilKnowledgeBase}.
#'
#' @details The default `base_url` is to "raw" JSON files stored in a GitHub repository that is regularly updated from the official source of Series Descriptions. Using format: `https://raw.githubusercontent.com/ncss-tech/SoilKnowledgeBase/main/inst/extdata/OSD/{LETTER}/{SERIES}.json`
#'
#' @return A `data.frame` with 1 row per series, and 1 column per "section" in the OSD as defined in National Soil Survey Handbook
#' @export
#'
#' @examples
#'
#' \donttest{
#' if(requireNamespace("curl") &
#'    curl::has_internet()) {
#'    
#' series <- c("Musick", "Hector", "Chewacla")
#' get_OSD_JSON(series)
#' }
#' }
get_OSD_JSON <- function(series,
                          base_url = NULL) {

  # http://github.com/ncss-tech/SoilKnowledgeBase is default JSON repository path
  if (missing(base_url) || is.null(base_url))
    base_url <- "https://raw.githubusercontent.com/ncss-tech/SoilKnowledgeBase/main/inst/extdata/OSD"

  if (!requireNamespace("jsonlite"))
    stop("package `jsonlite` is required", call. = FALSE)

  # convert series name to upper case and remove NA
  series <- toupper(na.omit(series))

  # get first letter of each taxon (if any)
  if (length(series) > 0 && all(nchar(series) > 1)) {
    firstLetter <- substr(series, 0, 1)
  } else stop("argument `series` should be character vector of existing official series names", call. = FALSE)

  # construct URL
  path <- file.path(base_url, firstLetter, paste0(series, ".json"))

  # query, handle errors, return 'tidy' data.frame result
  data.frame(data.table::rbindlist(lapply(path, function(p) {

    jsp <- try(jsonlite::read_json(p), silent = TRUE)

    # warning will be generated for non-existent URL
    if (inherits(jsp, 'try-error'))
      return(NULL)

    res <- try({
      data.table::as.data.table(lapply(jsp, function(x) {
        res2 <- x[[length(x)]]
        if (is.null(res2))
          res2 <- NA
        res2
      }))
    }, silent = FALSE)

    # handles weird cases
    if (inherits(res, 'try-error'))
      return(NULL)

    return(res)
  })))

}
