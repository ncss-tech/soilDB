#' Fetch Official Series Description Raw Data in JSON format
#'
#' @param series A character vector of Official Series names e.g. `"Chewacla"`
#' @param base_url Optional: alternate JSON repository path. Default: `NULL` uses \url{http://github.com/ncss-tech/SoilKnowledgeBase}.
#' 
#' @details The default `base_url` is to "raw" JSON files stored in a GitHub repository that is regularly updated from the official source of Series Descriptions. Using format: `https://raw.githubusercontent.com/ncss-tech/SoilKnowledgeBase/main/inst/extdata/OSD/{LETTER}/{SERIES}.json`
#' 
#' @return A `data.frame` with 1 row per series, and 1 column per "section" in the OSD as defined in National Soil Survey Handbook 
#' @export
#'
#' @examples
#' 
#' series <- c("Musick", "Hector", "Chewacla")
#' fetchOSD_JSON(series)
#' 
fetchOSD_JSON <- function(series, 
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
  do.call('rbind', lapply(path, function(p) {
    
    # warning will be generated for non-existent URL
    res <- try(data.frame(lapply(jsonlite::read_json(p), function(x) x[[length(x)]])),
               silent = TRUE)
    
    if (inherits(res, 'try-error'))
      return(NULL)
    
    return(res)
  }))

}
