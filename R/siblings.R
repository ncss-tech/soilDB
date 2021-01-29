# 2018-11-14
## TODO: launder series names, all upper case?
# return information on soil series that co-occur with `s`
# component.data: should the component names, kind, percent, etc. be returned as well?
# cousins: return siblings of siblings (cousins)?


#' Lookup siblings and cousins for a given soil series.
#' 
#' Lookup siblings and cousins for a given soil series, from the current fiscal
#' year SSURGO snapshot via SoilWeb.
#' 
#' The siblings of any given soil series are defined as those soil series
#' (major and minor component) that share a parent map unit with the named
#' series (as a major component). Cousins are siblings of siblings. Data are
#' sourced from SoilWeb which maintains a copy of the current SSURGO snapshot.
#' 
#' @param s character vector, the name of a single soil series,
#' case-insensitive.
#' @param only.major logical, should only return siblings that are major
#' components
#' @param component.data logical, should component data for siblings (and
#' optionally cousins) be returned?
#' @param cousins logical, should siblings-of-siblings (cousins) be returned?
#' @return \describe{ \item{sib}{\code{data.frame} containing siblings, major
#' component flag, and number of co-occurrences}
#' \item{sib.data}{\code{data.frame} containing sibling component data}
#' \item{cousins}{\code{data.frame} containing cousins, major component flag,
#' and number of co-occurrences} \item{cousin.data}{\code{data.frame}
#' containing cousin component data} }
#' @author D.E. Beaudette
#' @seealso \link{OSDquery}, \link{siblings}, \link{fetchOSD}
#' @references
#'  - [Soil Series Query Functions](http://ncss-tech.github.io/AQP/soilDB/soil-series-query-functions.html)
#'  - [Soil "Siblings" Tutorial](http://ncss-tech.github.io/AQP/soilDB/siblings.html)
#' @keywords manip
#' @examples
#' 
#' \donttest{
#' if(requireNamespace("curl") &
#'     curl::has_internet()) {
#'     
#'     # basic usage
#'     x <- siblings('zook')
#'     x$sib
#'     
#'     # restrict to siblings that are major components
#'     # e.g. the most likely siblings
#'     x <- siblings('zook', only.major = TRUE)
#'     x$sib
#' }
#' }
#' 
#' @export siblings
siblings <- function(s, only.major=FALSE, component.data=FALSE, cousins=FALSE) {
  
  # helper functions
  .getSibling <- function(i, only.major) {
    # these use the new API
    u <- URLencode(sprintf('https://casoilresource.lawr.ucdavis.edu/api/soil-series.php?q=siblings&s=%s', i))
    
    # attempt query to API for basic sibling set, result is JSON
    sib <- try(jsonlite::fromJSON(u))[[1]]
    
    # a data.frame result measn we have data, otherwise return NULL
    if(inherits(sib, 'data.frame')) {
      
      # convert 'Yes'|'No' -> TRUE|FALSE
      sib$majcompflag <- ifelse(sib$majcompflag == 'Yes', TRUE, FALSE)
      
      # TODO: convert series into title case
      # https://github.com/ncss-tech/soilDB/issues/95
      
      # note: there may be both major and minor siblings
      # optionally cut-down to just major siblings
      if(only.major)
        sib <- sib[which(sib$majcompflag), ]
      
      return(sib)
      
    } else {
      return(NULL)
    }
    
  }
  
  .getSiblingData <- function(i) {
    # these use the new API
    u <- URLencode(sprintf('https://casoilresource.lawr.ucdavis.edu/api/soil-series.php?q=sibling_data&s=%s', i))
    
    # attempt query to API for component data, result is JSON
    # result is FALSE if no matching data
    sib <- try(jsonlite::fromJSON(u))[[1]]
    
    # a data.frame result means we have data, otherwise return NULL
    if(inherits(sib, 'data.frame')) {
      return(sib)
    } else {
      return(NULL)
    }
  }
  
  # init output as list
  res <- list()
  
  # sanity check
  if( !requireNamespace('jsonlite', quietly=TRUE))
    stop('please install the `jsonlite` package', call.=FALSE)
  
  # get basic data
  res$sib <- .getSibling(s, only.major = only.major)
  
  # optionally get data
  if(component.data) {
    res$sib.data <- .getSiblingData(s)
  }
  
  # optionally get second set of siblings
  # flatten into single DF
  if(cousins) {
    cousins <- lapply(res$sib$sibling, .getSibling, only.major = only.major)
    res$cousins <- do.call('rbind', cousins)
    
    # data too?
    if(component.data) {
      cousin.data <- lapply(res$sib$sibling, .getSiblingData)
      res$cousin.data <- do.call('rbind', cousin.data)
    }
  }
  
  # TODO: error checking and further sanity checks
  return(res)
}


