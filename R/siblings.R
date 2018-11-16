
## TODO: document this!!

# 2018-11-14
# return information on soil series that co-occur with `s`
# component.data: should the component names, kind, percent, etc. be returned as well?
# cousins: return siblings of siblings (cousins)?
siblings <- function(s, component.data=FALSE, cousins=FALSE) {
  
  # helper functions
  .getSibling <- function(i) {
    # these use the new API
    u <- sprintf('http://soilmap2-1.lawr.ucdavis.edu/api/soil-series.php?q=siblings&s=%s', i)
    
    # attempt query to API for basic sibling set, result is JSON
    sib <- try(jsonlite::fromJSON(u))[[1]]
    
    # TODO, handle errors
    
    # result is FALSE if no matching data
    return(sib)
  }
  
  .getSiblingData <- function(i) {
    # these use the new API
    u <- sprintf('http://soilmap2-1.lawr.ucdavis.edu/api/soil-series.php?q=sibling_data&s=%s', i)
    
    # attempt query to API for component data, result is JSON
    sib <- try(jsonlite::fromJSON(u))[[1]]
    
    # TODO, handle errors
    
    # result is FALSE if no matching data
    return(sib)
  }
  
  # init output as list
  res <- list()
  
  # sanity check
  if( !requireNamespace('jsonlite', quietly=TRUE))
    stop('please install the `jsonlite` package', call.=FALSE)
  
  # get basic data
  res$sib <- .getSibling(s)
  
  # optionally get data
  if(component.data) {
    res$sib.data <- .getSiblingData(s)
  }
  
  # optionally get second set of siblings
  # flatten into single DF
  if(cousins) {
    cousins <- lapply(res$sib$sibling, .getSibling)
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


