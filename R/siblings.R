
# 2018-11-14
## TODO: launder series names, all upper case?
# return information on soil series that co-occur with `s`
# component.data: should the component names, kind, percent, etc. be returned as well?
# cousins: return siblings of siblings (cousins)?
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


