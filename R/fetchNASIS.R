# convenient interface to local NASIS data
# what: pedons | components | lab | ???
# ... : arguments passed on to helper functions
fetchNASIS <- function(what='pedons', ...) {
	
  res <- NULL
  
  # sanity check
  if(! what %in% c('pedons', 'components')) {
    stop('Must specify: pedons or components', call. = FALSE)
  }
  
  if(what == 'pedons') {
    # pass arguments through
    res <- fetchNASIS_pedons(...)
  }
  
  if(what == 'components') {
    # pass arguments through
    res <- fetchNASIS_components(...)
  }
  
  return(res)
  
}
