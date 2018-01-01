# convenient interface to local NASIS data
# from: pedons | components | lab | ???
# ... : arguments passed on to helper functions
fetchNASIS <- function(from='pedons', ...) {
	
  res <- NULL
  
  # sanity check
  if(! from %in% c('pedons', 'components')) {
    stop('Must specify: pedons or components', call. = FALSE)
  }
  
  if(from == 'pedons') {
    # pass arguments through
    res <- fetchNASIS_pedons(...)
  }
  
  if(from == 'components') {
    # pass arguments through
    res <- fetchNASIS_components(...)
  }
  
  return(res)
  
}
