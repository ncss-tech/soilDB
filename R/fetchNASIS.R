# convenient interface to local NASIS data
# from: pedons | components | lab | ???
# ... : arguments passed on to helper functions
fetchNASIS <- function(from='pedons', url = NULL, ...) {
	
  res <- NULL
  
  # sanity check
  if(! from %in% c('pedons', 'components', 'pedon_report')) {
    stop('Must specify: pedons, components or pedon_report', call. = FALSE)
  }
  
  if(from == 'pedons') {
    # pass arguments through
    res <- fetchNASIS_pedons(...)
  }
  
  if(from == 'components') {
    # pass arguments through
    res <- fetchNASIS_components(...)
  }
  
  if(from == 'pedon_report') {
    # pass arguments through
    res <- .fetchNASIS_report(url = url, ...)
  }
  
  return(res)
  
}
