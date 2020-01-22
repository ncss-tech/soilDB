# convenient interface to local NASIS data
# from: pedons | components | lab | ???
# ... : arguments passed on to helper functions
fetchNASIS <- function(from='pedons', 
                       url = NULL, 
                       SS = TRUE, 
                       rmHzErrors = TRUE,
                       nullFragsAreZero = TRUE, 
                       soilColorState = 'moist', 
                       lab = FALSE, 
                       fill = FALSE,
                       stringsAsFactors = default.stringsAsFactors()
                       ) {
	
  res <- NULL
  
  # sanity check
  if(! from %in% c('pedons', 'components', 'pedon_report')) {
    stop('Must specify: pedons, components or pedon_report', call. = FALSE)
  }
  
  if(from == 'pedons') {
    # pass arguments through
    res <- .fetchNASIS_pedons(SS = SS, 
                              rmHzErrors = rmHzErrors,
                              nullFragsAreZero = nullFragsAreZero, 
                              soilColorState = soilColorState, 
                              lab = lab,
                              stringsAsFactors = stringsAsFactors
                              )
  }
  
  if(from == 'components') {
    # pass arguments through
    res <- .fetchNASIS_components(SS = TRUE,
                                  rmHzErrors = rmHzErrors, 
                                  fill = fill, 
                                  stringsAsFactors = stringsAsFactors
                                  )
  }
  
  if(from == 'pedon_report') {
    # pass arguments through
    res <- .fetchNASIS_report(url              = url,
                              rmHzErrors       = rmHzErrors,
                              nullFragsAreZero = nullFragsAreZero, 
                              soilColorState   = soilColorState, 
                              stringsAsFactors = stringsAsFactors
                              )
  }
  
  return(res)
  
}
