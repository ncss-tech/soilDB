
# https://www.postgresql.org/docs/9.5/static/textsearch-controls.html

# these are all parameters expected by the SoilWeb OSD Fulltext search
OSDquery <- function(mlra='', taxonomic_class='', typical_pedon='', brief_narrative='', ric='', use_and_veg='', competing_series='', geog_location='', geog_assoc_soils='') {
  
  # check for required packages
  if(!requireNamespace('httr', quietly=TRUE) | !requireNamespace('jsonlite', quietly=TRUE))
    stop('please install the `httr` and `jsonlite` packages', call.=FALSE)
  
  # sanity checks
  
  # build parameters list
  parameters=list(json=1,
                  mlra=mlra,
                  taxonomic_class=taxonomic_class, 
                  typical_pedon=typical_pedon, 
                  brief_narrative=brief_narrative, 
                  ric=ric, 
                  use_and_veg=use_and_veg, 
                  competing_series=competing_series, 
                  geog_location=geog_location, 
                  geog_assoc_soils=geog_assoc_soils
                  )
  
  # API URL
  # note: this is the load-balancer
  u <- 'https://casoilresource.lawr.ucdavis.edu/osd-search/index.php'
  
  # POST it
  res <- httr::POST(u, body=parameters, encode='form')
  
  # TODO: figure out what an error state looks like
  # trap errors, likely related to SQL syntax errors
  request.status <- try(httr::stop_for_status(res), silent = TRUE)
  
  # the result is JSON
  # should simplify to data.frame nicely
  r.content <- httr::content(res, as = 'text', encoding = 'UTF-8')
  d <- jsonlite::fromJSON(r.content)
  
  # results will either be: data.frame, empty list, or NULL
  
  # ensure result is either data.frame or NULL
  if(inherits(d, 'list') & length(d) < 1)
    return(NULL)
  
  return(d)
}


