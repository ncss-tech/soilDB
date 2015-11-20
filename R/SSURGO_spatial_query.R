
# currently only queries SoilWeb for mapunit-level data
SoilWeb_spatial_query <- function(bbox=NULL, coords=NULL, what='mapunit', source='soilweb') {
  
  # check for required packages
  if(!requireNamespace('jsonlite', quietly = TRUE))
    stop('please install the `jsonlite` package', call.=FALSE)
  
  # no factors in resulting DFs
  options(stringsAsFactors=FALSE)
  
  # sanity-check: user must supply some kind of criteria
  if(missing(coords) & missing(bbox))
    stop('you must provide some filtering criteria')
  
  # can't provide both coords and bbox
  if(! missing(coords) & ! missing(bbox))
    stop('query cannot include both bbox and point coordinates')
  
  # init empty filter
  f <- vector()
  
  # init empty pieces
  res <- NULL
  
  # process filter components
  if(!missing(coords)) {
    f <- c(f, paste('&lon=', coords[1], '&lat=', coords[2], sep=''))
  }
  
  if(!missing(bbox)) {
    bbox <- paste(bbox, collapse=',')
    f <- c(f, paste('&bbox=', bbox, sep=''))
  }
  
  # build URL
  the.url <- paste('http://casoilresource.lawr.ucdavis.edu/soil_web/api/ssurgo.php?what=mapunit', f, sep='')
  
  ## TODO: this isn't robust
  # load data from JSON
  suppressWarnings(try(res <- jsonlite::fromJSON(the.url), silent=TRUE))
  
  # report missing data
  if(is.null(res)) {
    stop('query returned no data', call.=FALSE)
  }
  
  return(res)
}
