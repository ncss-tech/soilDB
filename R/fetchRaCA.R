## TODO: need to implement some kind of constraints on geographic queries
## TODO: consider compressing the data stream
## TODO: use download.file() to get gzipped stream from server
## TODO: consider compression at the www server: http://httpd.apache.org/docs/2.0/mod/mod_deflate.html

# this loads and packages the data into a list of objects
fetchRaCA <- function(series=NULL, bbox=NULL) {
  
  # sanity-check: user must supply some kind of criteria
  if(missing(series) & missing(bbox))
    stop('you must provide some filtering criteria')
  
  # init empty filter
  f <- vector()
  
  # init empty pieces
  s <- NULL
  h <- NULL
  trees <- NULL
  veg <- NULL
  conc <- NULL
  stock <- NULL
  
  # process filter components
  if(!missing(series)) {
    f <- c(f, paste('&series=', series, sep=''))
  }
  
  if(!missing(bbox)) {
    bbox <- paste(bbox, collapse=',')
    f <- c(f, paste('&bbox=', bbox, sep=''))
  }
  
  # combine filters
  f <- paste(f, collapse='')
  
  # build URLs
  site.url <- url(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=site', f, sep=''))
  hz.url <- url(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=horizon', f, sep=''))
  trees.url <- url(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=trees', f, sep=''))
  veg.url <- url(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=veg', f, sep=''))
  conc.url <- url(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=conc', f, sep=''))
  stock.url <- url(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=stock', f, sep=''))
  
  
  # load pieces
  try(s <- read.table(site.url, header=TRUE, sep='|', stringsAsFactors=FALSE), silent=TRUE)
  try(h <- read.table(hz.url, header=TRUE, sep='|', stringsAsFactors=FALSE), silent=TRUE)
  try(trees <- read.table(trees.url, header=TRUE, sep='|', stringsAsFactors=FALSE), silent=TRUE)
  try(veg <- read.table(veg.url, header=TRUE, sep='|', stringsAsFactors=FALSE), silent=TRUE)
  try(conc <- read.table(conc.url, header=TRUE, sep='|', stringsAsFactors=FALSE), silent=TRUE)
  try(stock <- read.table(stock.url, header=TRUE, sep='|', stringsAsFactors=FALSE), silent=TRUE)
  
  # report missing data
  if(all(c(is.null(s), is.null(h)))) {
    stop('query returned no data', call.=FALSE)
  }
  
  # upgrade to SoilProfileCollection
  depths(h) <- rcapid ~ hzdept + hzdepb
  site(h) <- s
  
  # don't init coordinates, as it would be hard for the user to update later
  # coordinates(h) <- ~ x + y
  # proj4string(h) <- '+proj=longlat +datum=WGS84'
  
  # pack into a list and return
  return(list(pedons=h, trees=trees, veg=veg, conc=conc, stock=stock))
}
