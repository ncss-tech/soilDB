## TODO: need to implement some kind of constraints on geographic queries
## NOTE: each VNIR spectra record is 6.6kb of data (compressed, gzip, level 5)

# this loads and packages the data into a list of objects
fetchRaCA <- function(series=NULL, bbox=NULL, state=NULL, rcasiteid=NULL, get.vnir=FALSE) {
  
  # important: change the default behavior of data.frame
  opt.original <- options(stringsAsFactors = FALSE)
  
  # sanity-check: user must supply some kind of criteria
  if(missing(series) & missing(state) & missing(bbox) & missing(rcasiteid))
    stop('you must provide some filtering criteria', call.=FALSE)
  
  # sanity-check: cannot request VNIR by state
  if(!missing(state) & get.vnir)
    stop('VNIR spectra cannot be requested for an entire state', call.=FALSE)
  
  
  ## 2015-09-23
  ## releasing point data for privates lands may be a problem, coordinates are truncated to 2 decimal places
  message('Site coordinates have been truncated to 2 decimal places, contact the National Soil Survey Center for more detailed coordinates.')
  
  # init empty filter
  f <- vector()
  
  # init empty pieces
  s <- NULL
  h <- NULL
  trees <- NULL
  veg <- NULL
  stock <- NULL
  sample <- NULL
  vnir <- NULL
  spectra <- NULL
  
  # process filter components
  if(!missing(series)) {
    f <- c(f, paste('&series=', series, sep=''))
  }
  
  if(!missing(bbox)) {
    bbox <- paste(bbox, collapse=',')
    f <- c(f, paste('&bbox=', bbox, sep=''))
  }
  
  if(!missing(state)) {
    f <- c(f, paste('&state=', state, sep=''))
  }
  
  if(!missing(rcasiteid)) {
    f <- c(f, paste('&rcasiteid=', rcasiteid, sep=''))
  }
  
  # combine filters
  f <- paste(f, collapse='')
  
  # build URLs
  site.url <- URLencode(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=site', f, sep=''))
  hz.url <- URLencode(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=horizon', f, sep=''))
  trees.url <- URLencode(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=trees', f, sep=''))
  veg.url <- URLencode(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=veg', f, sep=''))
  stock.url <- URLencode(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=stock', f, sep=''))
  sample.url <- URLencode(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=sample', f, sep=''))
  vnir.url <- URLencode(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=vnir', f, sep=''))
  
  # init temp files
  tf.site <- tempfile()
  tf.hz <- tempfile()
  tf.trees <- tempfile()
  tf.veg <- tempfile()
  tf.stock <- tempfile()
  tf.sample <- tempfile()
  tf.vnir <- tempfile()
  
  # download pieces
  download.file(url=site.url, destfile=tf.site, mode='wb', quiet=TRUE)
  download.file(url=hz.url, destfile=tf.hz, mode='wb', quiet=TRUE)
  download.file(url=trees.url, destfile=tf.trees, mode='wb', quiet=TRUE)
  download.file(url=veg.url, destfile=tf.veg, mode='wb', quiet=TRUE)
  download.file(url=stock.url, destfile=tf.stock, mode='wb', quiet=TRUE)
  download.file(url=sample.url, destfile=tf.sample, mode='wb', quiet=TRUE)
  
  # load pieces
  try(s <- read.table(gzfile(tf.site), header=TRUE, sep='|', quote='', comment.char=''), silent=TRUE)
  try(h <- read.table(gzfile(tf.hz), header=TRUE, sep='|', quote='', comment.char=''), silent=TRUE)
  try(trees <- read.table(gzfile(tf.trees), header=TRUE, sep='|', quote='', comment.char=''), silent=TRUE)
  try(veg <- read.table(gzfile(tf.veg), header=TRUE, sep='|', quote='', comment.char=''), silent=TRUE)
  
  ### 2014-01-16: data need to be re-generated, offline for now:
  message('Carbon concentration and stock values are probably wrong, or at least suspect. USE WITH CAUTION.')
  try(stock <- read.table(gzfile(tf.stock), header=TRUE, sep='|', quote='', comment.char=''), silent=TRUE)
  try(sample <- read.table(gzfile(tf.sample), header=TRUE, sep='|', quote='', comment.char=''), silent=TRUE)
  
  # optionally load spectra
  if(get.vnir) {
    message('spectra are large, download may take some time...', appendLF=TRUE)
    
    # save the file locally
    download.file(url=vnir.url, destfile=tf.vnir, mode='wb', quiet=TRUE)
    # try to open
    try(vnir <- read.table(gzfile(tf.vnir), header=TRUE, sep='|'), silent=TRUE)
    
    # test for missing data
    if(!is.null(vnir)) {
      # extract and parse the serialized spectra as matrix
      spectra <- as.matrix(read.table(textConnection(vnir$spectra), header=FALSE, sep=','))
      
      # since order is preserved (row-wise and col-wise), we can assign:
      # rownames = sample_id
      # colnames = wavenumber
      dimnames(spectra)[[1]] <- vnir$sample_id
      dimnames(spectra)[[2]] <- 350:2500
    }
  }
  
  # report missing data
  if(all(c(is.null(s), is.null(h)))) {
    stop('query returned no data', call.=FALSE)
  }
  
  # upgrade to SoilProfileCollection
  depths(h) <- rcapid ~ hzdept + hzdepb
  
  # extract landuse, region, soilgroup as characters
  s$landuse <- substr(s$rcasiteid, 6, 6)
  s$region <- substr(s$rcasiteid, 2, 3)
  s$soilgroup <- substr(s$rcasiteid, 2, 5)
  
  # merge-in site data
  site(h) <- s
  
  # don't init coordinates, as it would be hard for the user to update later
  # coordinates(h) <- ~ x + y
  # proj4string(h) <- '+proj=longlat +datum=WGS84'
  
  # reset options:
  options(opt.original)
  
  # pack into a list for the user
  res <- list(pedons=h, trees=trees, veg=veg, stock=stock, sample=sample, spectra=spectra)
  res.size <- round(object.size(res) / 1024 / 1024, 2)
  
  # some feedback via message:
  message(paste(length(unique(h$rcasiteid)), ' RaCA sites loaded (', res.size, ' Mb transferred)', sep=''))
  
  # done
  return(res)
  
}
