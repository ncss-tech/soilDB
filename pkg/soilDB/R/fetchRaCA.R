## TODO: need to implement some kind of constraints on geographic queries
## NOTE: each VNIR spectra record is 6.6kb of data (compressed, gzip, level 5)

# this loads and packages the data into a list of objects
fetchRaCA <- function(series=NULL, bbox=NULL, get.vnir=FALSE) {
  
  # important: change the default behavior of data.frame and melt
  opt.original <- options(stringsAsFactors = FALSE)
  
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
  
  # combine filters
  f <- paste(f, collapse='')
  
  # build URLs
  site.url <- URLencode(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=site', f, sep=''))
  hz.url <- URLencode(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=horizon', f, sep=''))
  trees.url <- URLencode(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=trees', f, sep=''))
  veg.url <- URLencode(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=veg', f, sep=''))
  conc.url <- URLencode(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=conc', f, sep=''))
  stock.url <- URLencode(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=stock', f, sep=''))
  vnir.url <- URLencode(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=vnir', f, sep=''))
  
  # init temp files
  tf.site <- tempfile()
  tf.hz <- tempfile()
  tf.trees <- tempfile()
  tf.veg <- tempfile()
  tf.conc <- tempfile()
  tf.stock <- tempfile()
  tf.vnir <- tempfile()
  
  # download pieces
  download.file(url=site.url, destfile=tf.site, mode='wb', quiet=TRUE)
  download.file(url=hz.url, destfile=tf.hz, mode='wb', quiet=TRUE)
  download.file(url=trees.url, destfile=tf.trees, mode='wb', quiet=TRUE)
  download.file(url=veg.url, destfile=tf.veg, mode='wb', quiet=TRUE)
  download.file(url=conc.url, destfile=tf.conc, mode='wb', quiet=TRUE)
  download.file(url=stock.url, destfile=tf.stock, mode='wb', quiet=TRUE)
  
  # load pieces
  try(s <- read.table(gzfile(tf.site), header=TRUE, sep='|'), silent=TRUE)
  try(h <- read.table(gzfile(tf.hz), header=TRUE, sep='|'), silent=TRUE)
  try(trees <- read.table(gzfile(tf.trees), header=TRUE, sep='|'), silent=TRUE)
  try(veg <- read.table(gzfile(tf.veg), header=TRUE, sep='|'), silent=TRUE)
  try(conc <- read.table(gzfile(tf.conc), header=TRUE, sep='|'), silent=TRUE)
  try(stock <- read.table(gzfile(tf.stock), header=TRUE, sep='|'), silent=TRUE)
  
  # optionally load spectra
  if(get.vnir) {
    message('spectra are large, download may take some time...', appendLF=TRUE)
    
    # save the file locally
    download.file(url=vnir.url, destfile=tf.vnir, mode='wb', quiet=FALSE)
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
  site(h) <- s
  
  # don't init coordinates, as it would be hard for the user to update later
  # coordinates(h) <- ~ x + y
  # proj4string(h) <- '+proj=longlat +datum=WGS84'
  
  # reset options:
  options(opt.original)
  
  # pack into a list and return
  return(list(pedons=h, trees=trees, veg=veg, conc=conc, stock=stock, spectra=spectra))
  
}
