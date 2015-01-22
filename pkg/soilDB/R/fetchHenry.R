## TODO: better checking of inputs, as the entitre DB could be downloaded by accident!!


# this loads and packages the data into a list of objects
fetchHenry <- function(usersiteid=NULL, project=NULL, type='soiltemp', gran='day', start.date=NULL, stop.date=NULL) {
  
  # important: change the default behavior of data.frame
  opt.original <- options(stringsAsFactors = FALSE)
  
  # sanity-check: user must supply some kind of criteria
  if(missing(usersiteid) & missing(project) & missing(type))
    stop('you must provide some filtering criteria', call.=FALSE)
  
  # init empty filter
  f <- vector()
  
  # init empty pieces
  s <- NULL
  soiltemp <- NULL
  
  # process filter components
  if(!missing(usersiteid)) {
    f <- c(f, paste('&usersiteid=', usersiteid, sep=''))
  }
  
  if(!missing(project)) {
    project <- paste(project, collapse=',')
    f <- c(f, paste('&project=', project, sep=''))
  }
  
  if(!missing(type)) {
    f <- c(f, paste('&type=', type, sep=''))
  }
  
  if(!missing(gran)) {
    f <- c(f, paste('&gran=', gran, sep=''))
  }
  
  if(!missing(start.date)) {
    f <- c(f, paste('&start=', start.date, sep=''))
  }
  
  if(!missing(stop.date)) {
    f <- c(f, paste('&stop=', stop.date, sep=''))
  }
  
  # combine filters
  f <- paste(f, collapse='')
  
  # build URLs
  site.url <- URLencode(paste('http://soilmap2-1.lawr.ucdavis.edu/soil_web/henry/query.php?what=site', f, sep=''))
  soiltemp.url <- URLencode(paste('http://soilmap2-1.lawr.ucdavis.edu/soil_web/henry/query.php?what=soil_temperature', f, sep=''))
  
  # init temp files
  tf.site <- tempfile()
  tf.soiltemp <- tempfile()
  
  # download pieces
  download.file(url=site.url, destfile=tf.site, mode='wb', quiet=TRUE)
  download.file(url=soiltemp.url, destfile=tf.soiltemp, mode='wb', quiet=TRUE)
  
  # load pieces
  try(s <- read.table(gzfile(tf.site), header=TRUE, sep='|', quote='', comment.char=''), silent=TRUE)
  try(soiltemp <- read.table(gzfile(tf.soiltemp), header=TRUE, sep='|', quote='', comment.char=''), silent=TRUE)
  
  # report missing data
  if(all(c(is.null(s), is.null(soiltemp)))) {
    stop('query returned no data', call.=FALSE)
  }
  
  # convert date/time
  if(!is.null(soiltemp))
    soiltemp$date_time <- as.POSIXct(soiltemp$date_time)
  
  # init coordinates
  if(!is.null(s)) {
    coordinates(s) <- ~ wgs84_longitude + wgs84_latitude
    proj4string(s) <- '+proj=longlat +datum=WGS84'
  }
  
  # reset options:
  options(opt.original)
  
  # pack into a list for the user
  res <- list(sites=s, soiltemp=soiltemp)
  res.size <- round(object.size(res) / 1024 / 1024, 2)
  
  # some feedback via message:
  message(paste(length(unique(s$user_site_id)), ' sites loaded (', res.size, ' Mb transferred)', sep=''))
  
  # done
  return(res)
  
}
