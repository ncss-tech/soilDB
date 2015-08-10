## TODO: better checking of inputs, as the entitre DB could be downloaded by accident!!

.month2season <- function(x) {
  season <- rep(NA, times=length(x))
  season[x %in% c('Jun', 'Jul', 'Aug')] <- 'Summer'
  season[x %in% c('Dec', 'Jan', 'Feb')] <- 'Winter'
  season[x %in% c('Mar', 'Apr', 'May')] <- 'Spring'
  season[x %in% c('Sep', 'Oct', 'Nov')] <- 'Fall'
  # fix factor levels for season
  season <- factor(season, levels=c('Winter', 'Spring', 'Summer', 'Fall'))
  return(season)
}


# experimental function for padding daily time-series with NA in the presence of missing days
# must be run on subsets defined by year
.fill_missing_days <- function(x) {
  
  ## TODO this doesn't account for leap-years
  # ID missing days 
  missing.days <- which(is.na(match(1:365, x$doy)))
  
  if(length(missing.days) < 1)
    return(x)
  
  # get constants
  this.id <- unique(x$id)
  
  # make fake date-times for missing data
  this.year <- unique(x$year)
  fake.datetimes <- paste0(this.year, ' ', missing.days, ' 00:00')
  fake.datetimes <- as.POSIXct(fake.datetimes, format="%Y %j %H:%M")
  
  # generate DF with missing information
  fake.data <- data.frame(id=this.id, date_time=fake.datetimes, year=this.year, doy=missing.days, month=format(fake.datetimes, "%b"))
  
  # splice in missing data via full join
  y <- join(x, fake.data, by='doy', type='full')
  
  # re-order by DOY and return
  return(y[order(y$doy), ])
}



# this loads and packages the data into a list of objects
fetchHenry <- function(usersiteid=NULL, project=NULL, type='soiltemp', gran='day', start.date=NULL, stop.date=NULL, pad.missing.days=TRUE) {
  
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
  if(!is.null(soiltemp)) {
    soiltemp$date_time <- as.POSIXct(soiltemp$date_time)
    soiltemp$year <- as.integer(format(soiltemp$date_time, "%Y"))
    soiltemp$doy <- as.integer(format(soiltemp$date_time, "%j"))
    soiltemp$month <- format(soiltemp$date_time, "%b")
    # re-level months
    soiltemp$month <- factor(soiltemp$month, levels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
            
    # optionally pad daily data with NA
    if(gran == 'day' & pad.missing.days) {
      soiltemp <- ddply(soiltemp, c('id', 'year'), .fill_missing_days)
      message(paste0('padded ', length(is.na(soiltemp$sensor_value)), ' missing soil temperature values'))
    }
    
    # add-in seasons
    soiltemp$season <- .month2season(soiltemp$month)
    
  }
    
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
