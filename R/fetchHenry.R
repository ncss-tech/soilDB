## TODO: better checking of inputs, as the entitre DB could be downloaded by accident!!

## TODO: vectorize
# vectors of MAST, summer mean, winter mean all in Deg C
.estimateSTR <- function(mast, mean.summer, mean.winter) {
  
  # check to make sure that the lengths of vectors are the same
  if(! all.equal(length(mast), length(mean.summer), length(mean.winter)))
    stop('inputs must all have the same length', call. = TRUE)
  
  # iterate over input
  n <- length(mast)
  res <- vector(mode = 'character', length = n)
  
  for(i in seq_along(mast)) {
    # check for NA
    if(any(is.na(c(mast[i], mean.summer[i], mean.winter[i])))){
      res[i] <- NA
      next
    }
    
    # gelic, suborder and GG levels
    if(mast[i] <= 0) {
      res[i] <- 'gelic*'
      next
    }
    
    # gelic, order level
    if(mast[i] <= 1) {
      res[i] <- 'gelic*'
      next
    }
    
    
    # possibly cryic, because we don't know saturation and O hz status
    if(mast[i] <= 8) {
      if(mean.summer[i] <= 8) {
        res[i] <- 'cryic*'
        next
      }
    }
    
    # frigid
    if(mast[i] <= 8) {
      if(mean.summer[i] - mean.winter[i] >= 6) {
        res[i] <- 'frigid*'
        next
      }
      else {
        res[i] <- 'isofrigid'
        next
      }
    }
    
    # mesic
    if(mast[i] >= 8 & mast[i] < 15) {
      if(mean.summer[i] - mean.winter[i] >= 6) {
        res[i] <- 'mesic'
        next
      }
      else {
        res[i] <- 'isomesic'
        next
      }
    }
    
    # thermic
    if(mast[i] >= 15 & mast[i] < 22) {
      if(mean.summer[i] - mean.winter[i] >= 6){
        res[i] <- 'thermic'
        next
      }
      else {
        res[i] <- 'isothermic'
        next
      }
    }
    
    # hyperthermic
    if(mast[i] >= 22) {
      if(mean.summer[i] - mean.winter[i] >= 6) {
        res[i] <- 'hyperthermic'
        next
      }
      else {
        res[i] <- 'isohyperthermic'
        next
      }
    }
    
    # unknown
    res[i] <- NA
  }
  
  # set levels
  res <- factor(res, levels=c('gelic*', 'cryic*','frigid*','isofrigid','mesic','isomesic','thermic','isothermic','hyperthermic','isohyperthermic'))
  
  # done
  return(res)
}

# summarize daily values via julian day
.summarizeSoilTemperature <- function(soiltemp.data) {
  
  # hacks to make R CMD check --as-cran happy:
  n <- NULL
  n.total <- NULL
  sensor_value <- NULL
  non.missing <- NULL
  daily.mean <- NULL
  summarize <- NULL
  
  # determine number of complete years of data
  cr.1 <- ddply(soiltemp.data, c('sid', 'year'), plyr::summarize, non.missing=length(na.omit(sensor_value)))
  cr.2 <- ddply(cr.1, 'sid', plyr::summarize, complete.yrs=length(which(non.missing >= 365)))
  
  # determine functional years of data
  # number of complete years after accounting for overlap
  fy <- ddply(soiltemp.data, 'sid', .fun=function(i) {
    # convert current sensor's data to wide format, first row is the year
    w <- dcast(i, year ~ doy, value.var = 'sensor_value')
    # on DOY 1-365, count total number of non-NA records over all years
    non.na.doy <- apply(w[, 2:366], 2, function(j) length(na.omit(j)))
    # the minimum value is the number of functional years
    return(data.frame(functional.yrs=min(non.na.doy)))
  })
  
  # compute summaries by DOY:
  # n: number of non-NA records
  # daily.mean: mean of non-NA values
  d <- ddply(soiltemp.data, c('sid', 'doy'), .fun=plyr::summarize, .progress='text', 
             n.total=length(sensor_value),
             n=length(na.omit(sensor_value)), 
             daily.mean=mean(sensor_value, na.rm=TRUE))
  
  # convert DOY -> month
  d$month <- format(as.Date(as.character(d$doy), format="%j"), "%b")
  d$season <- .month2season(d$month)
  
  # compute unbiased MAST, number of obs, complete records per average no. days in year
  d.mast <- ddply(d, 'sid', .fun=plyr::summarize, 
                  gap.index=round(1 - (sum(n) / sum(n.total)), 2),
                  days.of.data=sum(n), 
                  MAST=round(mean(daily.mean, na.rm=TRUE), 2)
                  )
  
  # compute unbiased seasonal averages
  d.seasonal.long <- ddply(d[which(d$season %in% c('Winter', 'Summer')), ], c('season', 'sid'), 
                           .fun=plyr::summarize, seasonal.mean.temp=round(mean(daily.mean, na.rm=TRUE), 2))
  
  # convert seasonal avgs to wide format
  d.season <- dcast(d.seasonal.long, sid ~ season, value.var = 'seasonal.mean.temp')
  
  # combine columns
  d.summary <- join(d.mast, d.season, by = 'sid')
  d.summary <- join(d.summary, cr.2, by='sid')
  d.summary <- join(d.summary, fy, by='sid')
  
  # estimate STR, note that gelic / cryic assignment is problematic
  d.summary$STR <- .estimateSTR(d.summary$MAST, d.summary$Summer, d.summary$Winter)
  
  # re-shuffle columns and return
  return(d.summary[, c('sid', 'days.of.data', 'gap.index', 'functional.yrs', 'complete.yrs', 'MAST', 'Winter', 'Summer', 'STR')])
}


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
  this.id <- unique(x$sid)
  
  # make fake date-times for missing data
  this.year <- unique(x$year)
  fake.datetimes <- paste0(this.year, ' ', missing.days, ' 00:00')
  fake.datetimes <- as.POSIXct(fake.datetimes, format="%Y %j %H:%M")
  
  # generate DF with missing information
  fake.data <- data.frame(sid=this.id, date_time=fake.datetimes, year=this.year, doy=missing.days, month=format(fake.datetimes, "%b"))
  
  # splice in missing data via full join
  y <- join(x, fake.data, by='doy', type='full')
  
  # re-order by DOY and return
  return(y[order(y$doy), ])
}


.formatDates <- function(sensor.data, gran, pad.missing.days) {
  sensor.data$date_time <- as.POSIXct(sensor.data$date_time)
  sensor.data$year <- as.integer(format(sensor.data$date_time, "%Y"))
  sensor.data$doy <- as.integer(format(sensor.data$date_time, "%j"))
  sensor.data$month <- format(sensor.data$date_time, "%b")
  # re-level months
  sensor.data$month <- factor(sensor.data$month, levels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
  
  # optionally pad daily data with NA
  if(gran == 'day' & pad.missing.days) {
    sensor.data <- ddply(sensor.data, c('sid', 'year'), .fill_missing_days)
    # message(paste0('padded ', length(is.na(sensor.data$sensor_value)), ' missing values'))
  }
  
  # add-in seasons
  sensor.data$season <- .month2season(sensor.data$month)
  
  return(sensor.data)
}



# this loads and packages the data into a list of objects
fetchHenry <- function(usersiteid=NULL, project=NULL, sso=NULL, gran='day', start.date=NULL, stop.date=NULL, pad.missing.days=TRUE, soiltemp.summaries=TRUE) {
  
  # check for required packages
  if(!requireNamespace('jsonlite', quietly=TRUE))
    stop('please install the `jsonlite` packages', call.=FALSE)
  
  # important: change the default behavior of data.frame
  opt.original <- options(stringsAsFactors = FALSE)
  
  # sanity-check: user must supply some kind of criteria
  if(missing(usersiteid) & missing(project) & missing(sso))
    stop('you must provide some filtering criteria', call.=FALSE)
  
  # init empty filter
  f <- vector()
  
  # init empty pieces
  s <- NULL
  
  # process filter components
  if(!is.null(usersiteid)) {
    f <- c(f, paste('&usersiteid=', usersiteid, sep=''))
  }
  
  if(!is.null(project)) {
    project <- paste(project, collapse=',')
    f <- c(f, paste('&project=', project, sep=''))
  }
  
  if(!is.null(sso)) {
    sso <- paste(sso, collapse=',')
    f <- c(f, paste('&sso=', sso, sep=''))
  }
  
  if(!is.null(gran)) {
    f <- c(f, paste('&gran=', gran, sep=''))
  }
  
  if(!is.null(start.date)) {
    f <- c(f, paste('&start=', start.date, sep=''))
  }
  
  if(!is.null(stop.date)) {
    f <- c(f, paste('&stop=', stop.date, sep=''))
  }
  
  # combine filters
  f <- paste(f, collapse='')
  
  # debugging
#   print(f)
  
  # build URLs
  json.url <- URLencode(paste('http://soilmap2-1.lawr.ucdavis.edu/henry/query.php?json=1', f, sep=''))
  
  # init temp file
  tf.json <- tempfile()
  
  # download all data via JSON interface
  download.file(url=json.url, destfile=tf.json, mode='wb', quiet=FALSE)
  
  ## TODO: check NA handling
  # parse JSON into list of DF
  try(s <- jsonlite::fromJSON(gzfile(tf.json)))
  
  
  # report missing data
  if(is.null(s$sensors)) {
    stop('query returned no data', call.=FALSE)
  }
  
  
  
  
  
  # convert date/time
  if( !is.null(s$soiltemp)) {
    
    # get period of record for each sensor, not including NA-padding
    por <- ddply(na.omit(rbind(s$soiltemp, s$soilVWC)), c('sid'), function(i) {
      start.date <- min(i$date_time, na.rm=TRUE)
      end.date <- max(i$date_time, na.rm=TRUE)
      return(data.frame(start.date, end.date))
    })
    
    # compute days since last visit
    por$dslv <- round(as.numeric(difftime(Sys.Date(), por$end.date, units='days')))
    
    # convert dates and add helper column
    s$soiltemp <- .formatDates(s$soiltemp, gran=gran, pad.missing.days=pad.missing.days)
    s$soilVWC <- .formatDates(s$soilVWC, gran=gran, pad.missing.days=pad.missing.days)
    
    # optionally compute summaries, requires padded NA values and, daily granularity
    if(soiltemp.summaries & pad.missing.days) {
      message('computing un-biased soil temperature summaries')
      
      if(gran != 'day')
        stop('soil temperature summaries can only be computed from daily data', call. = FALSE)
      
      # compute unbiased estimates of MAST and summer/winter temp
      soiltemp.summary <- .summarizeSoilTemperature(s$soiltemp)
      
      # combine summaries and join to sensors data
      por <- join(por, soiltemp.summary, by='sid')
    }
    
    # splice-into sensors data
    s$sensors <- join(s$sensors, por, by='sid')
  }
  
  ## TODO: abstract into more efficient function
  # copy over sensor name to sensor.data table
  name.idx <- match(s$soiltemp$sid, s$sensors$sid)
  s$soiltemp$name <- paste0(s$sensors$name[name.idx], '-', s$sensors$sensor_depth[name.idx])
  
  name.idx <- match(s$soilVWC$sid, s$sensors$sid)
  s$soilVWC$name <- paste0(s$sensors$name[name.idx], '-', s$sensors$sensor_depth[name.idx])
  
  
  # init coordinates
  if(!is.null(s$sensors)) {
    coordinates(s$sensors) <- ~ wgs84_longitude + wgs84_latitude
    proj4string(s$sensors) <- '+proj=longlat +datum=WGS84'
  }
  
  # reset options:
  options(opt.original)
  
  s.size <- round(object.size(s) / 1024 / 1024, 2)
  
  # some feedback via message:
  message(paste(nrow(s$sensors), ' sensors loaded (', s.size, ' Mb transferred)', sep=''))
  
  # done
  return(s)
  
}
