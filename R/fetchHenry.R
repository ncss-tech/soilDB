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



# this loads and packages the data into a list of objects
fetchHenry <- function(usersiteid=NULL, project=NULL, sso=NULL, type='soiltemp', gran='day', start.date=NULL, stop.date=NULL, pad.missing.days=TRUE, soiltemp.summaries=TRUE) {
  
  # important: change the default behavior of data.frame
  opt.original <- options(stringsAsFactors = FALSE)
  
  # sanity-check: user must supply some kind of criteria
  if(missing(usersiteid) & missing(project) & missing(type) & missing(sso))
    stop('you must provide some filtering criteria', call.=FALSE)
  
  # init empty filter
  f <- vector()
  
  # init empty pieces
  s <- NULL
  soiltemp <- NULL
  
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
  
  if(!is.null(type)) {
    f <- c(f, paste('&type=', type, sep=''))
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
  site.url <- URLencode(paste('http://soilmap2-1.lawr.ucdavis.edu/henry/query.php?what=site', f, sep=''))
  soiltemp.url <- URLencode(paste('http://soilmap2-1.lawr.ucdavis.edu/henry/query.php?what=soil_temperature', f, sep=''))
  
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
      soiltemp <- ddply(soiltemp, c('sid', 'year'), .fill_missing_days)
      message(paste0('padded ', length(is.na(soiltemp$sensor_value)), ' missing soil temperature values'))
    }
    
    # add-in seasons
    soiltemp$season <- .month2season(soiltemp$month)
    
    # get period of record for each site, not including NA-padding
    por <- ddply(soiltemp, c('sid'), function(i) {
      idx <- which(!is.na(i$sensor_value))
      start.date <- min(i$date_time[idx], na.rm=TRUE)
      end.date <- max(i$date_time[idx], na.rm=TRUE)
      return(data.frame(start.date, end.date))
    })
    
    # compute days since last visit
    por$dslv <- round(as.numeric(difftime(Sys.Date(), por$end.date, units='days')))
    
    # optionally compute summaries, requires padded NA values and, daily granularity
    if(soiltemp.summaries & pad.missing.days) {
      message('computing un-biased soil temperature summaries')
      
      if(gran != 'day')
        stop('soil temperature summaries can only be computed from daily data', call. = FALSE)
      
      # compute unbiased estimates of MAST and summer/winter temp
      soiltemp.summary <- .summarizeSoilTemperature(soiltemp)
      
      # combine summaries and join to site data
      por <- join(por, soiltemp.summary, by='sid')
    }
    
    # splice-into site data
    s <- join(s, por, by='sid')
  }
    
  # init coordinates
  if(!is.null(s)) {
    coordinates(s) <- ~ wgs84_longitude + wgs84_latitude
    proj4string(s) <- '+proj=longlat +datum=WGS84'
  }
  
  # reset options:
  options(opt.original)
  
  # pack into a list for the user
  res <- list(sensors=s, soiltemp=soiltemp)
  res.size <- round(object.size(res) / 1024 / 1024, 2)
  
  ## note: this is kind of wasteful, but helps with plotting
  # copy over sensor name to soiltemp table
  res$soiltemp$name <- res$sensors$name[match(res$soiltemp$sid, res$sensors$sid)]
  
  # some feedback via message:
  message(paste(length(unique(s$user_site_id)), ' sites loaded (', res.size, ' Mb transferred)', sep=''))
  
  # done
  return(res)
  
}
