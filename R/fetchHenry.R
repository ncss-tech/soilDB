# # TODO: finish this
# .summarizeSoilVWC <- function(soilVWC.data) {
#   
#   # TODO: strip-out bogus data
#   
#   
#   d <- ddply(soilVWC.data, c('sid', 'year'), .progress='text', .fun = function(i) {
#     
#     
#     i.q <- quantile(i$sensor_value, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE)
#     days.gt.q.crit <- length(which(i$sensor_value >= i.q['50%']))
#     days.of.data <- length(na.omit(i$sensor_value))
#     
#     # TODO: these summaries are only useful when you have an entire year of data...
#     res <- data.frame(days.gt.q.crit, days.of.data)
#     return(res)
#   })
#   
#   return(d)
# }

# summarize daily values by Julian day
summarizeSoilTemperature <- function(soiltemp.data) {
  
  # hacks to make R CMD check --as-cran happy:
  sensor_value <- NULL
  V1 <- NULL
  .SD <- NULL
  season <- NULL
  
  # determine number of complete years of data

  # proceed with data.table aggregation / joins
  dd <- as.data.table(soiltemp.data)
  
  # days of real data / site / year
  cr.1 <- dd[, sum(!is.na(sensor_value)), by = c('sid', 'year')]
  # complete yrs of real data / site
  cr.2 <- cr.1[, sum(V1 >= 365), by = 'sid']
  names(cr.2)[2] <- 'complete.yrs'
  
  # determine functional years of data
  # number of complete years after accounting for overlap
  .functionalYrs <- function(i) {
    # convert current sensor's data to wide format, first row is the year
    # note: when all data are NA, dcast will perform an aggregate
    #       take the first value (NA) in this case
    w <- data.table::dcast(i, year ~ doy, value.var = 'sensor_value', fun.aggregate = function(i) i[1])
    # on DOY 1-365, count total number of non-NA records over all years
    non.na.doy <- apply(w[, 2:366], 2, function(j) length(na.omit(j)))
    # the minimum value is the number of functional years
    res <- data.frame(functional.yrs = min(non.na.doy))
    return(res)
  }
  
  fy <- dd[, .functionalYrs(.SD), by = 'sid']
  
  # compute summaries by DOY:
  # n: number of non-NA records
  # daily.mean: mean of non-NA values
  .doySummary <- function(i) {
    res <- data.frame(
      n.total = length(i$sensor_value),
      n = length(na.omit(i$sensor_value)), 
      daily.mean = mean(i$sensor_value, na.rm = TRUE)
    )
    return(res)
  }
  
  d <- dd[, .doySummary(.SD), by = c('sid', 'doy')]
  
  # convert DOY -> month
  d$month <- format(as.Date(as.character(d$doy), format="%j"), "%b")
  d$season <- month2season(d$month)
  
  # compute unbiased MAST, number of obs, complete records per average no. days in year
  .unbiasedMAST <- function(i) {
    res <- data.frame(
      gap.index = round(1 - (sum(i$n) / sum(i$n.total)), 2),
      days.of.data = sum(i$n), 
      MAST = round(mean(i$daily.mean, na.rm=TRUE), 2)
    )
    return(res)
  }
  
  d.mast <- d[, .unbiasedMAST(.SD), by = 'sid']
  
  # compute unbiased seasonal averages
  .seasonalMeanTemp <- function(i) {
    res <- data.frame(
      seasonal.mean.temp = round(mean(i$daily.mean, na.rm = TRUE), 2)
    )
    return(res)
  }
  
  d.seasonal.long <- d[season %in% c('Winter', 'Summer'), .seasonalMeanTemp(.SD), by = c('season', 'sid')]
  
  # convert seasonal avgs to wide format
  d.season <- data.table::dcast(d.seasonal.long, sid ~ season, value.var = 'seasonal.mean.temp')
  
  # combine columns
  d.summary <- merge.data.table(d.mast, d.season, by = 'sid', all.x = TRUE, sort = FALSE)
  d.summary <- merge.data.table(d.summary, cr.2, by = 'sid', all.x = TRUE, sort = FALSE)
  d.summary <- merge.data.table(d.summary, fy, by = 'sid', all.x = TRUE, sort = FALSE)
  
  # estimate STR
  # note that gelic / cryic assignment is problematic when missing O horizon / saturation details
  d.summary$STR <- estimateSTR(d.summary$MAST, d.summary$Summer, d.summary$Winter)
  
  # downgrade to data.frame
  d.summary <- as.data.frame(d.summary)
  
  # re-shuffle columns and return
  return(d.summary[, c('sid', 'days.of.data', 'gap.index', 'functional.yrs', 'complete.yrs', 'MAST', 'Winter', 'Summer', 'STR')])
}


month2season <- function(x) {
  season <- rep(NA, times=length(x))
  
  season[x %in% c('Jun', 'Jul', 'Aug')] <- 'Summer'
  season[x %in% c('Dec', 'Jan', 'Feb')] <- 'Winter'
  season[x %in% c('Mar', 'Apr', 'May')] <- 'Spring'
  season[x %in% c('Sep', 'Oct', 'Nov')] <- 'Fall'
  
  # fix factor levels for season
  season <- factor(season, levels=c('Winter', 'Spring', 'Summer', 'Fall'))
  return(season)
}

## experimental function for padding daily time-series with NA in the presence of missing days
## must be run on subsets defined by year
.fill_missing_days <- function(x) {
  
  ## TODO this doesn't account for leap-years
  # ID missing days 
  missing.days <- which(is.na(match(1:365, x$doy)))
  
  # short-circuit
  if(length(missing.days) < 1) {
    return(x)
  }
  
  # get constants
  this.sid <- x$sid[1]
  this.year <- x$year[1]
  
  # make fake date-times for missing data
  fake.datetimes <- paste0(this.year, ' ', missing.days, ' 00:00')
  
  # TODO: this will result in timezone specific to locale; 
  #  especially an issue when granularity is less than daily or for large extents
  fake.datetimes <- as.POSIXct(fake.datetimes, format="%Y %j %H:%M")
  
  # generate DF with missing information
  fake.data <- data.frame(
    sid = this.sid,
    date_time = fake.datetimes, 
    year = this.year,
    doy = missing.days, 
    month = format(fake.datetimes, "%b")
  )
  
  fill.cols <- which(!colnames(x) %in% colnames(fake.data))
  if (length(fill.cols) > 0) {
    na.data <- as.data.frame(x)[, fill.cols, drop = FALSE][0,, drop = FALSE][1:nrow(fake.data),, drop = FALSE]
    fake.data <- cbind(fake.data, na.data)
  }
  
  # make datatypes for time match
  x$date_time <- as.POSIXct(x$date_time, format="%Y-%m-%d %H:%M:%S")
  
  # splice in missing data
  y <- rbind(x, fake.data)
  
  # re-order by DOY and return
  return(y[order(y$doy), ])
}


.formatDates <- function(sensor.data, gran, pad.missing.days) {
  
  .SD <- NULL
  
  # must have data, otherwise do nothing
  # when sensor data are missing, sensor.data is a list of length 0
  if(length(sensor.data) > 0) {
    
    sensor.data$date_time <- as.POSIXct(sensor.data$date_time)
    sensor.data$year <- as.integer(format(sensor.data$date_time, "%Y"))
    sensor.data$doy <- as.integer(format(sensor.data$date_time, "%j"))
    sensor.data$month <- format(sensor.data$date_time, "%b")
    
    # re-level months
    sensor.data$month <- factor(sensor.data$month, levels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
    
    # optionally pad daily data with NA
    if(gran == 'day' & pad.missing.days) {
      sensor.data <- as.data.table(sensor.data)
      cnm <- colnames(sensor.data)
      sensor.data <- sensor.data[, .fill_missing_days(.SD), by = c('sid', 'year'), .SDcols = cnm]
      sensor.data <- sensor.data[, .SD, .SDcols = cnm]
      sensor.data <- as.data.frame(sensor.data)
    }
    
    # add-in seasons
    sensor.data$season <- month2season(sensor.data$month)
    
    # water year/day: October 1st -- September 30th
    w <- waterDayYear(sensor.data$date_time)
    
    # row-order is preserved
    sensor.data$water_year <- w$wy
    sensor.data$water_day <- w$wd
  }
  
  return(sensor.data)
}



# this loads and packages the data into a list of objects


#' Get data from Henry Mount Soil Temperature and Water Database
#' 
#' This function is a front-end to the REST query functionality of the Henry
#' Mount Soil Temperature and Water Database.
#' 
#' Filling missing days with NA is useful for computing and index of how
#' complete the data are, and for estimating (mostly) unbiased MAST and
#' seasonal mean soil temperatures. Summaries are computed by first averaging
#' over Julian day, then averaging over all days of the year (MAST) or just
#' those days that occur within "summer" or "winter". This approach makes it
#' possible to estimate summaries in the presence of missing data. The quality
#' of summaries should be weighted by the number of "functional years" (number
#' of years with non-missing data after combining data by Julian day) and
#' "complete years" (number of years of data with >= 365 days of non-missing
#' data).
#' 
#' @aliases fetchHenry month2season summarizeSoilTemperature
#' @param what type of data to return: 'sensors': sensor metadata only |
#' 'soiltemp': sensor metadata + soil temperature data | 'soilVWC': sensor
#' metadata + soil moisture data | 'airtemp': sensor metadata + air temperature
#' data | 'waterlevel': sensor metadata + water level data |'all': sensor
#' metadata + all sensor data
#' @param usersiteid (optional) filter results using a NASIS user site ID
#' @param project (optional) filter results using a project ID
#' @param sso (optional) filter results using a soil survey office code
#' @param gran data granularity: "day", "week", "month", "year"; returned data
#' are averages
#' @param start.date (optional) starting date filter
#' @param stop.date (optional) ending date filter
#' @param pad.missing.days should missing data ("day" granularity) be filled
#' with NA? see details
#' @param soiltemp.summaries should soil temperature ("day" granularity only)
#' be summarized? see details
#' @return a list containing: \item{sensors}{a \code{SpatialPointsDataFrame}
#' object containing site-level information} \item{soiltemp}{a
#' \code{data.frame} object containing soil temperature timeseries data}
#' \item{soilVWC}{a \code{data.frame} object containing soil moisture
#' timeseries data} \item{airtemp}{a \code{data.frame} object containing air
#' temperature timeseries data} \item{waterlevel}{a \code{data.frame} object
#' containing water level timeseries data}
#' @note This function and the back-end database are very much a work in
#' progress.
#' @author D.E. Beaudette
#' @seealso \code{\link{fetchSCAN}}
#' @keywords manip
#' 
#' @details See: 
#'  - [Henry Mount Soil Climate Database](http://soilmap2-1.lawr.ucdavis.edu/henry/)
#'  - [`fetchHenry` Tutorial](http://ncss-tech.github.io/AQP/soilDB/Henry-demo.html)
#' 
#' @export fetchHenry
fetchHenry <- function(what='all', usersiteid=NULL, project=NULL, sso=NULL, gran='day', start.date=NULL, stop.date=NULL, pad.missing.days=TRUE, soiltemp.summaries=TRUE) {
  
  # check for required packages
  if(!requireNamespace('jsonlite', quietly=TRUE))
    stop('please install the `jsonlite` packages', call.=FALSE)
  
  # important: change the default behavior of data.frame
  opt.original <- options(stringsAsFactors = FALSE)
  
  # sanity-check: `what` should be within the legal set of options
  if(! what %in% c('all', 'sensors', 'soiltemp', 'soilVWC', 'airtemp', 'waterlevel'))
    stop("`what` must be either: 'all', 'sensors', 'soiltemp', 'soilVWC', 'airtemp', or 'waterlevel'", call.=FALSE)
  
  # sanity-check: user must supply some kind of criteria
  if(what != 'sensors') {
    if(missing(usersiteid) & missing(project) & missing(sso))
      stop('you must provide some filtering criteria', call.=FALSE)
  }
  
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
  
  # everything in one URL / JSON package
  json.url <- URLencode(paste('http://soilmap2-1.lawr.ucdavis.edu/henry/query.php?what=', what, f, sep=''))
  
  # this is a little noisy, but people like to see progress
  tf.json <- tempfile()
  download.file(url=json.url, destfile=tf.json, mode='wb', quiet=FALSE)
  
  ## TODO: check NA handling
  # parse JSON into list of DF
  try({
    s <- jsonlite::fromJSON(gzfile(tf.json))
  })
  
  # report query that returns no data and stop
  if( length(s$sensors) == 0 ) {
    stop('query returned no data', call.=FALSE)
  }
  
  # post-process data, if there are some
  if( length(s$soiltemp) > 0 | length(s$soilVWC) > 0 | length(s$airtemp) > 0 | length(s$waterlevel) > 0 ) {
    
    .SD <- NULL
    
    # period of record over all sensors
    .POR <- function(i) {
      # date range
      start.date <- min(i$date_time, na.rm = TRUE)
      end.date <- max(i$date_time, na.rm = TRUE)
      # compute days since last visit
      dslv <- round(as.numeric(difftime(Sys.Date(), end.date, units = 'days')))
      
      res <- data.frame(start.date, end.date, dslv)
      return(res)
    }
    
    por <- as.data.table(na.omit(rbind(s$soiltemp, s$soilVWC, s$airtemp, s$waterlevel)))[, .POR(.SD), by = 'sid']
    por <- as.data.frame(por)
    
    
    # convert dates and add helper column
    s$soiltemp <- .formatDates(s$soiltemp, gran=gran, pad.missing.days=pad.missing.days)
    s$soilVWC <- .formatDates(s$soilVWC, gran=gran, pad.missing.days=pad.missing.days)
    s$airtemp <- .formatDates(s$airtemp, gran=gran, pad.missing.days=pad.missing.days)
    s$waterlevel <- .formatDates(s$waterlevel, gran=gran, pad.missing.days=pad.missing.days)
    
    # optionally compute summaries, requires padded NA values and, daily granularity
    if(soiltemp.summaries & pad.missing.days & (length(s$soiltemp) > 0)) {
      message('computing un-biased soil temperature summaries')
      
      if(gran != 'day')
        stop('soil temperature summaries can only be computed from daily data', call. = FALSE)
      
      # compute unbiased estimates of MAST and summer/winter temp
      soiltemp.summary <- summarizeSoilTemperature(s$soiltemp)
      
      # combine summaries and join to sensors data
      por <- merge(por, soiltemp.summary, by = 'sid', all.x = TRUE, sort = FALSE)
    }
    
    # splice-into sensors data by = 'sid', all.x = TRUE, sort = FALSE)
    s$sensors <- merge(s$sensors, por, by = 'sid', all.x = TRUE, sort = FALSE)
  }
  
  # copy over sensor name + depth to all sensor tables--if present
  # "name" is the sensor name - depth for plotting
  if(length(s$soiltemp) > 0) {
    name.idx <- match(s$soiltemp$sid, s$sensors$sid)
    s$soiltemp$name <- paste0(s$sensors$name[name.idx], '-', s$sensors$sensor_depth[name.idx])
    s$soiltemp$sensor_name <- s$sensors$name[name.idx]
    s$soiltemp$sensor_depth <- s$sensors$sensor_depth[name.idx]
  }
  
  if(length(s$soilVWC) > 0) {
    name.idx <- match(s$soilVWC$sid, s$sensors$sid)
    s$soilVWC$name <- paste0(s$sensors$name[name.idx], '-', s$sensors$sensor_depth[name.idx])
    s$soilVWC$sensor_name <- s$sensors$name[name.idx]
    s$soilVWC$sensor_depth <- s$sensors$sensor_depth[name.idx]
  }
  
  if(length(s$airtemp) > 0) {
    name.idx <- match(s$airtemp$sid, s$sensors$sid)
    s$airtemp$name <- paste0(s$sensors$name[name.idx], '-', s$sensors$sensor_depth[name.idx])
    s$airtemp$sensor_name <- s$sensors$name[name.idx]
    s$airtemp$sensor_depth <- s$sensors$sensor_depth[name.idx]
  }
  
  if(length(s$waterlevel) > 0) {
    name.idx <- match(s$waterlevel$sid, s$sensors$sid)
    s$waterlevel$name <- paste0(s$sensors$name[name.idx], '-', s$sensors$sensor_depth[name.idx])
    s$waterlevel$sensor_name <- s$sensors$name[name.idx]
    s$waterlevel$sensor_depth <- s$sensors$sensor_depth[name.idx]
  }
  
  # init coordinates
  if(!is.null(s$sensors)) {
    coordinates(s$sensors) <- ~ wgs84_longitude + wgs84_latitude
    proj4string(s$sensors) <- '+proj=longlat +datum=WGS84'
  }
  
  # reset options:
  options(opt.original)
  
  # some feedback via message:
  s.size <- round(object.size(s) / 1024 / 1024, 2)
  message(paste(nrow(s$sensors), ' sensors loaded (', s.size, ' Mb transferred)', sep=''))
  
  # done
  return(s)
  
}

