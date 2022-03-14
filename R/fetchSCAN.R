## TODO:
## 1. get a list of stations
## 2. get a list of reports and matching headers / units
## 3. better documentation / testing
## 4. work with Deb / programmers to get compressed output
##
## see: http://www.wcc.nrcs.usda.gov/web_service/awdb_webservice_announcements.htm
##      http://www.wcc.nrcs.usda.gov/web_service/AWDB_Web_Service_Reference.htm
##      http://www.wcc.nrcs.usda.gov/report_generator/WebReportScripting.htm

## 5. we will need to address the potential for multiple sensor ID per type/depth
## examples in:
## https://github.com/ncss-tech/soilDB/issues/14


### sensor codes: http://wcc.sc.egov.usda.gov/nwcc/sensors

##
## ideas:
##   https://github.com/gunnarleffler/getSnotel
##

## site images:
## https://www.wcc.nrcs.usda.gov/siteimages/462.jpg
##
## site notes:
## https://wcc.sc.egov.usda.gov/nwcc/sitenotes?sitenum=462
##


## helper function for getting a single table of SCAN metadata
# site.code: a single SCAN site code
.get_single_SCAN_metadata <- function(site.code) {
  # base URL to service
  uri <- 'https://wcc.sc.egov.usda.gov/nwcc/sensors'

  # note: the SCAN form processor checks the refering page and user-agent
  new.headers <-  c("Referer" = "https://wcc.sc.egov.usda.gov/nwcc/sensors")

  # enable follow-location
  # http://stackoverflow.com/questions/25538957/suppressing-302-error-returned-by-httr-post
  # cf <- httr::config(followlocation = 1L, verbose=1L) # debugging
  cf <- httr::config(followlocation = 1L)

  req <- list(
    sitenum = site.code,
    report = 'ALL',
    interval = 'DAY',
    timeseries = " View Daily Sensor Descriptions "
  )

  # submit request
  r <- httr::POST(
    uri,
    body = req,
    encode = 'form',
    config = cf,
    httr::add_headers(new.headers)
  )
  httr::stop_for_status(r)

  # parsed XML
  r.content <- httr::content(r, as = 'parsed')

  # get tables
  n.tables <- rvest::html_nodes(r.content, "table")

  # the metadata table we want is the last one
  m <- rvest::html_table(n.tables[[length(n.tables)]], header=FALSE)

  # clean-up table
  # 1st row is header
  h <- make.names(m[1, ])
  # second row is junk
  m <- m[-c(1:2), ]
  names(m) <- h

  m$site.code <- site.code
  return(m)
}

# iterate over a vector of SCAN site codes, returning basic metadata
# site.code: vector of SCAN site codes
#' @rdname fetchSCAN
#' @export
SCAN_sensor_metadata <- function(site.code) {

  # check for required packages
  if(!requireNamespace('httr', quietly = TRUE) | !requireNamespace('rvest', quietly = TRUE))
    stop('please install the `httr` and `rvest` packages', call.=FALSE)

  # iterate over site codes, returning DF + site.code

  res <- do.call('rbind', lapply(site.code, .get_single_SCAN_metadata))

  return(as.data.frame(res))
}

## https://github.com/ncss-tech/soilDB/issues/61
# site.code: vector of SCAN site codes
#' @rdname fetchSCAN
#' @export
SCAN_site_metadata <- function(site.code = NULL) {

  # hack to please R CMD check
  SCAN_SNOTEL_metadata <- NULL

  # cached copy available in soilDB::SCAN_SNOTEL_metadata
  load(system.file("data/SCAN_SNOTEL_metadata.rda", package="soilDB")[1])

  if (is.null(site.code)) {
    idx <- 1:nrow(SCAN_SNOTEL_metadata)
  } else {
    idx <- which(SCAN_SNOTEL_metadata$Site %in% site.code)
  }

  # subset requested codes
  res <- SCAN_SNOTEL_metadata[idx, ]

  return(res)
}



#' @title Get daily climate data from USDA-NRCS SCAN (Soil Climate Analysis Network) Stations
#'
#' @description Query soil/climate data from USDA-NRCS SCAN Stations
#'
#' @details Possible above and below ground sensor types include: 'SMS' (soil moisture), 'STO' (soil temperature), 'SAL' (salinity), 'TAVG' (daily average air temperature), 'TMIN' (daily minimum air temperature), 'TMAX' (daily maximum air temperature), 'PRCP' (daily precipitation), 'PREC' (daily precipitation), 'SNWD' (snow depth), 'WTEQ' (snow water equivalent),'WDIRV' (wind direction), 'WSPDV' (wind speed), 'LRADT' (solar radiation/langley total).
#'
#'  - More on [SCAN sensors](https://www.nrcs.usda.gov/wps/portal/wcc/home/dataAccessHelp/faqs/scanSensors/)
#'  - More on [SNOTEL sensors](https://www.nrcs.usda.gov/wps/portal/wcc/home/dataAccessHelp/faqs/snotelSensors/)
#'
#' See the [SCAN and SNOTEL FAQ](https://www.nrcs.usda.gov/wps/portal/wcc/home/dataAccessHelp/faqs/) for answers to common questions about these data.
#'
#' See the [fetchSCAN tutorial](http://ncss-tech.github.io/AQP/soilDB/fetchSCAN-demo.html) for additional usage and visualization examples.
#'
#' @aliases fetchSCAN SCAN_sensor_metadata SCAN_site_metadata
#' @param site.code a vector of site codes. If `NULL` `SCAN_site_metadata()` returns metadata for all SCAN sites.
#' @param year a vector of years
#' @param report report name, single value only
#' @param ... additional arguments (not used)
#' @return a \code{data.frame} object; `NULL` on bad request.
#' @author D.E. Beaudette, A.G. Brown
#' @references https://www.nrcs.usda.gov/wps/portal/wcc/home/
#' @keywords manip
#' @examples
#'
#' \donttest{
#' if(requireNamespace("curl") &
#'     curl::has_internet()) {
#'
#'     # get data
#'     x <- fetchSCAN(site.code=c(356, 2072), year=c(2015, 2016))
#'     str(x)
#'
#'     # get sensor metadata
#'     m <- SCAN_sensor_metadata(site.code=c(356, 2072))
#'
#'     # get site metadata
#'     m <- SCAN_site_metadata(site.code=c(356, 2072))
#' }
#' }
#'
#' @export fetchSCAN
fetchSCAN <- function(site.code, year, report = 'SCAN', ...) {

  # check for required packages
  if(!requireNamespace('httr', quietly = TRUE))
    stop('please install the `httr` package', call.=FALSE)

  if(!requireNamespace('data.table', quietly = TRUE))
    stop('please install the `data.table` package', call.=FALSE)

  ## backwards compatibility
  l <- list(...)
  if (length(l) > 0) {
    if ("req" %in% names(l)) {
        .Deprecated(msg = "`req` argument is deprecated")
        return(.get_SCAN_data(req = l[["req"]]))
    }
  }

  # init list to store results
  res <- list()

  # add metadata from cached table in soilDB
  m <- SCAN_site_metadata(site.code)
  site.code <- m$Site

  # all possible combinations of site codes and year | single report type
  g <- expand.grid(s = site.code, y = year, r = report)

  # get a list of request lists
  req.list <- mapply(.make_SCAN_req, s = g$s, y = g$y, r = g$r, SIMPLIFY = FALSE)

  # format raw data into a list of lists:
  # sensor suite -> site number -> year
  d.list <- list()

  for(i in req.list) {

    # when there are no data, result is NULL
    d <- .get_SCAN_data(i)

    # handle timeouts or other bad requests
    if (is.null(d)) {
      return(NULL)
    }

    # save: sensor suite -> site number -> year
    sensors <- c('SMS', 'STO', 'SAL', 'TAVG', 'TMIN',
                 'TMAX', 'PRCP', 'PREC', 'SNWD', 'WTEQ',
                 'WDIRV', 'WSPDV', 'LRADT')

    for(sensor.i in sensors) {

      site.i <- as.character(i$sitenum)
      year.i <- as.character(i$year)

      if (is.null(d)) {
        res <- data.frame(Site = integer(0), Date = as.Date(NULL),
                          water_year = numeric(0), water_day = integer(0),
                          value = numeric(0), depth = numeric(0),
                          sensor.id = integer(0), row.names = integer(0))
      } else {
        res <- .formatSCAN_soil_sensor_suites(d, code = sensor.i)
      }

      d.list[[sensor.i]][[site.i]][[year.i]] <- res
    }
  }

  # iterate over sensors
  for (sensor.i in sensors) {

    # flatten individual sensors over years, by site number
    r.i <- data.table::rbindlist(lapply(d.list[[sensor.i]], data.table::rbindlist))
    rownames(r.i) <- NULL

    res[[sensor.i]] <- as.data.frame(r.i)
  }

  # report object size
  if (length(res) > 0) {

    res.size <- round(object.size(res) / 1024 / 1024, 2)
    res.rows <- sum(sapply(res, nrow), na.rm = TRUE)
    message(paste(res.rows, ' records (', res.size, ' Mb transferred)', sep = ''))

  } else message('query returned no data')

  res[['metadata']] <- m
  return(res)
}

# combine soil sensor suites into stackable format
.formatSCAN_soil_sensor_suites <- function(d, code) {

  value <- NULL

  # locate named columns
  d.cols <- grep(code, names(d))

  # return NULL if no data
  if(length(d.cols) == 0)
    return(NULL)

  ## https://github.com/ncss-tech/soilDB/issues/14
  ## there may be multiple above-ground sensors (takes the first)
  if(length(d.cols) > 1 & code %in% c('TAVG', 'TMIN', 'TMAX', 'PRCP', 'PREC',
                                      'SNWD', 'WTEQ', 'WDIRV', 'WSPDV', 'LRADT')) {
    message(paste0('multiple sensors per site [site ', d$Site[1], '] ',
                   paste0(names(d)[d.cols], collapse = ',')))
    # use only the first sensor
    d.cols <- d.cols[1]
  }

  # coerce all values to double (avoids data.table warnings)
  mvars <- names(d)[d.cols]
  d[mvars] <- lapply(d[mvars], as.double)

  # convert to long format
  d.long <- data.table::melt(
    data.table::as.data.table(d),
    id.vars = c('Site', 'Date'),
    measure.vars = mvars
  )

  # extract depths
  d.depths <- strsplit(as.character(d.long$variable), '_', fixed = TRUE)
  d.long$depth <- sapply(d.depths, function(i) as.numeric(i[2]))

  # convert depths (in to cm)
  d.long$depth <- round(d.long$depth * 2.54)

  # change 'variable' to 'sensor.id'
  names(d.long)[which(names(d.long) == 'variable')] <- 'sensor.id'

  ## there can be multiple sensors at below-ground label
  .SD <- NULL
  no.na <- NULL
  sensors.per.depth <- d.long[, list(no.na = sum(complete.cases(.SD))),
                              by = c('sensor.id', 'depth'),
                              .SDcols = c('sensor.id', 'depth', 'value')]

  most.data <- sensors.per.depth[, .SD[which.max(no.na)], by = 'depth']

  # check for multiple sensors per depth
  tab <- table(sensors.per.depth$depth) > 1
  if (any(tab)) {
    multiple.sensor.ids <- as.character(sensors.per.depth$sensor.id[which(sensors.per.depth$depth %in% names(tab))])
    message(paste0('multiple sensors per depth [site ', d$Site[1], '] ',
                   paste(multiple.sensor.ids, collapse = ', ')))
  }

  # multiple rows / day, remove NA in sensor values
  idx <- which(!is.na(d.long$value))
  d.long <- d.long[idx, ]

  # water year/day: October 1st -- September 30th
  w <- waterDayYear(d.long$Date)

  # row-order is preserved
  d.long$water_year <- w$wy
  d.long$water_day <- w$wd

  # format and return
  return(as.data.frame(d.long[, c('Site', 'Date', 'water_year', 'water_day',
                                  'value', 'depth', 'sensor.id')]))
}

# format a list request for SCAN data
# s: single site code
# y: single year
# r: single report type
.make_SCAN_req <- function(s, y, r) {
  req <- list(
    intervalType = ' View Historic ',
    report = r,
    timeseries = 'Daily',
    format = 'copy',
    sitenum = s,
    interval = 'YEAR',
    year = y,
    month = 'CY'
  )
  return(req)
}

# req is a named vector or list
.get_SCAN_data <- function(req) {

  # convert to list as needed
  if (!inherits(req, 'list'))
    req <- as.list(req)

  # base URL to service
  uri <- 'https://wcc.sc.egov.usda.gov/nwcc/view'

  # note: the SCAN form processor checks the referring page and user-agent
  new.headers <- c("Referer" = "https://wcc.sc.egov.usda.gov/nwcc/")

  # enable follow-location
  # http://stackoverflow.com/questions/25538957/suppressing-302-error-returned-by-httr-post
  # cf <- httr::config(followlocation = 1L, verbose=1L) # debugging
  cf <- httr::config(followlocation = 1L)

  # submit request
  r <- try(httr::POST(
    uri,
    body = req,
    encode = 'form',
    config = cf,
    httr::add_headers(new.headers)
  ))

  if (inherits(r, 'try-error'))
    return(NULL)

  res <- httr::stop_for_status(r)

  # extract content as text, cannot be directly read-in
  r.content <- httr::content(r, as='text')

  # connect to the text as a standard file
  tc <- textConnection(r.content)

  # attempt to read column headers, after skipping the first two lines of data
  # note: this moves the text connection cursor forward 3 lines
  # 2018-03-06 DEB: results have an extra line up top, now need to skip 3 lines
  h <- unlist(read.table(
      tc,
      nrows = 1,
      skip = 3,
      header = FALSE,
      stringsAsFactors = FALSE,
      sep = ',',
      quote = '',
      strip.white = TRUE,
      na.strings = '-99.9',
      comment.char = ''
    ))

  # the last header is junk (NA)
  h <- as.vector(na.omit(h))

  # split column names on white space and keep the first element
  h <- sapply(strsplit(h, split=' '), function(i) i[[1]])

  # clean some more junk
  h <- gsub('-1', '', fixed=TRUE, h)
  h <- gsub(':-', '_', h)

  # NOTE: we have already read-in the first 3 lines above, therefore we don't need to skip lines here
  # read as CSV, skipping junk + headers, accommodating white-space and NA values encoded as -99.9
  x <- try(read.table(
    tc,
    header = FALSE,
    stringsAsFactors = FALSE,
    sep = ',',
    quote = '',
    strip.white = TRUE,
    na.strings = '-99.9',
    comment.char = ''
  ), silent = TRUE)

  # catch errors
  if (inherits(x, 'try-error')) {
    close.connection(tc)

    message("Error [site ", req$sitenum, "]: ", attr(x, 'condition')[["message"]])

    x <- as.data.frame(matrix(ncol = 12, nrow = 0))
    return(x)
  }

  # the last column is always junk
  x[[names(x)[length(x)]]] <- NULL

  # apply truncated column names:
  names(x) <- h

  # clean-up connections
  close.connection(tc)

  # convert date to Date class
  x$Date <- as.Date(x$Date)

  # done
  return(x)
}





