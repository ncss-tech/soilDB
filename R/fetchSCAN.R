## TODO:
## 1. get a list of stations
## 2. get a list of reports and matching headers / units
## 3. better documentation / testing
## 4. work with Deb / programmers to get compressed output
##
## see: http://www.wcc.nrcs.usda.gov/web_service/awdb_webservice_announcements.htm
##      http://www.wcc.nrcs.usda.gov/web_service/AWDB_Web_Service_Reference.htm
##      http://www.wcc.nrcs.usda.gov/report_generator/WebReportScripting.htm
#
### sensor codes: http://wcc.sc.egov.usda.gov/nwcc/sensors

### TODO: why are there sometimes duplicate above-ground sensors:
###   station 482
###
###  WTEQ.I WTEQ.I-2 PREC.I PREC.I-2 TOBS.I TOBS.I-2 TOBS.I-3 TMAX.D TMIN.D TAVG.D SNWD.I SMS.I_8 STO.I_8

##
## ideas:
##   https://github.com/gunnarleffler/getSnotel
##


## TODO: this crashes on 32bit R / libraries
# helper function for getting a single table of SCAN metadata
# site.code: a single SCAN site code
.get_single_SCAN_metadata <- function(site.code) {
  # base URL to service
  uri <- 'http://wcc.sc.egov.usda.gov/nwcc/sensors'
  
  # note: the SCAN form processor checks the refering page and user-agent
  new.headers <- c(
    "Referer"="http://www.wcc.nrcs.usda.gov/nwcc/",
    "User-Agent" = "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; en-US; rv:1.9.2.13) Gecko/20101203 Firefox/3.6.13"
  )
  
  # enable follow-location
  # http://stackoverflow.com/questions/25538957/suppressing-302-error-returned-by-httr-post
  # cf <- httr::config(followlocation = 1L, verbose=1L) # debugging
  cf <- httr::config(followlocation = 1L)
  
  req <- list(sitenum=site.code, report='ALL', interval='DAY', timeseries=" View Daily Sensor Descriptions ")
  
  # submit request
  r <- httr::POST(uri, body=req, encode='form', config = cf, httr::add_headers(new.headers))
  httr::stop_for_status(r)
  
  # parsed XML
  r.content <- httr::content(r, as='parsed')
  # get tables
  n.tables <- rvest::html_nodes(r.content, "table")
  
  ## TODO: this line crashes on 32bit R / libraries
  # the metadata table we want is 5th
  m <- rvest::html_table(n.tables[[5]], header=FALSE)
  
  # clean-up table
  # 1st row is header
  h <- make.names(m[1, ])
  # second row is junk
  m <- m[-c(1:2), ]
  names(m) <- h
  
  return(m)
}




# iterate over a vector of SCAN site codes, returning basic metadata
# site.code: vector of SCAN site codes
SCAN_sensor_metadata <- function(site.code) {
  
  # check for 64bit mode
  if(.Machine$sizeof.pointer != 8)
    stop("Sorry! For some reason this function crashes in 32bit mode, I don't know why!", call. = FALSE)
  
  # check for required packages
  if(!requireNamespace('httr', quietly = TRUE) | !requireNamespace('rvest', quietly = TRUE))
    stop('please install the `httr` and `rvest` packages', call.=FALSE)
  
  # iterate over site codes, returning DF + site.code
  res <- ddply(data.frame(site.code=site.code), 'site.code', .get_single_SCAN_metadata)
  
  return(res)
}


# site.code: vector of site codes
# year: vector of years
# report: single report type
# req: for backwards compatibility
fetchSCAN <- function(site.code, year, report='SCAN', req=NULL) {
  
  ## backwards compatibility:
  if(!missing(req)) {
    message('using old-style interface...')
    return(.get_SCAN_data(req))
  }
  
  
  # all possible combinations of site codes and year | single report type
  g <- expand.grid(s=site.code, y=year, r=report)
  
  # get a list of request lists
  req.list <- mapply(.make_SCAN_req, s=g$s, y=g$y, r=g$r, SIMPLIFY = FALSE)
  
  # format raw data into a list of lists:
  # sensor suite -> site number -> year
  d.list <- list()
  for(i in req.list) {
    # raw data is messy, reformat
    d <- .get_SCAN_data(i)
    
    ## TODO: sometimes the above ground sensors will match multiple (?) versions
    
    # save: sensor suite -> site number -> year
    sensors <- c('SMS', 'STO', 'SAL', 'TAVG', 'PRCP', 'PREC', 'SNWD', 'WTEQ', 'WDIRV', 'WSPDV', 'LRADT')
    for(sensor.i in sensors) {
      d.list[[sensor.i]][[as.character(i$sitenum)]][[as.character(i$year)]] <- .formatSCAN_soil_sensor_suites(d, code=sensor.i)
    }
    
  }
  
  # init list to store results
  res <- list()
  for(sensor.i in sensors) {
    # flatten individual sensors over years, by site number
    r.i <- ldply(llply(d.list[[sensor.i]], ldply))
    r.i$.id <- NULL
    res[[sensor.i]] <- r.i
  }
  
  return(res)
}




# combine soil sensor suites into stackable format
.formatSCAN_soil_sensor_suites <- function(d, code) {
  # locate named columns
  d.cols <- grep(code, names(d))
  # return NULL if no data
  if(length(d.cols) == 0)
    return(NULL)
  # convert to long format
  d.long <- melt(d, id.vars = c('Site', 'Date'), measure.vars = names(d)[d.cols])
  # extract depths
  d.depths <- base::strsplit(as.character(d.long$variable), split = '_', fixed = TRUE)
  d.long$depth <- sapply(d.depths, function(i) as.numeric(i[2]))
  # convert depths (in) to cm
  d.long$depth <- round(d.long$depth * 2.54)
  # format and return
  return(d.long[, c('Site', 'Date', 'value', 'depth')])
}

# format a list request for SCAN data
# s: single site code
# y: single year
# r: single report type
.make_SCAN_req <- function(s, y, r) {
  req <- list(intervalType=' View Historic ', report=r, timeseries='Daily', format='copy', sitenum=s, interval='YEAR', year=y, month='CY')
  return(req)
}


## this is an internally used function
# req is a named vector or list
.get_SCAN_data <- function(req) {
  
  # convert to list as needed
  if(class(list) != 'list')
    req <- as.list(req)
  
  # check for required packages
  if(!requireNamespace('httr', quietly = TRUE))
    stop('please install the `httr` package', call.=FALSE)
  
  # base URL to service
  uri <- 'http://wcc.sc.egov.usda.gov/nwcc/view'
  
  # note: the SCAN form processor checks the refering page and user-agent
  new.headers <- c(
    "Referer"="http://www.wcc.nrcs.usda.gov/nwcc/",
    "User-Agent" = "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; en-US; rv:1.9.2.13) Gecko/20101203 Firefox/3.6.13"
  )
  
  # enable follow-location
  # http://stackoverflow.com/questions/25538957/suppressing-302-error-returned-by-httr-post
  # cf <- httr::config(followlocation = 1L, verbose=1L) # debugging
  cf <- httr::config(followlocation = 1L)
  
  # submit request
  r <- httr::POST(uri, body=req, encode='form', config = cf, httr::add_headers(new.headers))
  httr::stop_for_status(r)
  
  # extract content as text, cannot be directly read-in
  r.content <- httr::content(r, as='text')
  
  # connect to the text as a standard file
  tc <- textConnection(r.content)
  
  # attempt to read column headers, after skipping the first two lines of data
  h <- unlist(read.table(tc, nrows=1, skip=2, header=FALSE, stringsAsFactors=FALSE, sep=',', quote='', strip.white=TRUE, na.strings='-99.9', comment.char=''))
  
  # the last header is junk (NA)
  h <- as.vector(na.omit(h))
  
  # split colunmn names on white space and keep the first element
  h <- sapply(strsplit(h, split=' '), function(i) i[[1]])
  
  # clean some more junk
  h <- gsub('-1', '', fixed=TRUE, h)
  h <- gsub(':-', '_', h)
  
  # NOTE: we have already read-in the first 3 lines above, therefore we don't need to skip lines here
  # read as CSV, skipping junk + headers, accomodating white-space and NA values encoded as -99.9
  x <- read.table(tc, header=FALSE, stringsAsFactors=FALSE, sep=',', quote='', strip.white=TRUE, na.strings='-99.9', comment.char='')
  
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





