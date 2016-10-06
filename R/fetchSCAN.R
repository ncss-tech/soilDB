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


##
## ideas:
##   https://github.com/gunnarleffler/getSnotel
##


SCAN_sensor_metadata <- function(site.code) {
  # check for required packages
  if(!requireNamespace('httr', quietly = TRUE) | !requireNamespace('rvest', quietly = TRUE))
    stop('please install the `httr` and `rvest` packages', call.=FALSE)
  
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


# site.code: vector of site codes
# year: vector of years
# report: single report type
fetchSCAN <- function(site.code, year, report='SCAN') {
  
  # all possible combinations of site codes and year | single report type
  g <- expand.grid(s=site.code, y=year, r=report)
  
  # get a list of request lists
  req.list <- mapply(.make_SCAN_req, s=g$s, y=g$y, r=g$r, SIMPLIFY = FALSE)
  
  ## TODO: can probably be optimised, processing requires 2 passes
  # format raw data into a list of lists
  # indexed by:
  # sitenum -> year -> sensor cluster
  d.list <- list()
  for(i in req.list) {
    # raw data is messy, reformat
    d <- .get_SCAN_data(i)
    
    # temp list to store results
    l.data <- list()
    
    # current iteration data, parsed into components
    l.data[['SMS']] <- .formatSCAN(d, code='SMS')
    l.data[['STO']] <- .formatSCAN(d, code='STO')
    
    # current year's worth of data
    d.list[[as.character(i$sitenum)]][[as.character(i$year)]] <- l.data
  }
  
  # flatten individual sensors over years, by site number
  d.sms <- .flatten_SCAN_data(d.list, 'SMS')
  d.sto <- .flatten_SCAN_data(d.list, 'STO')
  
  return(list(sms=d.sms, sto=d.sto))
}


## TODO: this seems wastefull...
.flatten_SCAN_data <- function(datalist, code) {
  
  # extract single suite of data and flatten to DF across years
  res <- lapply(datalist, function(this.site) {
    r <- ldply(this.site, function(this.year) {
      this.year[[code]]
    })
    # strip .id
    r$.id <- NULL
    return(r)
  })
  
  # flatten by site number
  res <- ldply(res)
  # strip .id
  res$.id <- NULL
  
  return(res)
}

# first attempt at unifying formats
.formatSCAN <- function(d, code) {
  # locate named columns
  d.cols <- grep(code, names(d))
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





