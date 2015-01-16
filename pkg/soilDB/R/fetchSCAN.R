## TODO:
## 1. get a list of stations
## 2. get a list of reports and matching headers / units
## 3. better documentation / testing
## 4. work with Deb / programmers to get compressed output
fetchSCAN <- function(req) {
  # base URL to service
  uri <- 'http://www.wcc.nrcs.usda.gov/nwcc/view'
  
  # set CURL options here
  o <- curlOptions(
    referer="http://www.wcc.nrcs.usda.gov/nwcc/",
    verbose = FALSE, 
    followLocation = TRUE,
    useragent = "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; en-US; rv:1.9.2.13) Gecko/20101203 Firefox/3.6.13"
  )
  
  # post form, result is a pile of text
  res <- postForm(uri, .params=req, .opts=o, style="POST")
  
  # connect to the text as a standard file
  tc <- textConnection(res)
  
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



  



