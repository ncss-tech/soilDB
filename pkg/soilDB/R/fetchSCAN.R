## TODO:
## 1. get a list of stations
## 2. get a list of reports and matching headers / units
## 3. better documentation / testing
## 4. work with Deb / programmers to get compressed output
fetchSCAN <- function(req) {
  # need this
  require(RCurl)
  
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
  
  # read as CSV, skipping junk + headers, accomodating white-space and NA values encoded as -99.9
  x <- read.table(tc, skip=3, header=FALSE, stringsAsFactors=FALSE, sep=',', quote='', strip.white=TRUE, na.strings='-99.9', comment.char='')
  
  # done
  close.connection(tc)
  return(x)
}



  



