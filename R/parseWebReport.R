
## parallel requests?
# https://cran.r-project.org/web/packages/curl/vignettes/intro.html#async_requests


## TODO: POST doesn't seem to work either ...

# library(httr)
# url <- 'https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=WEB-PROPERY-COMPONENT_property'
# r <- POST(url, body=args, encode = "multipart", verbose())
# rvest::html_table(content(r))


# requires rvest
# note: get argument names from report HTML source
#
# examples:
# url = 'https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=WEB-PROJECT_MUKEY_BY_GOAL_YEAR'
# args = list(msso='2-MIN', fy='2018', asym='%', proj='0')
parseWebReport <- function(url, args, index=1) {
  
  # sanity check: package requirements
  if(!requireNamespace('rvest'))
    stop('please install the package: rvest', call. = FALSE)
  
  # parse args and create final URL
  URLargs <- paste0('&', paste(names(args), unlist(args), sep='='), collapse='')
  url <- paste0(url, URLencode(URLargs, reserved = FALSE))
  
  # get HTML, result is NULL when HTTP ERROR is encountered
  # using curl package functions / low level options to account for slow servers
  
  ## TODO: random timeouts / SSL errors / evaluation errors
  # this happens when asking for many web reports in a row (5-10% of the time for 150+ calls)
  # solution: use a while() loop and n-iterations until there is no error, or i > n
  x <- tryCatch(xml2::read_html(curl::curl(url,  handle = curl::new_handle(verbose=FALSE, useragent = "Mozilla/5.0", CONNECTTIMEOUT = 60))), error = function(e){print(e); return(NULL)})
  
  # catch (likely) HTTP errors here
  if(is.null(x))
    return(NULL)
  
  # read all of the HTML tables
  d <- rvest::html_table(x, header=TRUE)
  
  # sanity check empty list = no data
  if(length(d) < 1)
    return(NULL)
  
  # if specified, get only the indexed table
  if(! is.null(index)) {
    d <- d[[index]]
  }
  
  # TODO: col names aren't legal data.frame names
  
  # done
  return(d)
}
