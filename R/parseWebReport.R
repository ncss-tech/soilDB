## parallel requests?
# https://cran.r-project.org/web/packages/curl/vignettes/intro.html#async_requests

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

#' @title Parse contents of a web report, based on supplied arguments.
#' 
#' @description Parse contents of a web report, based on supplied arguments.
#' 
#' @details Report argument names can be inferred by inspection of the HTML source
#' associated with any given web report.
#' 
#' @param url Base URL to a LIMS/NASIS web report.
#' 
#' @param args List of named arguments to send to report, see details.
#' 
#' @param index Integer index specifying the table to return, or, NULL for a
#' list of tables
#' 
#' @return A `data.frame` object in the case of a single integer `index`, otherwise a `list`
#' 
#' @note Most web reports are for internal use only.
#' 
#' @author D.E. Beaudette and S.M. Roecker
#' 
#' @keywords IO
#' 
#' @export parseWebReport
#' 
parseWebReport <- function(url, args, index = 1) {
  
  # suggested packages
  if (!requireNamespace('rvest'))
    stop('please install the package: rvest', call. = FALSE)
  
  if (!requireNamespace('xml2'))
    stop('please install the package: xml2', call. = FALSE)
  
  if (!requireNamespace('curl'))
    stop('please install the package: curl', call. = FALSE)
  
  # parse args and create final URL
  args2 <- lapply(args, function(x) URLencode(as.character(x), reserved = TRUE))
  URLargs <- paste0('&', paste(names(args2), unlist(args2), sep = '='), collapse = '')
  url <- paste0(url, URLargs)
  
  # get HTML, result is NULL when HTTP ERROR is encountered
  # using curl package functions / low level options to account for slow servers
  
  ## TODO: random timeouts / SSL errors / evaluation errors
  # this happens when asking for many web reports in a row (5-10% of the time for 150+ calls)
  # solution: use a while() loop and n-iterations until there is no error, or i > n
  
  # TODO: evaluate whether we can achieve this without suggesting curl 
  x <- tryCatch(
    xml2::read_html(curl::curl(url, handle = curl::new_handle(verbose = FALSE,
                                                              useragent = "Mozilla/5.0",
                                                              CONNECTTIMEOUT = 60))), 
    error = function(e) {
      print(e)
      return(NULL)
    }
  )
  
  # catch (likely) HTTP errors here
  if (is.null(x))
    return(NULL)
  
  # read all of the HTML tables
  # result is a list
  d <- rvest::html_table(x, header = TRUE)
  
  # sanity check empty list = no data
  if (length(d) < 1)
    return(NULL)
  
  ## TODO: consider message when length(d) > length(index)
  
  # iterate over tables
  d <- lapply(d, function(i) {
    
    # replace blanks with NA, problem with LIMS (NASIS Web) reports
    idx <- unlist(lapply(i, is.character))
    if (any(idx)) {
      i[idx] <- lapply(i[idx], function(x) ifelse(x == "", NA, x))
    }
    
    # convert to DF
    # note: col names aren't legal data.frame names
    i <- as.data.frame(i)
    return(i)
  })
  
  
  # return requested tables via `index`
  # or all tables if NULL
  if(is.null(index)) {
    
    # result is a list
    return(d)
  } else {
   
     # single table -> data.frame
    if(length(index) == 1) {
      return(d[[index]])
    } else {
      # multiple tables -> list
      return(d[index])
    }
    
  }
  
}
