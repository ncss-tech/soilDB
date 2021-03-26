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


#' Parse contents of a web report, based on supplied arguments.
#' 
#' Parse contents of a web report, based on supplied arguments.
#' 
#' Report argument names can be inferred by inspection of the HTML source
#' associated with any given web report.
#' 
#' @param url Base URL to a LIMS/NASIS web report.
#' @param args List of named arguments to send to report, see details.
#' @param index Integer index specifying the table to return, or, NULL for a
#' list of tables
#' @return A \code{data.frame} object in the case of a single integer passed to
#' \code{index}, a \code{list} object in the case of an integer vector or NULL
#' passed to \code{index}.
#' @note Most web reports are for internal use only.
#' @author D.E. Beaudette and S.M. Roecker
#' @keywords IO
#' @examples
#' 
#' \donttest{
#' # pending
#' }
#' 
#' @export parseWebReport
parseWebReport <- function(url, args, index=1) {
  
  # sanity check: package requirements
  if(!requireNamespace('rvest'))
    stop('please install the package: rvest', call. = FALSE)
  
  # parse args and create final URL
  args2 <- lapply(args, function(x) URLencode(as.character(x), reserved = TRUE))
  URLargs <- paste0('&', paste(names(args2), unlist(args2), sep = '='), collapse='')
  url <- paste0(url, URLargs)
  
  # get HTML, result is NULL when HTTP ERROR is encountered
  # using curl package functions / low level options to account for slow servers
  
  ## TODO: random timeouts / SSL errors / evaluation errors
  # this happens when asking for many web reports in a row (5-10% of the time for 150+ calls)
  # solution: use a while() loop and n-iterations until there is no error, or i > n
  x <- tryCatch(
    read_html(curl(url,  handle = new_handle(verbose=FALSE, useragent = "Mozilla/5.0", CONNECTTIMEOUT = 60)))
    , error = function(e) {
      print(e)
      return(NULL)
    }
  )
  
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
  
  # replace blanks with NA, problem with LIMS reports
  idx <- unlist(lapply(d, is.character))
  if (any(idx)) {
    d[idx] <- lapply(d[idx], function(x) ifelse(x == "", NA, x))
  }
  
  # note: col names aren't legal data.frame names
  
  # done
  return(d)
}
