#' @title ROSETTA Model Versions 1 and 3 API
#' @description A simple interface to the ROSETTA model for predicting hydraulic parameters from soil properties. Access is via API and still a work in progress.
#' 
#' @param x a \code{data.frame} or \code{matrix} of soil properties, may not contain NA, see details
#' @param v single character of '1' or '3', this is the model version number
#' @param conf configuration passed to code{httr::POST()}.
#' 
#' @details ...
#' 
#' 
#' @return a \code{data.frame} object with estimated water retention curve parameters and saturated hydraulic conductivity:
#' 
#' \describe{
#' 
#'  \item{theta_r: }{residual volumetric water content}
#'  \item{theta_s: }{saturated volumetric water content}
#'  \item{alpha:}{related to the inverse of the air entry suction, log10-tranformed values with units of cm}
#'  \item{npar: }{index of pore size distribution, log10-tranformed values with units of 1/cm}
#'  \item{ksat: }{saturated hydraulic conductivity with units of cm/day}
#'  
#' }
#' 


ROSETTA <- function(x, v = c('1', '3'), conf = NULL) {
  
  # check for required packages
  if (!requireNamespace('httr', quietly = TRUE) | !requireNamespace('jsonlite', quietly = TRUE))
    stop('please install the `httr` and `jsonlite` packages', call. = FALSE)
  
  # argument check
  v <- match.arg(v)
  
  if( ! inherits(x, c('data.frame', 'matrix')) ) {
    stop('x must be a data.frame or matrix')
  }
  
  
  # convert x to a matrix as needed
  if( inherits(x, 'data.frame') ) {
    x <- as.matrix(x)
  }
  
  # must be numeric
  if( ! is.numeric(x) ) {
    stop('x must contain only numeric values')
  }
  
  # for now, no NA allowed
  if(any(is.na(x))) {
    stop('x may not contain NA')
  }
  
  # generate model code
  m <- ncol(x) - 1
  
  # API url: version / model code
  u <- sprintf("http://www.handbook60.org/api/v1/rosetta/%s/model/%s", v, m)
  
  # submit request
  # note:
  r <- httr::POST(
    url = u,
    body = list(X = x),
    encode = "json", 
    config = conf
  )
  
  
  # trap errors
  request.status <- try(httr::stop_for_status(r), silent = TRUE)
  
  # return the error object so calling function/user can handle it
  if (inherits(request.status, 'try-error'))
    return(request.status)
  
  # the result is JSON
  r.content <- try(httr::content(r, as = 'text', encoding = 'UTF-8'), silent = TRUE)
  
  # error trapping
  if (inherits(r.content,'try-error'))
    return(r.content)
  
  # convert JSON -> list(van_genuchten_params = [numeric matrix])
  # note that NA / errors will result in 'NaN'
  # TODO: as far as I can tell there is no way to interpret 'Nan' as NA
  # TODO: asking Todd to change NaN -> null, that is correctly interpreted as NA
  d <- try(jsonlite::fromJSON(r.content))
  
  # error trapping
  if (inherits(d, 'try-error'))
    return(d)
  
  # extract first list element and convert to DF
  d <- as.data.frame(d[[1]])
  # names
  names(d) <- c('theta_r', 'theta_s', 'alpha', 'npar', 'ksat')
  
  # convert to familiar representation: alpha and npar are log10-transformed
  d$alpha <- log(d$alpha, base = 10)
  d$npar <- log(d$npar, base = 10)
  
  return(d)
}
