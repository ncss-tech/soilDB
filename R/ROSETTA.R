# TODO: 
# * generalize interface so that data with varying levels of detail (3, 4, 5, 6 properties) can be split 
#   into chunks and submitted as separate requests (e.g. different models)
#   then merged back together
#   this is important for those cases where propertites beyond SSC are _sometimes_ available
#
# TODO: add chunk parameter to balance CPU time vs number of requests, see fetchSDA_spatial.R
#



#' @title ROSETTA Model Versions 1 and 3 API
#' @description A simple interface to the \href{https://www.ars.usda.gov/pacific-west-area/riverside-ca/agricultural-water-efficiency-and-salinity-research-unit/docs/model/rosetta-model/}{ROSETTA model} for predicting hydraulic parameters from soil properties. The ROSETTA API was developed by Dr. Todd Skaggs (USDA-ARS) and links to the work of Zhang and Schaap, (2017).
#' 
#' @param x a \code{data.frame} of required soil properties, may contain other columns, see details
#' @param vars character vector of soil properties in \code{x} containing relevant soil property values, see details
#' @param v single character of '1' or '3', this is the model version number
#' @param conf configuration passed to \code{httr::POST()}.
#' 
#' @details 
#' 
#' Soil properties supplied in \code{x} must be described, in order, via \code{vars} argument. The column names are irrelevant but must follow the ordering: sand, silt, clay, bulk density, volumetric water content at 33kPa, and volumetric water content at 1500kPa. 
#' 
#' The ROSETTA model relies on a minimum of 3 soil properties, with increasing (expected) accuracy as additional properties are included:
#'  \itemize{
#'    \item{required, sand, silt, clay: }{USDA soil texture separates (percentages) that sum to 100\%}
#'    \item{optional, bulk density (any moisture basis): }{mass per volume after accounting for >2mm fragments, units of gm/cm3}
#'    \item{optional, volumetric water content at 33 kPa: }{roughly "field capacity" for most soils, units of cm^3/cm^3}
#'    \item{optional, volumetric water content at 1500 kPa: }{roughly "permanent wilting point" for most plants, units of cm^3/cm^3}
#'  }
#' 
#' 
#' 
#' @return a \code{data.frame} object with estimated water retention curve parameters and saturated hydraulic conductivity:
#' 
#' \describe{
#' 
#'  \item{... }{all columns present in \code{x}}
#' 
#'  \item{theta_r: }{residual volumetric water content (cm^3/cm^3)}
#'  \item{theta_s: }{saturated volumetric water content (cm^3/cm^3)}
#'  \item{alpha:}{related to the inverse of the air entry suction, log10-tranformed values with units of cm}
#'  \item{npar: }{index of pore size distribution, log10-tranformed values with units of 1/cm}
#'  \item{ksat: }{saturated hydraulic conductivity, log10-transformed values with units of cm/day}
#'  
#' }
#' 
#' 
#' 
#' @references 
#' Consider using the interactive version, with copy/paste functionality at: \url{https://www.handbook60.org/rosetta}.
#' 
#' Rosetta Model Home Page: \url{https://www.ars.usda.gov/pacific-west-area/riverside-ca/agricultural-water-efficiency-and-salinity-research-unit/docs/model/rosetta-model/}.
#' 
#' Python ROSETTA model: \url{http://www.u.arizona.edu/~ygzhang/download.html}.
#' 
#' Yonggen Zhang, Marcel G. Schaap. 2017. Weighted recalibration of the Rosetta pedotransfer model with improved estimates of hydraulic parameter distributions and summary statistics (Rosetta3). Journal of Hydrology. 547: 39-53. \url{https://doi.org/10.1016/j.jhydrol.2017.01.004}.
#' 
#' Kosugi, K. 1999. General model for unsaturated hydraulic conductivity for soils with lognormal pore-size distribution. Soil Sci. Soc. Am. J. 63:270-277.
#' 
#' Mualem, Y. 1976. A new model predicting the hydraulic conductivity of unsaturated porous media. Water Resour. Res. 12:513-522.
#' 
#' Schaap, M.G. and W. Bouten. 1996. Modeling water retention curves of sandy soils using neural networks. Water Resour. Res. 32:3033-3040.
#' 
#' Schaap, M.G., Leij F.J. and van Genuchten M.Th. 1998. Neural network analysis for hierarchical prediction of soil water retention and saturated hydraulic conductivity. Soil Sci. Soc. Am. J. 62:847-855.
#' 
#' Schaap, M.G., and F.J. Leij, 1998. Database Related Accuracy and Uncertainty of Pedotransfer Functions, Soil Science 163:765-779.
#' 
#' Schaap, M.G., F.J. Leij and M. Th. van Genuchten. 1999. A bootstrap-neural network approach to predict soil hydraulic parameters. In: van Genuchten, M.Th., F.J. Leij, and L. Wu (eds), Proc. Int. Workshop, Characterization and Measurements of the Hydraulic Properties of Unsaturated Porous Media, pp 1237-1250, University of California, Riverside, CA.
#' 
#' Schaap, M.G., F.J. Leij, 1999, Improved prediction of unsaturated hydraulic conductivity with the Mualem-van Genuchten, Submitted to Soil Sci. Soc. Am. J.
#' 
#' van Genuchten, M.Th. 1980. A closed-form equation for predicting the hydraulic conductivity of unsaturated soils. Soil Sci. Am. J. 44:892-898.
#' 
#' 
#' 
#' 




ROSETTA <- function(x, vars, v = c('1', '3'), conf = NULL) {
  
  # check for required packages
  if (!requireNamespace('httr', quietly = TRUE) | !requireNamespace('jsonlite', quietly = TRUE))
    stop('please install the `httr` and `jsonlite` packages', call. = FALSE)
  
  # argument check
  v <- match.arg(v)
  
  if( ! inherits(x, c('data.frame')) ) {
    stop('x must be a data.frame')
  }
  
  # check that vars exist in x
  if(! all(vars %in% names(x))) {
    stop('vars must match columns in x')
  }
  
  # split data from IDs if present
  x.orig <- x
  x <- x[, vars, drop = FALSE]
  
  # 2020-11-03: all NA causes Internal Server Error (HTTP 500)
  # keep track of NA positions for later
  complete.idx <- which(complete.cases(x))
  
  # filter NA
  x <- x[complete.idx, ]
  
  # convert x to a matrix
  x <- as.matrix(x)
  
  # must be numeric
  if( ! is.numeric(x) ) {
    stop('x must contain only numeric values')
  }
  
  # generate model code
  m <- ncol(x) - 1
  
  # API url: version / model code
  u <- sprintf("http://www.handbook60.org/api/v1/rosetta/%s/model/%s", v, m)
  
  # submit request
  # note: JSON is composed at function eval time
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
  # note that NA / errors will result in 'null' -> translated to NA by fromJSON()
  d <- try(jsonlite::fromJSON(r.content))
  
  # error trapping
  if (inherits(d, 'try-error'))
    return(d)
  
  # extract first list element and convert to DF
  d <- as.data.frame(d[[1]])
  # names
  return.vars <- c('theta_r', 'theta_s', 'alpha', 'npar', 'ksat')
  names(d) <- return.vars 
  
  # empty DF to store padded results
  d.full <- data.frame(trash = 1:nrow(x.orig), stringsAsFactors = FALSE)
  
  # fill with returned data while padding NA
  for(i in return.vars) {
    d.full[[i]] <- NA
    d.full[[i]][complete.idx] <- d[[i]]
  }
  
  # remove trash
  d.full$trash <- NULL
  
  # combine with original data
  res <- cbind(x.orig, d.full)
  
  return(res)
}
