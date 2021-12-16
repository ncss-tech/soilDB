
# handle a single ROSETTA API request
# x.chunk: single set of data to be processed, a data.frame
# vars: column names of those soil propertie passed to API
# v: model version
# conf: configuration
# include.sd: include bootstrapped standard deviation?
.ROSETTA_request <- function(x.chunk, vars, v, conf, include.sd = FALSE) {

  # save a copy of columns not used by API
  x.chunk.other <- x.chunk[, which(!names(x.chunk) %in% vars), drop = FALSE]

  # retain only those columns required by the API
  x.chunk <- x.chunk[, vars, drop = FALSE]

  ## TODO: may not need to convert to matrix, API will now accept a list of lists
  # x.chunk is a data.frame
  # convert to matrix for proper JSON encoding
  x.chunk.mat <- as.matrix(x.chunk)

  # API url: version / model code
  u <- sprintf("http://www.handbook60.org/api/v1/rosetta/%s", v)

  # submit request
  # note: JSON is composed at function eval time
  r <- httr::POST(
    url = u,
    body = list(X = x.chunk.mat),
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

  # a valid result will contain a list with the following:
  # "model_code" (results from automatic model selection, -1 means no prediction / error)
  # "rosetta_version" (1, 2, 3)
  # "stdev" (bootstrapped standard deviation)
  # "van_genuchten_params" (standard output)

  # extract VG parameters, may include NA
  vg <- as.data.frame(d[['van_genuchten_params']])
  # set names
  vg.names <- c('theta_r', 'theta_s', 'alpha', 'npar', 'ksat')
  names(vg) <- vg.names

  # add model code
  vg[['.rosetta.model']] <- factor(d[['model_codes']])

  # add ROSETTA version
  vg[['.rosetta.version']] <- d[['rosetta_version']]
  
  # optionally return SD
  if(include.sd) {
    vg.sd <- as.data.frame(d[['stdev']])
    names(vg.sd) <- sprintf("sd_%s", vg.names)
    
    # original "extra" data + relevant properties + vg parameters + SD
    vg.data <- cbind(x.chunk.other, x.chunk, vg, vg.sd)
  } else {
    # original "extra" data + relevant properties + vg parameters
    vg.data <- cbind(x.chunk.other, x.chunk, vg)
  }
  
  
  return(vg.data)
}




#' @title Query USDA-ARS ROSETTA Model API
#'
#' @description A simple interface to the \href{https://www.ars.usda.gov/pacific-west-area/riverside-ca/agricultural-water-efficiency-and-salinity-research-unit/docs/model/rosetta-model/}{ROSETTA model} for predicting hydraulic parameters from soil properties. The ROSETTA API was developed by Dr. Todd Skaggs (USDA-ARS) and links to the work of Zhang and Schaap, (2017). See the \href{http://ncss-tech.github.io/AQP/soilDB/ROSETTA-API.html}{related tutorial} for additional examples.
#'
#' @author D.E. Beaudette, Todd Skaggs (ARS), Richard Reid
#'
#' @param x a \code{data.frame} of required soil properties, may contain other columns, see details
#'
#' @param vars character vector of column names in \code{x} containing relevant soil property values, see details
#'
#' @param v ROSETTA model version number: '1', '2', or '3', see details and references.
#' 
#' @param include.sd logical, include bootstrap standard deviation for estimated parameters
#'
#' @param chunkSize number of records per API call
#'
#' @param conf configuration passed to \code{httr::POST()} such as \code{verbose()}.
#'
#' @details Soil properties supplied in \code{x} must be described, in order, via \code{vars} argument. The API does not use the names but column ordering must follow: sand, silt, clay, bulk density, volumetric water content at 33kPa (1/3 bar), and volumetric water content at 1500kPa (15 bar).
#'
#' The ROSETTA model relies on a minimum of 3 soil properties, with increasing (expected) accuracy as additional properties are included:
#'  - required, sand, silt, clay: USDA soil texture separates (percentages) that sum to 100\%
#'  - optional, bulk density (any moisture basis): mass per volume after accounting for >2mm fragments, units of gm/cm3
#'  - optional, volumetric water content at 33 kPa: roughly "field capacity" for most soils, units of cm^3/cm^3
#'  - optional, volumetric water content at 1500 kPa: roughly "permanent wilting point" for most plants, units of cm^3/cm^3
#'  
#' The Rosetta pedotransfer function predicts five parameters for the van Genuchten model of unsaturated soil hydraulic properties
#' 
#'  - theta_r : residual volumetric water content
#'  - theta_s : saturated volumetric water content
#'  - log10(alpha) : retention shape parameter `[log10(1/cm)]`
#'  - log10(npar) : retention shape parameter
#'  - log10(ksat) : saturated hydraulic conductivity `[log10(cm/d)]`
#' 
#' Column names not specified in \code{vars} are retained in the output.
#'
#' Three versions of the ROSETTA model are available, selected using \code{v = 1}, \code{v = 2}, or \code{v = 3}.
#'
#'  - version 1 - Schaap, M.G., F.J. Leij, and M.Th. van Genuchten. 2001. ROSETTA: a computer program for estimating soil hydraulic parameters with hierarchical pedotransfer functions. Journal of Hydrology 251(3-4): 163-176. doi: \doi{10.1016/S0022-1694(01)00466-8}.
#'
#'  - version 2 - Schaap, M.G., A. Nemes, and M.T. van Genuchten. 2004. Comparison of Models for Indirect Estimation of Water Retention and Available Water in Surface Soils. Vadose Zone Journal 3(4): 1455-1463. doi: \doi{10.2136/vzj2004.1455}.
#'
#'  - version 3 - Zhang, Y., and M.G. Schaap. 2017. Weighted recalibration of the Rosetta pedotransfer model with improved estimates of hydraulic parameter distributions and summary statistics (Rosetta3). Journal of Hydrology 547: 39-53. doi: \doi{10.1016/j.jhydrol.2017.01.004}.
#'
#' @references
#' Consider using the interactive version, with copy/paste functionality at: \url{https://www.handbook60.org/rosetta}.
#'
#' Rosetta Model Home Page: \url{https://www.ars.usda.gov/pacific-west-area/riverside-ca/agricultural-water-efficiency-and-salinity-research-unit/docs/model/rosetta-model/}.
#'
#' Python ROSETTA model: \url{http://www.u.arizona.edu/~ygzhang/download.html}.
#'
#' Yonggen Zhang, Marcel G. Schaap. 2017. Weighted recalibration of the Rosetta pedotransfer model with improved estimates of hydraulic parameter distributions and summary statistics (Rosetta3). Journal of Hydrology. 547: 39-53. \doi{10.1016/j.jhydrol.2017.01.004}.
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
#' Schaap, M.G., F.J. Leij, and M.Th. van Genuchten. 2001. ROSETTA: a computer program for estimating soil hydraulic parameters with hierarchical pedotransfer functions. Journal of Hydrology 251(3-4): 163-176. doi: \doi{10.1016/S0022-1694(01)00466-8}.
#'
#' Schaap, M.G., A. Nemes, and M.T. van Genuchten. 2004. Comparison of Models for Indirect Estimation of Water Retention and Available Water in Surface Soils. Vadose Zone Journal 3(4): 1455-1463. doi: \doi{10.2136/vzj2004.1455}.
#'
#' Zhang, Y., and M.G. Schaap. 2017. Weighted recalibration of the Rosetta pedotransfer model with improved estimates of hydraulic parameter distributions and summary statistics (Rosetta3). Journal of Hydrology 547: 39-53. doi: \doi{10.1016/j.jhydrol.2017.01.004}.
#'
#'

# 2020-11-19:
# * NA are handled safely by the API now
# * model selection is automatic when model code = 0
# * work with Todd to determin the optimal request / record count trade-off

# 2021-01-06
# * best model (0) is always used, API no longer accepts `model` as a parameter
# * versions 1,2,3 supported

ROSETTA <- function(x, vars, v = c('1', '2', '3'), include.sd = FALSE, chunkSize = 10000, conf = NULL) {

  # check for required packages
  if (!requireNamespace('httr', quietly = TRUE) | !requireNamespace('jsonlite', quietly = TRUE))
    stop('please install the `httr` and `jsonlite` packages', call. = FALSE)

  # ROSETTA version check
  v <- match.arg(v)

  if( ! inherits(x, c('data.frame')) ) {
    stop('x must be a data.frame')
  } else {
    # support for data.table (or other) by casting to data.frame for all data.frame subclasses
    x <- as.data.frame(x)
  }

  # if it inherits from data.frame, nrow is defined
  if( ! nrow(x) > 0 ) {
    stop('x must contain more than 0 rows')
  }

  # check that vars exist in x
  if(! all(vars %in% names(x))) {
    stop('vars must match columns in x')
  }

  # soil properties must be numeric
  # Note: not data.table safe
  if(! all(sapply(x[, vars], is.numeric)) ){
    stop('x must contain only numeric values')
  }

  # chunk
  x[['.chunk']] <- makeChunks(1:nrow(x), size = chunkSize)

  # split
  x <- split(x, x[['.chunk']])

  # iterate over chunks
  # results may contain try-errors
  res <- lapply(x, FUN = .ROSETTA_request, vars = vars, v = v, conf = conf, include.sd = include.sd)

  ## TODO: think about error handling...
  # most likely a curl time-out
  # data are pre-screened before submitting to the API
  # locate errors
  err.idx <- which(sapply(res, inherits, 'try-error'))

  # remove errors
  if(length(err.idx) > 0) {
    res <- res[-err.idx]
  }

  # all errors, return NULL
  if(length(res) < 1) {
    message('empty result set')
    return(NULL)
  }

  # stack into DF
  res <- do.call('rbind', res)
  row.names(res) <- as.character(1:nrow(res))

  # remove chunkID
  res[['.chunk']] <- NULL

  return(res)
}
