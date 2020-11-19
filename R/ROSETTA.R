
# handle a single ROSETTA API request
# x.chunk: single set of data to be processed, a data.frame
# vars: column names of those soil propertie passed to API
# v: model version
# m: model type
# conf: configuration
.ROSETTA_request <- function(x.chunk, vars, v, m, conf) {
  
  # save a copy of columns not used by API
  x.chunk.other <- x.chunk[, which(!names(x.chunk) %in% vars), drop = FALSE]
  
  # retain only those columns required by the API
  x.chunk <- x.chunk[, vars, drop = FALSE]
  
  # x.chunk is a data.frame
  # convert to matrix for proper JSON encoding
  x.chunk <- as.matrix(x.chunk)
  
  # API url: version / model code
  u <- sprintf("http://www.handbook60.org/api/v1/rosetta/%s/model/%s", v, m)
  
  # submit request
  # note: JSON is composed at function eval time
  r <- httr::POST(
    url = u,
    body = list(X = x.chunk),
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
  
  # a valid result will containt a list with the following:
  # "best_codes"
  # "model_code"
  # "rosetta_version"
  # "van_genuchten_params"
  
  # extract VG parameters, may include NA
  vg <- as.data.frame(d[['van_genuchten_params']])
  # set names
  vg.names <- c('theta_r', 'theta_s', 'alpha', 'npar', 'ksat')
  names(vg) <- vg.names
  
  # add model code
  # if m = 0, then copy from the 'best_codes' result element
  # otherwise, if m is manually specified, use `m` argument
  if(m == '0') {
    vg[['.rosetta.model']] <- factor(d[['best_codes']])
  } else {
    vg[['.rosetta.model']] <- factor(d[['model_code']])
  }
  
  
  # add model version
  vg[['.rosetta.version']] <- d[['rosetta_version']]
  
  # combine with original data
  vg.data <- cbind(x.chunk.other, vg)
  
  return(vg.data)
}




#' @title ROSETTA Model Versions 1 and 3 API
#' 
#' @description A simple interface to the \href{https://www.ars.usda.gov/pacific-west-area/riverside-ca/agricultural-water-efficiency-and-salinity-research-unit/docs/model/rosetta-model/}{ROSETTA model} for predicting hydraulic parameters from soil properties. The ROSETTA API was developed by Dr. Todd Skaggs (USDA-ARS) and links to the work of Zhang and Schaap, (2017). See the \href{http://ncss-tech.github.io/AQP/soilDB/ROSETTA-API.html}{related tutorial} for additional examples.
#'  

#' @param x a \code{data.frame} of required soil properties, may contain other columns, see details
#' 
#' @param vars character vector of column names in \code{x} containing relevant soil property values, see details
#' 
#' @param v ROSETTA model version number: '1' or '3'
#' 
#' @param m ROSETTA model type: '0' (automatic, best model selection), '2' (sand, silt, clay), '3' (sand, silt, clay, Db), '4' (sand, silt, clay, Db, 33kPa WT), or '5' (sand, silt, clay, Db, 33kPa WT, 1500kPa WT). Model type '0' is usually the best approach as the most appropriate model (e.g. in light of potential missing values) is selected by the API.
#' 
#' @param chunkSize number of records per API call
#' 
#' @param conf configuration passed to \code{httr::POST()} such as \code{verbose()}.
#' 
#' @details 
#' 
#' Soil properties supplied in \code{x} must be described, in order, via \code{vars} argument. The API does not use the names but column ordering must follow: sand, silt, clay, bulk density, volumetric water content at 33kPa (1/3 bar), and volumetric water content at 1500kPa (15 bar). 
#' 
#' The ROSETTA model relies on a minimum of 3 soil properties, with increasing (expected) accuracy as additional properties are included:
#'  \itemize{
#'    \item{required, sand, silt, clay: }{USDA soil texture separates (percentages) that sum to 100\%}
#'    \item{optional, bulk density (any moisture basis): }{mass per volume after accounting for >2mm fragments, units of gm/cm3}
#'    \item{optional, volumetric water content at 33 kPa: }{roughly "field capacity" for most soils, units of cm^3/cm^3}
#'    \item{optional, volumetric water content at 1500 kPa: }{roughly "permanent wilting point" for most plants, units of cm^3/cm^3}
#'  }
#'  
#'  Column names not specified in \code{vars} are retained in the output.
#' 
#' @note Input data should not contain columns names that will conflict with the ROSETTA API results: `theta_r`, `theta_s`, `alpha`, `npar`, `ksat`.
#' 
#' @return a \code{data.frame} object:
#' 
#' \describe{
#' 
#'  \item{... }{other columns present in \code{x}}
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

# 2020-11-19: 
# * NA are handled safely by the API now
# * model selection is automatic when model code = 0



ROSETTA <- function(x, vars, v = c('1', '3'), m = '0', chunkSize = 10000, conf = NULL) {
  
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
  
  # soil properties must be numeric
  if(! all(sapply(x[, vars], is.numeric)) ){
    stop('x must contain only numeric values') 
  }
  
  # chunk
  x[['.chunk']] <- makeChunks(1:nrow(x), size = chunkSize)
  
  # split
  x <- split(x, x[['.chunk']])
  
  # iterate over chunks
  # results may contain try-errors
  res <- lapply(x, FUN = .ROSETTA_request, vars = vars, v = v, m = m, conf = conf)
  
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
  
  ## 2020-11-19: this is no longer neccessary
  # # empty DF to store padded results
  # d.full <- data.frame(trash = 1:nrow(x.orig), stringsAsFactors = FALSE)
  # 
  # # fill with returned data while padding NA
  # for(i in return.vars) {
  #   d.full[[i]] <- NA
  #   d.full[[i]][complete.idx] <- d[[i]]
  # }
  # 
  # # remove trash
  # d.full$trash <- NULL
  # 
  # # combine with original data
  # res <- cbind(x.orig, d.full)
  
  return(res)
}
