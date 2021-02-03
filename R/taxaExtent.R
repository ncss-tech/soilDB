
#' @title Retrieve Soil Taxonomy Membership Grids
#' 
#' @description This function downloads a generalized representation of the geographic extent of any single taxa from the top 4 tiers of Soil Taxonomy. Data are provided by SoilWeb, ultimately sourced from from the current SSURGO snapshot. Data are returned as \code{raster} objects representing area proportion falling within 800m cells. Data are only available in CONUS and returned using an Albers Equal Area / NAD83 coordinate reference system.
#' 
#' @param x single taxa name, case-insensitive
#' 
#' @param level the taxonomic level within the top 4 tiers of Soil Taxonomy, one of \code{c('order', 'suborder', 'greatgroup', 'subgroup')}
#' 
#' @param timeout time that we are willing to wait for a response, in seconds
#' 
#' @return a \code{raster} object
#' 
#' @author D.E. Beaudette
#' 
#' @note This is a work in progress.
#' 
#' @examples 
#' \donttest{
#' 
#' if(requireNamespace("curl") &
#'    curl::has_internet()) {
#'   
#'   library(raster)
#'   
#'   # try a couple of different examples
#'   
#'   # soil order
#'   taxa <- 'vertisols'
#'   x <- taxaExtent(taxa, level = 'order')
#'   a <- raster::aggregate(x, fact = 5)
#'   
#'   # suborder
#'   taxa <- 'ustalfs'
#'   x <- taxaExtent(taxa, level = 'suborder')
#'   a <- raster::aggregate(x, fact = 5)
#'   
#'   # greatgroup
#'   taxa <- 'haplohumults'
#'   x <- taxaExtent(taxa, level = 'greatgroup')
#'   a <- raster::aggregate(x, fact = 5)
#'   
#'   # subgroup
#'   taxa <- 'Typic Haploxerepts'
#'   x <- taxaExtent(taxa, level = 'subgroup')
#'   a <- raster::aggregate(x, fact = 5)
#'   
#'   # quick evaluation of the result
#'   if(requireNamespace("rasterVis") & requireNamespace('viridis')) {
#'     rasterVis::levelplot(a, 
#'       margin = FALSE, scales = list(draw = FALSE), 
#'       col.regions = viridis::viridis, 
#'       main = names(a)
#'     )
#'   }
#'   
#'   # slippy map
#'   if(requireNamespace("mapview")) {
#'     mapview::mapview(a, col.regions = viridis::viridis, na.color = NA, use.layer.names = TRUE)
#'   }
#'   
#'   
#'   
#' }
#' 
#' }
#' 
taxaExtent <- function(x, level = c('order', 'suborder', 'greatgroup', 'subgroup'), timeout = 60) {
 
  level <- match.arg(level)
  
  # encode taxa name: spaces -> underscores
  x <- gsub(pattern=' ', replacement='_', x = tolower(x), fixed = TRUE)
  
  # convert taxa level to path
  subdir <- switch(
    level,
    order = 'taxorder',
    suborder = 'taxsuborder',
    greatgroup = 'taxgrtgroup',
    subgroup = 'taxsubgrp'
  )
  
  ## TODO: update load-balancer to expose this subdir
  # base URL to cached data
  u <- URLencode(
    sprintf(
      'https://soilmap2-1.lawr.ucdavis.edu/taxa-grid-cache/%s/%s.tif', 
      subdir, 
      x
    )
  )
  
  # init temp files
  tf <- tempfile(fileext='.tif')
  
  # safely download GeoTiff file
  # Mac / Linux: file automatically downloaded via binary transfer
  # Windows: must manually specify binary transfrer
  res <- tryCatch(
    suppressWarnings(
      download.file(url=u, destfile=tf, extra=c(timeout=timeout), quiet=TRUE, mode = 'wb')
    ),
    error = function(e) {e}
  )
  
  # trap errors
  if(inherits(res, 'error')){
    message('no data returned')
    return(NULL)
  }
 
  # load raster object into memory
  r <- raster(tf, verbose=FALSE)
  r <- readAll(r)
  # transfer layer name
  names(r) <- gsub(pattern='_', replacement=' ', x = x, fixed = TRUE)
  
  # cleanup
  unlink(tf)
  
  # possibly fix CRS here, likely needs to be re-defined on the server
  # https://github.com/ncss-tech/soilDB/issues/144
  
  # CONUS AEA
  return(r)
  
  return(res)
}

