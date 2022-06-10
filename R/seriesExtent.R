#' Retrieve Soil Series Extent Maps from SoilWeb
#' @description This function downloads a generalized representations of a soil series extent from SoilWeb, derived from the current SSURGO snapshot. Data can be returned as vector outlines (\code{sf} object) or gridded representation of area proportion falling within 800m cells (\code{SpatRaster} object). Gridded series extent data are only available in CONUS. Vector representations are returned with a GCS/WGS84 coordinate reference system and raster representations are returned with an Albers Equal Area / NAD83 coordinate reference system (`EPSG:5070`).
#' @param s a soil series name, case-insensitive
#' @param type series extent representation, \code{vector} results in a \code{SpatialPolygonsDataFrame} object and \code{raster} results in a \code{raster} object
#' @param timeout time that we are willing to wait for a response, in seconds
#' @references \url{https://casoilresource.lawr.ucdavis.edu/see/}
#' @author D.E. Beaudette
#' @export
#' @examples
#' \donttest{
#' if(requireNamespace("curl") &
#'    requireNamespace("sf") &
#'    requireNamespace("terra") &
#'    curl::has_internet()) {
#'   
#'   # required packages
#'   library(sf)
#'   library(terra)
#'   
#'   # specify a soil series name
#'   s <- 'magnor'
#'   
#'   # return an sf object
#'   x <- seriesExtent(s, type = 'vector')
#'   
#'   # return a terra SpatRasters
#'   y <- seriesExtent(s, type = 'raster')
#'   
#'   if (!is.null(x) && !is.null(y)) {
#'     # note that CRS are different
#'     sf::st_crs(x)
#'     terra::crs(y)
#'   
#'     # transform vector representation to CRS of raster
#'     x <- sf::st_transform(x, terra::crs(y))
#'   
#'     # graphical comparison
#'     par(mar = c(1, 1 , 1, 3))
#'     plot(y, axes = FALSE)
#'     
#'     # no fill color
#'     plot(x['series'], add = TRUE, col = NA)
#'   }
#'   
#' }
#' }
#' 
seriesExtent <- function(s, type = c('vector', 'raster'), timeout = 60) {

  type <- match.arg(type)
  
  # encode series name: spaces -> underscores
  s <- gsub(pattern = ' ', replacement = '_', x = tolower(s), fixed = TRUE)
  
  # select type of output
  res <- switch(
    type,
    vector = {.vector_extent(s, timeout = timeout)},
    raster = {.raster_extent(s, timeout = timeout)}
  )
  
  return(res)
}

.vector_extent <- function(s, timeout) {
  
  if (!requireNamespace("sf")) 
    stop("package sf is required to return vector series extent grids", call. = FALSE)
  
  # base URL to cached data
  u <- URLencode(paste0('https://casoilresource.lawr.ucdavis.edu/series-extent-cache/json/', s, '.json'))
  
  # init temp files
  tf <- tempfile(fileext = '.json')
  
  # safely download GeoJSON file
  res <- tryCatch(
    suppressWarnings(
      download.file(url = u, destfile = tf, extra = c(timeout = timeout), quiet = TRUE)
    ),
    error = function(e) {e}
  )

  # trap errors
  if (inherits(res, 'error')) {
    message('no data returned')
    return(NULL)
  }
    
  # load into sf object and clean-up
  # can use terra::vect() also
  x <- sf::st_read(tf, quiet = TRUE)
  unlink(tf)
  
  # reset row names in attribute data to series name
  rownames(x) <- as.character(x$series)
  
  # GCS WGS84
  return(x)
}

.raster_extent <- function(s, timeout) {
  
  if (!requireNamespace("terra")) 
    stop("package terra is required to return raster series extent grids", call. = FALSE)
  
  # base URL to cached data
  u <- URLencode(paste0('https://casoilresource.lawr.ucdavis.edu/series-extent-cache/grid/', s, '.tif'))
  
  # init temp files
  tf <- tempfile(fileext = '.tif')
  
  # safely download GeoTiff file
  # Mac / Linux: file automatically downloaded via binary transfer
  # Windows: must manually specify binary transfer
  res <- tryCatch(
    suppressWarnings(
      download.file(url = u, destfile = tf, extra = c(timeout = timeout), quiet = TRUE, mode = 'wb')  
    ),
    error = function(e) {e}
  )
  
  # trap errors
  if (inherits(res, 'error')) {
    message('no data returned')
    return(NULL)
  }
  
  # init SpatRaster
  x <- terra::rast(tf)
  
  # load all values into memory
  terra::values(x) <- terra::values(x)
  
  # remove tempfile 
  unlink(tf)
  
  # transfer layer name
  names(x) <- gsub(pattern = '_', replacement = ' ', x = s, fixed = TRUE)
  
  # make CRS explicit
  terra::crs(x) <- 'EPSG:5070'
  
  return(x)
}

