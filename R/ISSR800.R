
#' @title ISSR-800 Web Coverage Service (WCS)
#'
#' @param aoi area of interest (AOI) defined using a \code{Spatial*}, a \code{sf}, \code{sfc} or \code{bbox} object or a \code{list}, see details
#' 
#' @param var ISSR-800 grid name, see details
#' 
#' @param res grid resolution, units of meters. The native resolution of ISSR-800 grids (this WCS) is 800m.
#' 
#' @param quiet logical, passed to \code{download.file} to enable / suppress URL and progress bar for download.
#'  
#' @details \code{aoi} should be specified as either a \code{Spatial*}, \code{sf}, \code{sfc} or \code{bbox} object or a \code{list} containing:
#' 
#' \describe{
#'   \item{\code{aoi}}{bounding-box specified as (xmin, ymin, xmax, ymax) e.g. c(-114.16, 47.65, -114.08, 47.68)}
#'   \item{\code{crs}}{coordinate reference system of BBOX, e.g. '+init=epsg:4326'}
#' }
#' 
#' The WCS query is parameterized using \code{raster::extent} derived from the above AOI specification, after conversion to the native CRS (EPSG:6350) of the ISSR-800 grids.
#' 
#' Variables available from this WCS can be queried using \code{WCS_details(wcs = 'ISSR800')}.
#' 
#' @return \code{raster} object containing indexed map unit keys and associated raster attribute table
#' 
#' @export
ISSR800.wcs <- function(aoi, var, res = 800, quiet = FALSE) {
  
  if(!requireNamespace('rgdal', quietly=TRUE))
    stop('please install the `rgdal` package', call.=FALSE)
  
  # sanity check: aoi specification
  if(!inherits(aoi, c('list', 'Spatial', 'sf', 'sfc', 'bbox'))) { # TODO:  'wk_rct'?
    stop('invalid `aoi` specification', call. = FALSE)
  }
  
  # reasonable resolution
  if(res < 400 | res > 1600) {
    stop('`res` should be within 400 <= res <= 1600 meters')
  }
  
  
  # get layer specs
  var.spec <- .ISSR800.spec[[var]]
  
  # compute BBOX / IMG geometry in native CRS
  wcs.geom <- .prepare_AEA_AOI(aoi, res = res)
  
  # sanity check: keep output images within a reasonable limit
  # limits set in the MAPFILE
  max.img.dim <- 5000
  
  # check image size > max.img.dim
  if(wcs.geom$height > max.img.dim | wcs.geom$width > max.img.dim) {
    msg <- sprintf(
      'AOI is too large: %sx%s pixels requested (%sx%s pixels max)', 
      wcs.geom$width, 
      wcs.geom$height, 
      max.img.dim,
      max.img.dim
    )
    
    stop(msg, call. = FALSE)
  }
  
  # base URL + parameters
  base.url <- 'http://soilmap2-1.lawr.ucdavis.edu/cgi-bin/mapserv?'
  service.url <- 'map=/soilmap2/website/wcs/issr800.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage'
  
  # unpack BBOX for WCS 2.0
  xmin <- wcs.geom$bbox[1]
  xmax <- wcs.geom$bbox[3]
  ymin <- wcs.geom$bbox[2]
  ymax <- wcs.geom$bbox[4]
  
  # compile WCS 2.0 style URL
  u <- paste0(
    base.url,
    service.url,
    '&COVERAGEID=', var.spec$dsn,
    '&FORMAT=image/tiff',
    '&GEOTIFF:COMPRESSION=Deflate',
    '&SUBSETTINGCRS=EPSG:6350',
    '&FORMAT=', var.spec$type,
    '&SUBSET=x(', xmin, ',', xmax,')',
    '&SUBSET=y(', ymin, ',', ymax,')',
    '&RESOLUTION=x(', res, ')',
    '&RESOLUTION=y(', res, ')'
  )
  
  # get data
  tf <- tempfile()
  dl.try <- try(
    suppressWarnings(
      download.file(u, destfile = tf, mode = 'wb', quiet = quiet)
    ),
    silent = TRUE
  )
  
  if(inherits(dl.try, 'try-error')) {
    stop('bad WCS request', call. = FALSE) 
  }
  
  # load pointer to file and return
  r <- try(
    raster(tf),
    silent = TRUE
  )
  
  if(inherits(r, 'try-error')) {
    stop('result is not a valid GeoTiff, why?', call. = FALSE)
  }
  
  ## TODO: this isn't quite right... '0' is returned by the WCS sometimes
  # TODO: how can this be set server-side?
  # specification of NODATA using local definitions
  # NAvalue(r) <- var.spec$na
  NAvalue(r) <- 0
  
  ## TODO: should probably save to a file and return a pointer
  r <- readAll(r)
  
  # set layer name in object
  names(r) <- var.spec$desc
  # and as an attribute
  attr(r, 'layer name') <- var.spec$desc
  
  # optional processing of RAT
  if(! is.null(var.spec$rat)) {
    
    # convert to RAT-enabled raster
    r <- ratify(r)
    
    # get rat
    rat <- read.csv(var.spec$rat, stringsAsFactors = FALSE)
    
    # rename ID column
    names(rat)[2] <- 'ID'
    
    # get / merge codes
    ll <- raster::levels(r)[[1]]
    
    ll <- base::merge(ll, rat, by = 'ID', sort = FALSE, all.x = TRUE)
    levels(r) <- ll
  }
  
  return(r)
}




