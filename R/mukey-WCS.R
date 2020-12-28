

#' @title gNATSGO / gSSURGO Map Unit Key Web Coverage Service (WCS)
#'
#' @param db name of the gridded map unit key grid to access, should be either 'gnatsgo' or 'gssurgo'
#' 
#' @param aoi area of interest (AOI) defined using a \code{list} or \code{Spatial*} object, see details
#' 
#' @param res grid resolution, units of meters. The native resolution of gNATSGO and gSSURGO (this WCS) is 30m.
#' 
#' @param quiet logical, passed to \code{download.file} to enable / suppress URL and progress bar for download.
#' 
#' @note This is an experimental interface that can change at any time. The gNATSGO grid includes raster soil survey map unit keys which are not in SDA.
#' 
#' @details \code{aoi} should be specified as either a \code{Spatial*} object or a \code{list} containing:
#' 
#' \describe{
#'   \item{\code{aoi}}{bounding-box specified as (xmin, ymin, xmax, ymax) e.g. c(-114.16, 47.65, -114.08, 47.68)}
#'   \item{\code{crs}}{coordinate reference system of BBOX, e.g. '+init=EPSG:4326'}
#' }
#' 
#' The WCS query is parameterized using \code{raster::extent} derived from the above AOI specification, after conversion to the native CRS (EPSG:6350) of the gNATSGO / gSSURGO grid.
#' 
#' @return \code{raster} object containing indexed map unit keys and associated raster attribute table
#' 
#' @export
#'
mukey.wcs <- function(db = c('gnatsgo', 'gssurgo'), aoi, res = 30, quiet = FALSE) {
  
  if(!requireNamespace('rgdal', quietly=TRUE))
    stop('please install the `rgdal` package', call.=FALSE)
  
  # sanity check: db name
  db <- match.arg(db)
  
  # sanity check: aoi specification
  if(!inherits(aoi, c('list', 'Spatial'))) {
    stop('invalid `aoi` specification', call. = FALSE)
  }
  
  # reasonable resolution
  if(res < 30 | res > 3000) {
    stop('`res` should be within 30 <= res <= 3000 meters')
  }
  
  ## TODO: 
  # sanity checks
  # error trapping
  # WCS request errors
  # native CRS is 'EPSG:6350'
  
  # prepare WCS details
  var.spec <- .mukey.spec[[db]]
  # prepare AOI in native CRS
  wcs.geom <- .prepare_AEA_AOI(obj = aoi, res = res)
  
  
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
  service.url <- 'map=/soilmap2/website/wcs/mukey.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage'
  
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
  
  # source data are UINT32 (INT4U in raster pkg) data
  # converted to FLOAT32 by WCS
  dataType(r) <- 'INT4U'
  
  # specification of NODATA
  # this doesn't seem to make it through the WCS
  # value is derived from the original UINT32 grid
  NAvalue(r) <- 2147483647
  
  ## TODO: should probably save to a file and return a pointer
  r <- readAll(r)
  
  # set layer name in object
  names(r) <- var.spec$desc
  # and as an attribute
  attr(r, 'layer name') <- var.spec$desc
  
  # init RAT
  r <- ratify(r)
  
  return(r)
}

