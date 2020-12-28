
## TODO: finish documenting new AOI interface

#' @title gNATSGO / gSSURGO Web Coverage Service Interface
#'
#' @param db 'gnatsgo' or 'gssurgo'
#' @param aoi area of interest defined using a list + CRS or \code{Spatial*} object, see details
#' @param res grid cell resolution (units specific to \code{crs}), typically 30 (meters), the native resolution of gNATSGO and gSSURGO
#' @note This is an experimental interface that can change at any time. gNATSGO also includes raster soil survey mukeys which are not in SDA.
#' 
#' @details \code{aoi} should be specified as a \code{list} with BBOX specified \code{c(-121, 37, -120, 38)} (xmin, ymin, xmax, ymax) and valid CRS. ...
#'
#' @return \code{raster} object containing indexed map unit keys and associated raster attribute table
#' @export
#'
mukey.wcs <- function(db = c('gnatsgo', 'gssurgo'), aoi, res = 30) {
  
  if(!requireNamespace('rgdal', quietly=TRUE))
    stop('please install the `rgdal` package', call.=FALSE)
  
  # sanity check: db
  db <- match.arg(db)
  
  # sanity check: aoi specification
  if(!inherits(aoi, c('list', 'Spatial'))) {
    stop('invalid `aoi` specification', call. = FALSE)
  }
  
  ## TODO: 
  # sanity checks
  # error trapping
  # WCS request errors
  # ???
  
  # sanity check, resolution must be > 30
  
  
  var.spec <- .mukey.spec[[db]]
  
  wcs.geom <- .prepare_AEA_AOI(obj = aoi, res = res)
  
  # base URL + parameters
  base.url <- 'http://soilmap2-1.lawr.ucdavis.edu/cgi-bin/mapserv?'
  service.url <- 'map=/soilmap2/website/wcs/mukey.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage'
  
  # map=/soilmap2/website/wcs/mukey.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=gnatsgo&FORMAT=image/tiff&GEOTIFF:COMPRESSION=Deflate&SUBSET=x(-2178680,-2065431)&SUBSET=y(1816510,1947200)&RESOLUTION=x(30)&RESOLUTION=y(30)&INTERPOLATION=NEAREST'
  
  ## I'm pretty sure this is right
  # native CRS is 'EPSG:6350'
  
  ## TODO: output type should match source data
  # , var.spec$type
  # -> this is specified in the layer metadata
  
  ## TODO: re-work spec pre-processing code for WCS 2.0
  # unpack BBOX
  xmin <- wcs.geom$bbox[1]
  xmax <- wcs.geom$bbox[3]
  ymin <- wcs.geom$bbox[2]
  ymax <- wcs.geom$bbox[4]
  
  # compile full URL
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
  download.file(u, destfile = tf, mode = 'wb')
  
  # load pointer to file and return
  r <- raster(tf)
  
  ## TODO: how do we set this server-side?
  # specification of NODATA
  NAvalue(r) <- 0
  
  ## TODO: should probably save to a file and return a pointer
  r <- readAll(r)
  
  # set layer name in object
  names(r) <- var.spec$desc
  # and as an attribute
  attr(r, 'layer name') <- var.spec$desc
  
  # init RAT
  r <- ratify(r)
  
  # optional joining of data from SDA?
  # that will happen later
  
  return(r)
}

