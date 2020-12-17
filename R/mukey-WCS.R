
#' @title gNATSGO / gSSURGO Web Coverage Service Interface
#'
#' @param var variable label
#' @param aoi area of interest as WGS84 coordinates c(-121, 37, -120, 38) (xmin, ymin, xmax, ymax)
#' @param res grid cell resolution (units specific to \code{crs}), typically 30 (meters), the native resolution of gNATSGO and gSSURGO
#' @param crs coordinate reference specification in the form `EPSG:6350`, must contain a valid EPSG code
#' 
#' @note This is an experimental interface that can change at any time.
#'
#' @return \code{raster} object containing indexed map unit keys and associated raster attribute table
#' @export
#'
mukey.wcs <- function(var = 'gnatsgo', aoi, res = 30, crs = 'EPSG:6350') {
  
  if(!requireNamespace('rgdal', quietly=TRUE))
    stop('please install the `rgdal` package', call.=FALSE)
  
  ## TODO: 
  # sanity checks
  # error trapping
  # WCS request errors
  # ???
  
  var.spec <- .mukey.spec[[var]]
  
  wcs.geom <- .prepare_AEA_AOI_fromWGS84(aoi, res = res, targetCRS = crs)
  
  # base URL + parameters
  base.url <- 'https://soilmap2-1.lawr.ucdavis.edu/cgi-bin/mapserv?'
  service.url <- 'map=/soilmap2/website/wcs/mukey.map&SERVICE=WCS&VERSION=1.0.0&REQUEST=GetCoverage'
  
  ## I'm pretty sure this is right
  # native CRS is 'EPSG:6350'
  
  # compile full URL
  u <- paste0(
    base.url,
    service.url,
    '&CRS=', crs,
    '&coverage=', var.spec$dsn,
    '&FORMAT=', var.spec$type,
    '&BBOX=', paste(wcs.geom$bbox, collapse = ','),
    '&WIDTH=', wcs.geom$width,
    '&HEIGHT=', wcs.geom$height
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
  
  return(r)
}

