
#' @title ISSR-800 Web Coverage Service Interface
#'
#' @param var variable label
#' @param aoi area of interest as WGS84 coordinates c(-121, 37, -120, 38) (xmin, ymin, xmax, ymax)
#' @param res grid cell resolution (units specific to \code{crs}), typically 800 (meters), the native resolution of ISSR-800
#' @param crs coordinate reference specification in the form `EPSG:6350`, must contain a valid EPSG code
#' 
#' @note This is an experimental interface that can change at any time. gNATSGO also includes raster soil survey mukeys which are not in SDA.
#'
#' @return \code{raster} object
#' @export
#'
ISSR800.wcs <- function(var, aoi, res = 800, crs = 'EPSG:6350') {
  
  if(!requireNamespace('rgdal', quietly=TRUE))
    stop('please install the `rgdal` package', call.=FALSE)
  
  ## TODO: 
  # sanity checks
  # error trapping
  # WCS request errors
  # ???
  
  var.spec <- .ISSR800.spec[[var]]
  
  wcs.geom <- .prepare_AEA_AOI_fromWGS84(aoi, res = res, targetCRS = crs)
  
  # base URL + parameters
  base.url <- 'https://soilmap2-1.lawr.ucdavis.edu/cgi-bin/mapserv?'
  service.url <- 'map=/soilmap2/website/wcs/issr800.map&SERVICE=WCS&VERSION=1.0.0&REQUEST=GetCoverage'
  
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
  
  ## TODO: this isn't correct for all data (e.g. SAR), how do we set this server-side?
  # specification of NODATA
  NAvalue(r) <- 0
  
  ## TODO: should probably save to a file and return a pointer
  r <- readAll(r)
  
  # set layer name in object
  names(r) <- var.spec$desc
  # and as an attribute
  attr(r, 'layer name') <- var.spec$desc
  
  # optional processing of RAT
  if(! is.null(var.spec$rat)) {
    
    # get rat
    rat <- read.csv(var.spec$rat, stringsAsFactors = FALSE)
    
    # rename ID column
    names(rat)[2] <- 'ID'
    
    # convert to RAT-enabled raster
    r <- ratify(r)
    
    # get / merge codes
    ll <- levels(r)[[1]]
    ll <- merge(ll, rat, by = 'ID', sort = FALSE, all.x = TRUE)
    levels(r) <- ll
  }
  
  return(r)
}




