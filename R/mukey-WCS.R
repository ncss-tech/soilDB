

#' @title gNATSGO / gSSURGO Map Unit Key Web Coverage Service (WCS)
#' 
#' @description Download chunks of the gNATSGO or gSSURGO map unit key grid via bounding-box from the SoilWeb WCS.
#' 
#' @author D.E. Beaudette and A.G. Brown
#'
#' @param aoi area of interest (AOI) defined using either a \code{Spatial*}, \code{RasterLayer}, \code{sf}, \code{sfc} or \code{bbox} object, or a \code{list}, see details
#'
#' @param db name of the gridded map unit key grid to access, should be either 'gnatsgo' or 'gssurgo'
#'
#' @param res grid resolution, units of meters. The native resolution of gNATSGO and gSSURGO (this WCS) is 30m.
#'
#' @param quiet logical, passed to \code{download.file} to enable / suppress URL and progress bar for download.
#'
#' @note The gNATSGO grid includes raster soil survey map unit keys which are not in SDA.
#'
#' @details \code{aoi} should be specified as one of: \code{Spatial*}, \code{RasterLayer}, \code{sf}, \code{sfc}, \code{bbox} object, or a \code{list} containing:
#'
#' \describe{
#'   \item{\code{aoi}}{bounding-box specified as (xmin, ymin, xmax, ymax) e.g. c(-114.16, 47.65, -114.08, 47.68)}
#'   \item{\code{crs}}{coordinate reference system of BBOX, e.g. '+init=epsg:4326'}
#' }
#'
#' The WCS query is parameterized using \code{raster::extent} derived from the above AOI specification, after conversion to the native CRS (EPSG:6350) of the gNATSGO / gSSURGO grid.
#' 
#' Databases available from this WCS can be queried using \code{WCS_details(wcs = 'mukey')}.
#' 
#' @return \code{raster} object containing indexed map unit keys and associated raster attribute table
#'
#' @export
#'
mukey.wcs <- function(aoi, db = c('gnatsgo', 'gssurgo'), res = 30, quiet = FALSE) {

  if(!requireNamespace('rgdal', quietly=TRUE))
    stop('please install the `rgdal` package', call.=FALSE)

  # sanity check: db name
  db <- match.arg(db)

  # sanity check: aoi specification
  if(!inherits(aoi, c('list', 'Spatial', 'sf', 'sfc', 'bbox', 'RasterLayer'))) {
    stop('invalid `aoi` specification', call. = FALSE)
  }

  # reasonable resolution
  if(res < 30 | res > 3000) {
    stop('`res` should be within 30 <= res <= 3000 meters')
  }


  # prepare WCS details
  var.spec <- .mukey.spec[[db]]
  # prepare AOI in native CRS
  wcs.geom <- .prepare_AEA_AOI(obj = aoi, res = res)
  
  ## TODO: investigate why this is so
  # sanity check: a 1x1 pixel request to WCS results in a corrupt GeoTiff 
  if(wcs.geom$width == 1 & wcs.geom$height == 1) {
    stop('WCS requests for a 1x1 pixel image are not supported, try a smaller resolution', call. = FALSE)
  }


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
    '&FORMAT=GEOTIFF_FLOAT',
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
  
  ## TODO: suppressWarnings() used to quiet proj4string noise until we have a better solution
  #        https://stackoverflow.com/questions/63727886/proj4-to-proj6-upgrade-and-discarded-datum-warnings
  #        also, the source files should be re-made with updated CRS info in the GeoTiff metadata
  
  # load pointer to file and return
  r <- try(
    suppressWarnings(raster(tf)),
    silent = TRUE
  )

  if(inherits(r, 'try-error')) {
    stop('result is not a valid GeoTiff, why?', call. = FALSE)
  }

  # source data are UINT32 (INT4U in raster pkg) data
  # converted to FLOAT32 by WCS
  dataType(r) <- 'INT4U'

  ## TODO: this isn't quite right... '0' is returned by the WCS sometimes
  # specification of NODATA
  # this doesn't seem to make it through the WCS
  # value is derived from the original UINT32 grid
  NAvalue(r) <- 2147483647

  # read into memory to make NODATA value permanent
  r <- readAll(r)

  # set layer name in object
  names(r) <- var.spec$desc
  # and as an attribute
  attr(r, 'layer name') <- var.spec$desc

  # init RAT
  r <- ratify(r)

  return(r)
}

