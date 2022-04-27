

#' @title Get gNATSGO / gSSURGO Map Unit Key (`mukey`) grid from SoilWeb Web Coverage Service (WCS)
#' 
#' @description Download chunks of the gNATSGO or gSSURGO map unit key grid via bounding-box from the SoilWeb WCS.
#' 
#' @author D.E. Beaudette and A.G. Brown
#'
#' @param aoi area of interest (AOI) defined using either a \code{Spatial*}, \code{RasterLayer}, \code{sf}, \code{sfc} or \code{bbox} object, or a \code{list}, see details
#' @param db name of the gridded map unit key grid to access, should be either 'gNATSGO' or 'gSSURGO' (case insensitive)
#' @param res grid resolution, units of meters. The native resolution of gNATSGO and gSSURGO (this WCS) is 30m.
#' @param quiet logical, passed to \code{download.file} to enable / suppress URL and progress bar for download.
#'
#' @note The gNATSGO grid includes raster soil survey map unit keys which are not in SDA.
#'
#' @details \code{aoi} should be specified as one of: \code{SpatRaster}, \code{Spatial*}, \code{RasterLayer}, \code{sf}, \code{sfc}, \code{bbox} object, OR a \code{list} containing:
#'
#' \describe{
#'   \item{\code{aoi}}{bounding-box specified as (xmin, ymin, xmax, ymax) e.g. c(-114.16, 47.65, -114.08, 47.68)}
#'   \item{\code{crs}}{coordinate reference system of BBOX, e.g. 'OGC:CRS84' (EPSG:4326, WGS84 Longitude/Latitude)}
#' }
#'
#' The WCS query is parameterized using a rectangular extent derived from the above AOI specification, after conversion to the native CRS (EPSG:5070) of the gNATSGO / gSSURGO grid.
#' 
#' Databases available from this WCS can be queried using \code{WCS_details(wcs = 'mukey')}.
#' 
#' @return A SpatRaster (or RasterLayer) object containing indexed map unit keys and associated raster attribute table or a try-error if request fails. By default, spatial classes from the `terra` package are returned. If the input object class is from the `raster` or `sp` packages a RasterLayer is returned. 
#'
#' @export
#' @examples 
#' \dontrun{
#' library(terra)
#' 
#' res <- mukey.wcs(list(aoi = c(-116.7400, 35.2904, -116.7072, 35.3026), crs = "EPSG:4326"),
#'                  db = 'gNATSGO', res = 30) 
#'   
#' MUKEY <- unique(values(res))
#' 
#' prp <- setNames(
#'   get_SDA_property(
#'     c("ph1to1h2o_r", "claytotal_r"),
#'     "weighted average",
#'     mukeys = MUKEY,
#'     top_depth = 0,
#'     bottom_depth = 25,
#'     include_minors = TRUE,
#'     miscellaneous_areas = FALSE
#'   )[, c("mukey", "ph1to1h2o_r", "claytotal_r")],
#'   c("ID",    "pH1to1_0to25", "clay_0to25")
#' )
#' 
#' levels(res) <- prp
#' res2 <- catalyze(res)
#' res2
#' 
#' plot(res2[['pH1to1_0to25']])
#' }
mukey.wcs <- function(aoi, db = c('gNATSGO', 'gSSURGO', 'RSS'), res = 30, quiet = FALSE) {

  # sanity check: db name
  db <- match.arg(tolower(db[1]), choices = c('gnatsgo', 'gssurgo', 'rss'))

  # sanity check: aoi specification
  if (!inherits(aoi, c('list', 'Spatial', 'sf', 'sfc', 'bbox', 'RasterLayer', 'SpatRaster', 'SpatVector'))) { 
    stop('invalid `aoi` specification', call. = FALSE)
  }

  # reasonable resolution
  if(db %in% c('gssurgo', 'gnatsgo')) {
    if (res < 30 || res > 3000) {
      stop('`res` should be within 30 <= res <= 3000 meters')
    }
  }
  
  # reasonable resolution
  if(db == 'rss') {
    if (res < 10 || res > 1000) {
      stop('`res` should be within 10 <= res <= 1000 meters')
    }
  }
  

  # prepare WCS details
  # list-lookup is lower-case
  var.spec <- .mukey.spec[[db]]
  
  ## TODO: if a resolution isn't specified, use the data-specific default
  # var.spec$res
  
  # prepare AOI in native CRS
  wcs.geom <- .prepare_AEA_AOI(obj = aoi, res = res)
  
  ## TODO: investigate why
  # sanity check: a 1x1 pixel request to WCS results in a corrupt GeoTiff 
  if (wcs.geom$width == 1 && wcs.geom$height == 1) {
    stop('WCS requests for a 1x1 pixel image are not supported, try a smaller resolution', call. = FALSE)
  }

  # sanity check: keep output images within a reasonable limit
  # limits set in the MAPFILE
  max.img.dim <- 5000

  # check image size > max.img.dim
  if (wcs.geom$height > max.img.dim || wcs.geom$width > max.img.dim) {
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
  ymin <- wcs.geom$bbox[2]
  
  # xmax and ymax are now calculated from AOI dimensions and resolution
  # xmax <- wcs.geom$bbox[3]
  # ymax <- wcs.geom$bbox[4]

  # scaling factors
  # sclx <- ((xmax - xmin) / res) / wcs.geom$width
  # scly <- ((ymax - ymin) / res) / wcs.geom$height
  
  # recalculate x/ymax based on xmin + resolution multiplied by AOI dims
  xmax2 <- xmin + res * wcs.geom$width
  ymax2 <- ymin + res * wcs.geom$height
  
  # compile WCS 2.0 style URL
  u <- paste0(
    base.url,
    service.url,
    '&COVERAGEID=', var.spec$dsn,
    '&FORMAT=image/tiff',
    '&GEOTIFF:COMPRESSION=DEFLATE',
    '&SUBSETTINGCRS=EPSG:5070',
    '&FORMAT=GEOTIFF_FLOAT',
    '&SUBSET=x(', xmin, ',', xmax2, ')',
    '&SUBSET=y(', ymin, ',', ymax2, ')',
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

  if (inherits(dl.try, 'try-error')) {
   message('bad WCS request')
   return(dl.try)
  }
  
  # load pointer to file 
  r <- try(terra::rast(tf), silent = TRUE)
  
  if (inherits(r, 'try-error')) {
    message(attr(r, 'condition'))
    stop('result is not a valid GeoTIFF', call. = FALSE)
  }
  
  ## TODO: this isn't quite right... '0' is returned by the WCS sometimes
  # specification of NODATA
  # this doesn't seem to make it through the WCS
  # value is derived from the original UINT32 grid
  terra::NAflag(r) <- 2147483647

  # load all values into memory
  terra::values(r) <- terra::values(r)
  
  # remove tempfile 
  unlink(tf)

  # build RAT
  uids <- terra::unique(r)[,1]
  rat <- data.frame(value = uids, 
                    mukey = uids,
                    ID = uids)
  r <- terra::categories(r, layer = 1, rat)
  
  # set layer name in object
  names(r) <- 'mukey'
  
  # and as an attribute
  attr(r, 'layer name') <- var.spec$desc

  input_class <- attr(wcs.geom, '.input_class')
  
  if ((!is.null(input_class) && input_class == "raster") ||
      getOption('soilDB.return_Spatial', default = FALSE)) {
    if (requireNamespace("raster")) {
      r <- raster::raster(r)
    }
  }
  
  return(r)
}

