
#' @title Access gridded soil color data from the SoilWeb Web Coverage Service (WCS)
#' 
#' @author D.E. Beaudette and A.G. Brown
#' 
#' @description Moist soil colors, FY2026.
#' 
#' @param aoi area of interest (AOI) defined using a terra pakcage objects (`SpatRaster`, `SpatVector`, `ext`), sf objects (`sf`, `sfc`), raster package object (`RasterLayer`, `bbox`), sp package `Spatial*`, or a `list`, see details
#' 
#' @param var soil color grid name (case insensitive), see details
#' 
#' @param res grid resolution, leave as NULL to use native grid resolution, see details
#' 
#' @param quiet logical, passed to `curl::curl_download` to enable / suppress URL and progress bar for download.
#'  
#' @details When specified as a `list`, `aoi` should contain:
#' 
#'   * `aoi`: bounding-box specified as (xmin, ymin, xmax, ymax) e.g. c(-114.16, 47.65, -114.08, 47.68)
#'   * `crs`: coordinate reference system of BBOX, e.g. 'OGC:CRS84' (EPSG:4326, WGS84 Longitude/Latitude)
#' 
#' The WCS query is parameterized using a rectangular extent derived from the above AOI specification, after conversion to the native CRS of the soil color grids. 
#' 
#' Variables available from this WCS can be queried using `WCS_details(wcs = 'soilColor')`. The full resolution version of the CONUS soil color grids use a `hr` suffix, e.g. 'sc025cm_hr'.
#' 
#' 
#' @return A `SpatRaster` (or `RasterLayer`) object containing indexed map unit keys and associated raster attribute table or a try-error if request fails. By default, spatial classes from the `terra` package are returned. If the input object class is from the `raster` or `sp` packages a `RasterLayer` is returned.
#' 
#' @examples 
#' \dontrun{
#' library(terra)
#' 
#' # see WCS_details() for variable options
#' WCS_details(wcs = 'soilColor')
#' 
#' # moist soil color at 25cm, 270m version
#' res <- soilColor.wcs(list(aoi = c(-116, 35, -115.5, 35.5), crs = "EPSG:4326"), 
#'                    var = 'sc025cm', res = 270)
#'
#' # note colors and other metadata are stored
#' # in raster attribute table
#' terra::plot(res, col = cats(res)[[1]]$col, axes = FALSE, legend = FALSE)
#' }
#' @export
soilColor.wcs <- function(aoi, var, res = NULL, quiet = FALSE) {
  
  # vintage of source data
  .vintage <- 'FY2026'
  
  # sanity check: AOI specification
  if (!inherits(aoi, c('list', 'Spatial', 'sf', 'sfc', 'bbox', 'RasterLayer', 'SpatRaster', 'SpatVector'))) { 
    stop('invalid `aoi` specification', call. = FALSE)
  }
  
  # match variable name in catalog
  var.cat <- sapply(.soilColor.spec, '[[', 'dsn')
  var <- match.arg(tolower(var), choices = var.cat)
  
  # get variable specs
  var.spec <- .soilColor.spec[[var]]
  
  # use native resolution of DSN
  if(is.null(res)) {
    res <- var.spec$res
  } else {
    # user-defined resolution
    
    # NOTE: this doesn't apply to WGS84 grids like PW, AS, MI
    # ensure reasonable resolution
    if (res < 30 || res > 1000) {
      stop('`res` should be within 30 <= res <= 1000 meters')
    }
  }
  
  # compute BBOX / IMG geometry in native CRS
  wcs.geom <- .prepare_AOI(aoi, res = res, native_crs = var.spec$crs)
  
  ## TODO: investigate why this is so
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
  base.url <- 'http://soilmap4-1.lawr.ucdavis.edu/cgi-bin/mapserv?'
  service.url <- 'map=/data1/website/wcs/soilcolor.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage'
  
  # unpack BBOX for WCS 2.0
  xmin <- wcs.geom$bbox[1]
  ymin <- wcs.geom$bbox[2]
  
  # xmax and ymax are now calculated from AOI dimensions and resolution
  # xmax <- wcs.geom$bbox[3]
  # ymax <- wcs.geom$bbox[4]
  
  # recalculate x/ymax based on xmin + resolution multiplied by AOI dims
  xmax2 <- xmin + res * wcs.geom$width
  ymax2 <- ymin + res * wcs.geom$height
  
  # WCS notes:
  # https://github.com/geographika/wcs-test
  
  # compile WCS 2.0 style URL
  u <- paste0(
    base.url,
    service.url,
    '&COVERAGEID=', var.spec$dsn,
    '&FORMAT=image/tiff',
    '&GEOTIFF:COMPRESSION=LZW',
    '&SUBSETTINGCRS=', var.spec$crs,
    '&FORMAT=', var.spec$type,
    '&SUBSET=x(', xmin, ',', xmax2, ')',
    '&SUBSET=y(', ymin, ',', ymax2, ')',
    '&RESOLUTION=x(', res, ')',
    '&RESOLUTION=y(', res, ')'
  )
  
  # get data
  tf <- tempfile()
  dl.try <- try(
    suppressWarnings(
      curl::curl_download(u, destfile = tf, mode = 'wb', handle = .soilDB_curl_handle(), quiet = quiet)
    ),
    silent = TRUE
  )
  
  if (inherits(dl.try, 'try-error')) {
    message('bad WCS request')
    return(dl.try)
  }
  
  # load pointer to file and return
  # NODATA correctly interpreted when it is max(Int16) --> 32767
  r <- try(terra::rast(tf), silent = TRUE)
  
  if (inherits(r, 'try-error')) {
    message(attr(r, 'condition'))
    stop('result is not a valid GeoTIFF', call. = FALSE)
  }
  
  # load all values into memory
  terra::values(r) <- terra::values(r)
  
  # remove tempfile 
  unlink(tf)
  
  # set layer name in object
  names(r) <- var.spec$desc
  
  # make categories + RAT
  r <- terra::as.factor(r)
  rat <- terra::cats(r)[[1]]
  
  # optional processing of color LUT
  if (!is.null(var.spec$rat)) {
    
    # get color LUT
    lut <- try(read.csv(var.spec$rat, stringsAsFactors = FALSE), silent = TRUE)
    
    if(inherits(lut, 'try-error')) {
      stop('cannot access soil color lookup table')
    }
    
    # color ID is always in the first column
    # use the name expected by terra::levels() and terra::cats()
    names(lut)[1] <- 'ID'
    
    # create hex notation for color
    lut$col <- rgb(lut$r, lut$g, lut$b, maxColorValue = 255)
    
    # merge RAT + LUT
    rat <- merge(rat[, 'ID', drop = FALSE], lut, by = 'ID', all.x = TRUE, sort = FALSE)
    
    # register RAT
    levels(r) <- rat
    
    # set color table
    terra::coltab(r) <- rat[, c('ID', 'r', 'g', 'b')]
  }
  
  input_class <- attr(wcs.geom, '.input_class')
  
  if (((!is.null(input_class) && input_class == "raster") ||
      getOption('soilDB.return_Spatial', default = FALSE)) && 
      requireNamespace("raster")) {
    r <- raster::raster(r)
  }
  
  # set metadata
  terra::metags(r) <- c(description = var.spec$desc, vintage = .vintage)
  
  return(r)
}

