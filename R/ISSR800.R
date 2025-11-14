
#' @title Get 800m gridded soil properties from SoilWeb ISSR-800 Web Coverage Service (WCS)
#' @author D.E. Beaudette and A.G. Brown
#' @description Intermediate-scale gridded (800m) soil property and interpretation maps from aggregated SSURGO and STATSGO data. These maps were developed by USDA-NRCS-SPSD staff in collaboration with UCD-LAWR. Originally for educational use and \href{https://casoilresource.lawr.ucdavis.edu/soil-properties/}{interactive thematic maps}, these data are a suitable alternative to gridded STATSGO-derived thematic soil maps. The full size grids can be \href{https://casoilresource.lawr.ucdavis.edu/soil-properties/download.php}{downloaded here}.
#' @param aoi area of interest (AOI) defined using a \code{Spatial*}, \code{RasterLayer}, \code{sf}, \code{sfc} or \code{bbox} object, OR a \code{list}, see details
#' @param var ISSR-800 grid name (case insensitive), see details 
#' @param res grid resolution, units of meters. The native resolution of ISSR-800 grids (this WCS) is 800m.
#' @param quiet logical, passed to \code{curl::curl_download} to enable / suppress URL and progress bar for download.
#'  
#' @details \code{aoi} should be specified as a \code{SpatRaster}, \code{Spatial*}, \code{RasterLayer}, \code{SpatRaster}/\code{SpatVector}, \code{sf}, \code{sfc}, or \code{bbox} object or a \code{list} containing:
#' 
#' \describe{
#'   \item{\code{aoi}}{bounding-box specified as (xmin, ymin, xmax, ymax) e.g. c(-114.16, 47.65, -114.08, 47.68)}
#'   \item{\code{crs}}{coordinate reference system of BBOX, e.g. 'OGC:CRS84' (EPSG:4326, WGS84 Longitude/Latitude)}
#' }
#' 
#' The WCS query is parameterized using a rectangular extent derived from the above AOI specification, after conversion to the native CRS (EPSG:5070) of the ISSR-800 grids.
#' 
#' Variables available from this WCS can be queried using \code{WCS_details(wcs = 'ISSR800')}.
#' 
#' @note There are still some issues to be resolved related to the encoding of NA Variables with a natural zero (e.g. SAR) have 0 set to NA.
#' 
#' @return A SpatRaster (or RasterLayer) object containing indexed map unit keys and associated raster attribute table or a try-error if request fails. By default, spatial classes from the `terra` package are returned. If the input object class is from the `raster` or `sp` packages a RasterLayer is returned. 
#' @examples 
#' \dontrun{
#' library(terra)
#' 
#' # see WCS_details() for variable options
#' WCS_details(wcs = 'ISSR800')
#' 
#' # get wind erodibility group
#' res <- ISSR800.wcs(list(aoi = c(-116, 35, -115.5, 35.5), crs = "EPSG:4326"), 
#'                    var = 'weg', res = 800)
#' plot(res)
#' }
#' @export
ISSR800.wcs <- function(aoi, var, res = 800, quiet = FALSE) {
  
  ## vintage of source data
  
  # TODO: use remote metadata
  #  --> must add to load-balancer
  # .vintage <- readLines('http://casoilresource.lawr.ucdavis.edu/wcs-files/...')
  
  # hard-coded
  .vintage <- 'FY2024'
  
  if (!requireNamespace("terra")) {
    stop("package 'terra' is required", call. = FALSE)
  }
  
  # sanity check: AOI specification
  if (!inherits(aoi, c('list', 'Spatial', 'sf', 'sfc', 'bbox', 'RasterLayer', 'SpatRaster', 'SpatVector'))) { 
    stop('invalid `aoi` specification', call. = FALSE)
  }
  
  # reasonable resolution
  if (res < 400 || res > 1600) {
    stop('`res` should be within 400 <= res <= 1600 meters')
  }
  
  # match variable name in catalog
  var.cat <- sapply(.ISSR800.spec, '[[', 'dsn')
  var <- match.arg(tolower(var), choices = var.cat)
  
  # get variable specs
  var.spec <- .ISSR800.spec[[var]]
  
  # 2024-04-23: updated grid system
  # 3620, 5769, 1  (nrow, ncol, nlyr)
  # -2356800, 2258400, 276800, 3172800  (xmin, xmax, ymin, ymax)
  
  # authoritative CONUS = grid
  .crs <- 'EPSG:5070'
  .grid <- terra::rast(nrows = 3620, ncols = 5769, crs = .crs, 
                       extent = terra::ext(-2356800, 2258400, 276800, 3172800))
  
  # compute BBOX / IMG geometry in native CRS
  wcs.geom <- .prepare_AEA_AOI(aoi, res = res, native_crs = .crs)
  
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
  base.url <- 'http://casoilresource.lawr.ucdavis.edu/cgi-bin/mapserv?'
  service.url <- 'map=/data1/website/wcs/issr800.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage'
  
  # unpack BBOX for WCS 2.0
  xmin <- wcs.geom$bbox[1]
  ymin <- wcs.geom$bbox[2]
  
  # xmax and ymax are now calculated from AOI dimensions and resolution
  # xmax <- wcs.geom$bbox[3]
  # ymax <- wcs.geom$bbox[4]
  
  # recalculate x/ymax based on xmin + resolution multiplied by AOI dims
  xmax2 <- xmin + res * wcs.geom$width
  ymax2 <- ymin + res * wcs.geom$height
  
  ## TODO: source data are LZW compressed, does it make sense to alter the compression (e.g. Deflate) for delivery?
  
  # compile WCS 2.0 style URL
  u <- paste0(
    base.url,
    service.url,
    '&COVERAGEID=', var.spec$dsn,
    '&FORMAT=image/tiff',
    '&GEOTIFF:COMPRESSION=LZW',
    '&SUBSETTINGCRS=EPSG:5070',
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
  
  # this may fail if terra installation is missing proj.db
  # init SpatRaster
  r <- try(terra::rast(tf), silent = TRUE)
  
  if (inherits(r, 'try-error')) {
    message(attr(r, 'condition'))
    stop('result is not a valid GeoTIFF', call. = FALSE)
  }
  
  ## NOTE: terra (as of 1.6-28) will create 0-indexed grids sometimes when
  ##       values(x) <- function_that_returns_factors(...)
  ##
  ## soil texture grids remain indexed from 1 for this reason
  ##
  
  ## TODO: this isn't quite right... '0' is returned by the WCS sometimes
  # specification of NODATA using local definitions
  # NAvalue(r) <- var.spec$na
  terra::NAflag(r) <- 0
  
  # load all values into memory
  terra::values(r) <- terra::values(r)
 
  # remove tempfile 
  unlink(tf)
  
  # set layer name in object
  names(r) <- var.spec$desc
  
  # test for all NA
  # if TRUE, we cannot process RATs
  .allNA <- terra::global(r, fun = "isNA")$isNA == terra::ncell(r)
  
  # message when all cells are NA
  if(.allNA) {
    message('all cells are NA')
  }
  
  # optional processing of RAT
  if (!is.null(var.spec$rat) && !.allNA) {
    
    # get rat
    rat <- try(suppressWarnings(read.csv(var.spec$rat, stringsAsFactors = FALSE)), silent = TRUE)
    
    # trap missing RAT
    if (inherits(rat, 'try-error')) {
      message("\nFailed to download RAT from ", var.spec$rat, "; returning integer grid")
      return(r)
    } else {
      # the cell value / ID column is always the 2nd column
      # name it for reference later
      names(rat)[2] <- 'ID'
      
      # re-order columns by name
      # there may be > 2 columns (hex colors, etc.)
      col.names <- c('ID', names(rat)[-2])
      
      # make categories + RAT
      r <- terra::as.factor(r)
      r.rat <- terra::cats(r)[[1]]
      
      # merge basic RAT + ISSR800 RAT
      rat <- merge(r.rat[, 'ID', drop = FALSE], rat, by = 'ID', all.x = TRUE, sort = FALSE)
      
      # register RAT
      levels(r) <- rat
      
      # set color table if there is a column named 'hex'
      if('hex' %in% names(rat)) {
        terra::coltab(r) <- rat[, c('ID', 'hex')]
      }
      
    }
  }
  
  # align to authoritative grid
  terra::ext(r) <- terra::align(terra::ext(r), .grid)
  
  input_class <- attr(wcs.geom, '.input_class')
  
  if ((!is.null(input_class) && input_class == "raster") ||
      getOption('soilDB.return_Spatial', default = FALSE) && 
      requireNamespace("raster")) {
    r <- raster::raster(r)
    
    # return without setting metadata via terra methods
    return(r)
  }
  
  # set metadata
  terra::metags(r) <- c(description = var.spec$desc, vintage = .vintage)
  
  return(r)
}




