

#' @title Get Map Unit Key (`mukey`) grid from SoilWeb Web Coverage Service (WCS)
#' 
#' @description Download chunks of the gNATSGO, gSSURGO, OCONUS SSURGO, STATSGO2, and RSS map unit key grid via bounding-box from the SoilWeb WCS.
#' 
#' @author D.E. Beaudette and A.G. Brown
#'
#' @param aoi area of interest (AOI) defined using a terra pakcage objects (`SpatRaster`, `SpatVector`, `ext`), sf objects (`sf`, `sfc`), raster package object (`RasterLayer`, `bbox`), sp package `Spatial*`, or a `list`, see details
#' 
#' @param db character, mukey grid source, see details
#' 
#' @param res grid resolution, leave as NULL to use native grid resolution, see details
#' 
#' @param quiet logical, passed to `curl::curl_download()` to enable / suppress URL and progress bar for download.
#'  
#' @details When specified as a `list`, `aoi` should contain:
#' 
#'   * `aoi`: bounding-box specified as (xmin, ymin, xmax, ymax) e.g. c(-114.16, 47.65, -114.08, 47.68)
#'   * `crs`: coordinate reference system of BBOX, e.g. 'OGC:CRS84' (EPSG:4326, WGS84 Longitude/Latitude)
#'
#' The WCS query is parameterized using a rectangular extent derived from the above AOI specification, after conversion to the native CRS (EPSG:5070) of the WCS grids.
#' 
#' Databases available from this WCS can be queried using `WCS_details(wcs = 'mukey')`.
#' 
#' |db        |crs        |description                   |
#' |:---------|:----------|:-----------------------------|
#'   |ak_ssurgo |EPSG:3338  |AK map unit keys              |
#'   |as_ssurgo |EPSG:4326  |AS map unit keys              |
#'   |fssurgo   |EPSG:5070  |SSURGO/STATSGO2 map unit keys |
#'   |gnatsgo   |EPSG:5070  |gNATSGO map unit keys         |
#'   |gssurgo   |EPSG:5070  |gSSURGO map unit keys         |
#'   |gu_ssurgo |EPSG:4326  |GU map unit keys              |
#'   |hi_ssurgo |EPSG:6628  |HI map unit keys              |
#'   |mp_ssurgo |EPSG:4326  |MP map unit keys              |
#'   |pr_ssurgo |EPSG:32161 |PR map unit keys              |
#'   |pw_ssurgo |EPSG:4326  |PW map unit keys              |
#'   |rss       |EPSG:5070  |RSS map unit keys             |
#'   |statsgo   |EPSG:5070  |STATSGO2 map unit keys        |
#'   
#' 
#' The `fSSURGO` database is an unofficial hybrid of gSSURGO, back-filled with STATSGO2 data where SSURGO data are missing (e.b. denied access, NOTCOM, large misc. areas other than water). 
#' 
#' The RSS mukey grid is 10m resolution. CONUS, AK, HI, and PR mukey grids are 30m resolution. AS, GU, MP, and PW use a geographic coordinate system with a grid size of approximately 30m.
#' 
#' 
#' @return A `SpatRaster` (or `RasterLayer`) object containing indexed map unit keys and associated raster attribute table, or a `try-error` if the WCS request fails. Basic metadata are encoded into the resulting `SpatRaster`, accessible via `terra::metags()`.
#'
#' @export
#' @examples 
#' \dontrun{
#' library(terra)
#' 
#' aoi <- list(aoi = c(-116.7400, 35.2904, -116.7072, 35.3026), crs = "EPSG:4326")
#' res <- mukey.wcs(aoi, db = 'gNATSGO') 
#'   
#' m <- unique(values(res))
#' 
#' prp <- setNames(
#'   get_SDA_property(
#'     c("ph1to1h2o_r", "claytotal_r"),
#'     "weighted average",
#'     mukeys = m,
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
#' terra::plot(res2[['pH1to1_0to25']])
#' }
mukey.wcs <- function(
    aoi, 
    db = c('gNATSGO', 'gSSURGO', 'RSS', 'STATSGO', 'fSSURGO', 'PR_SSURGO', 'HI_SSURGO'), 
    res = NULL, 
    quiet = FALSE
) {
  
  # all EPSG:5070
  .conusGrids <- c('gnatsgo', 'gssurgo', 'fssurgo', 'rss', 'statsgo')
  
  # outside of CONUS, several CRS
  .oconusGrids <- c('ak_ssurgo', 'hi_ssurgo', 'pr_ssurgo', 'pw_ssurgo', 'gu_ssurgo', 'as_ssurgo', 'mp_ssurgo')
  
  # sanity check: db name
  db <- match.arg(tolower(db[1]), choices = c(.conusGrids, .oconusGrids))
  
  if (!requireNamespace("terra")) {
    stop("package 'terra' is required", call. = FALSE)
  }
  
  # 
  # 
  # # lookup native CRS
  # if (db %in% .conusGrids) {
  #   # CONUS
  #   .crs <- 'EPSG:5070'
  #   .grid <- terra::rast(nrows = 97053, ncols = 153996, crs = .crs, 
  #                        extent = terra::ext(-2356125, 2263755, 260985, 3172575))
  # } else if (db == 'pr_ssurgo') {
  #   # PR
  #   .crs <- 'EPSG:32161'
  #   .grid <- terra::rast(nrows = 2229, ncols = 9608, crs = .crs, 
  #                        extent = terra::ext(39905, 328145, 208815, 275685))
  # } else if (db == 'hi_ssurgo') {
  #   # HI
  #   .crs <- 'EPSG:6628'
  #   .grid <- terra::rast(nrows = 12441, ncols = 17193, crs = .crs, 
  #                        extent = terra::ext(56992, 572782, 8585, 381815))
  # }
  
  # sanity check: aoi specification
  if (!inherits(aoi, c('list', 'Spatial', 'sf', 'sfc', 'bbox', 'RasterLayer', 'SpatRaster', 'SpatVector'))) { 
    stop('invalid `aoi` specification', call. = FALSE)
  }
  
  # prepare WCS details
  # list-lookup is lower-case
  var.spec <- .mukey.spec[[db]]
  
  # ideally, use the default resolution for each coverage
  if (is.null(res)) {
    res <- var.spec$res
  } else {
    # check for reasonable resolutions
    
    # all CONUS grids are 30m resolution
    if (db %in% .conusGrids && (res < 30 || res > 3000)) {
      stop('`res` should be within 30 <= res <= 3000 meters')
    }
    
    # TODO: OCONUS grids use a mixture of projected and geographic CRS
  }
  
  
  
  # prepare AOI in native CRS
  wcs.geom <- .prepare_AOI(obj = aoi, res = res, native_crs = var.spec$crs)
  
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
  base.url <- 'http://casoilresource.lawr.ucdavis.edu/cgi-bin/mapserv?'
  service.url <- 'map=/data1/website/wcs/mukey-grids.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage'
  
  # unpack BBOX for WCS 2.0
  xmin <- wcs.geom$bbox[1]
  ymin <- wcs.geom$bbox[2]
  
  # recalculate x/ymax based on xmin + resolution multiplied by AOI dims
  xmax2 <- xmin + res * wcs.geom$width[[1]]
  ymax2 <- ymin + res * wcs.geom$height[[1]]
  
  # WCS notes:
  # https://github.com/geographika/wcs-test
  
  # compile WCS 2.0 style URL
  u <- paste0(
    base.url,
    service.url,
    '&COVERAGEID=', var.spec$dsn,
    '&FORMAT=image/tiff',
    '&GEOTIFF:COMPRESSION=DEFLATE',
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
  
  if (!quiet) cat("\n")
  
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
  
  ## 2026-04-15: is this neccessary?

  test_y <- round((ymax2 - ymin) / res) != nrow(r)
  test_x <- round((xmax2 - xmin) / res) != ncol(r)

  # fix requested vs. received grid dimensions
  if (test_x || test_y) {
    if (test_x)
      message("Request partially outside boundary of coverage source data: expected ",
              (xmax2 - xmin) / res, " columns, received ", ncol(r))
    if (test_y)
      message("Request partially outside boundary of coverage source data: expected ",
              (ymax2 - ymin) / res, " rows, received ", nrow(r))

    # fix extent of result due to rounding error/incomplete pixels at edges of map
    rex <- terra::ext(r)
    terra::ext(r) <- terra::ext(c(rex[1], rex[1] + ncol(r) * res, rex[3], rex[3] + nrow(r) * res))

    # extend to input extent
    r <- terra::extend(r, terra::ext(c(xmin, xmax2, ymin, ymax2)))
  }
  
  # read into memory
  terra::values(r) <- terra::values(r)
  
  # remove tempfile 
  unlink(tf)
  
  # set layer name in object
  names(r) <- 'mukey'
  
  # build RAT
  r <- terra::as.factor(r)
  
  input_class <- attr(wcs.geom, '.input_class')
  
  # align with official grid system
  .grid <- terra::rast(
    nrows = var.spec$grid$dim[1], 
    ncols = var.spec$grid$dim[2], 
    crs = var.spec$crs,
    extent = terra::ext(var.spec$grid$ext)
  )
  
  terra::ext(r) <- terra::align(terra::ext(r), .grid)
  
  
  if ((!is.null(input_class) && input_class == "raster") ||
      getOption('soilDB.return_Spatial', default = FALSE) &&
      (requireNamespace("raster"))) {
    r <- raster::raster(r)
    
    # return without setting metadata via terra methods
    return(r)
  }
  
  # set metadata
  terra::metags(r) <- c(description = var.spec$desc, vintage = var.spec$vintage)
  
  return(r)
}

