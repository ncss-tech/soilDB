

soilColor.wcs <- function(aoi, var, res = 270, quiet = FALSE) {
  
  # sanity check: aoi specification
  if (!inherits(aoi, c('list', 'Spatial', 'sf', 'sfc', 'bbox', 'RasterLayer', 'SpatRaster', 'SpatVector'))) { 
    stop('invalid `aoi` specification', call. = FALSE)
  }
  
  # reasonable resolution
  if (res < 30 || res > 1000) {
    stop('`res` should be within 30 <= res <= 1000 meters')
  }
  
  # match variable name in catalog
  var.cat <- sapply(.soilColor.spec, '[[', 'dsn')
  var <- match.arg(tolower(var), choices = var.cat)
  
  # get variable specs
  var.spec <- .soilColor.spec[[var]]
  
  # compute BBOX / IMG geometry in native CRS
  wcs.geom <- .prepare_AEA_AOI(aoi, res = res)
  
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
  base.url <- 'http://soilmap2-1.lawr.ucdavis.edu/cgi-bin/mapserv?'
  service.url <- 'map=/soilmap2/website/wcs/color.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage'
  
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
  
  # load pointer to file and return
  r <- try(terra::rast(tf), silent = TRUE)
  
  if (inherits(r, 'try-error')) {
    message(attr(r, 'condition'))
    stop('result is not a valid GeoTIFF', call. = FALSE)
  }
  
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
  
  # and as an attribute
  attr(r, 'layer name') <- var.spec$desc
  
  # optional processing of RAT
  if (!is.null(var.spec$rat)) {
    
    # get rat
    rat <- read.csv(var.spec$rat, stringsAsFactors = FALSE)
    
    # the cell value / ID column is always the 2nd colum
    # name it for reference later
    names(rat)[1] <- 'ID'
    
    ## TODO: changes since previous version
    ##         * the raster-based version set only existing levels
    ##         * there may be more than ID, code in the RAT: see texture RATS
    ##         * very large RATs with mostly un-used levels (series name) will be a problem
    
    # re-order columns by name
    # there may be > 2 columns (hex colors, etc.)
    col.names <- c('ID', names(rat)[-1])
    
    # unique cell values
    u.values <- terra::unique(r)[[1]]
    
    # index those categories present in r
    cat.idx <- which(rat$ID %in% u.values)
    
    # subset RAT
    rat <- rat[cat.idx, col.names]
    
    # create hex notation for color
    rat$col <- rgb(rat$r, rat$g, rat$b, maxColorValue = 255)
    
    # register categories in new order
    levels(r) <- rat
  }
  
  input_class <- attr(wcs.geom, '.input_class')
  
  if ((!is.null(input_class) && input_class == "raster") ||
      getOption('soilDB.return_Spatial', default = FALSE)) {
    if (requireNamespace("raster")) {
      r <- raster::raster(r)
    }
  }
  
  return(r)
}

