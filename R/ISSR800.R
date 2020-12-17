.ISSR800.spec <- list(
  
  'ph_05cm' = list(
    dsn = 'ph_05cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'pH 0-5cm depth',
    rat = NULL
  ),
  
  'ph_025cm' = list(
    dsn = 'ph_025cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'pH 5-25cm depth',
    rat = NULL
  ),
  
  'ph_2550cm' = list(
    dsn = 'ph_2550cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'pH 25-50cm depth',
    rat = NULL
  ),
  
  'ph_3060cm' = list(
    dsn = 'ph_3060cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'pH 30-60cm depth',
    rat = NULL
  ),
  
  'clay_05cm' = list(
    dsn = 'clay_05cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'clay 0-5cm depth',
    rat = NULL
  ),
  
  'clay_025cm' = list(
    dsn = 'clay_025cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'clay 5-25cm depth',
    rat = NULL
  ),
  
  'clay_2550cm' = list(
    dsn = 'clay_2550cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'clay 25-50cm depth',
    rat = NULL
  ),
  
  'clay_3060cm' = list(
    dsn = 'clay_3060cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'clay 30-60cm depth',
    rat = NULL
  ),
  

  
  'drainage_class' = list(
    dsn = 'drainage_class',
    type = 'GEOTIFF_BYTE',
    desc = 'Soil Drainage Class',
    rat = 'https://soilmap2-1.lawr.ucdavis.edu/800m_grids/RAT/drainage_class.csv'
  ),
  
  'weg' = list(
    dsn = 'weg',
    type = 'GEOTIFF_BYTE',
    desc = 'Wind Erodibility Group',
    rat = 'https://soilmap2-1.lawr.ucdavis.edu/800m_grids/RAT/weg.csv'
  )
  
  
)



ISSR800.wcs <- function(var, aoi, res = 800, crs = 'EPSG:6350') {
  
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





# aoi.wgs84: BBOX in WGS84 GCS ~ c(-121,37,-120,38)
# res: grid resolution in native CRS (meters) [ISSR-800: 800, gNATSGO: 30]
.prepare_AEA_AOI_fromWGS84 <- function(aoi.wgs84, res, targetCRS) {
  # manage extent
  # note that BBOX is re-arranged to form a legal extent: xmin, xmax, ymin, ymax
  e <- extent(aoi.wgs84[1], aoi.wgs84[3], aoi.wgs84[2], aoi.wgs84[4])
  
  # convert to BBOX polygon and set CRS
  p <- as(e, 'SpatialPolygons')
  proj4string(p) <- '+proj=longlat +datum=WGS84'
  
  # ISSR-800 and gNATSGO CRS
  # crs <- '+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'
  crs <- paste0('+init=', targetCRS)
  
  # double-check, likely a better approach
  p <- suppressWarnings(spTransform(p, crs))
  
  # AOI and image calculations in native CRS
  e.native <- extent(p)
  
  # create BBOX used for WMS
  # xmin, ymin, xmax, ymax
  aoi.native <- round(c(e.native[1], e.native[3], e.native[2], e.native[4]))
  
  # xmax - xmin
  w <- round(abs(e.native[2] - e.native[1]) / res)
  # ymax - ymin
  h <- round(abs(e.native[4] - e.native[3]) / res)
  
  return(
    list(
      bbox = aoi.native,
      width = w,
      height = h
    )
  )
}
