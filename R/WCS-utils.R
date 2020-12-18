
## TODO: let Mapserver compute image dimensions from BBOX, RESX, RESY

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
  
  ## TODO: there must be a better way to ensure the target resolution
  
  # create BBOX used for WMS
  # xmin, ymin, xmax, ymax
  aoi.native <- round(c(e.native[1], e.native[3], e.native[2], e.native[4]))
  
  ## TODO: these aren't required if we specify the resolution in WCS request
  
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
  ),
  
  'str' = list(
    dsn = 'str',
    type = 'GEOTIFF_BYTE',
    desc = 'Soil Temperature Regime',
    rat = 'https://soilmap2-1.lawr.ucdavis.edu/800m_grids/RAT/str.csv'
  )
  
  
)


.mukey.spec <- list(
  
  'gnatsgo' = list(
    dsn = 'gnatsgo',
    type = 'GEOTIFF_FLOAT',
    desc = 'gnatsgo map unit keys',
    rat = NULL
  )
)
