

# obj: list(aoi, crs) or Spatial*, sf, sfc, bbox object
# res: grid resolution in native CRS (meters) [ISSR-800: 800, gNATSGO: 30]
.prepare_AEA_AOI <- function(obj, res) {

  # convert AOI to SpatialPolygons object and assign CRS
  if(inherits(obj, c('sf', 'sfc', 'bbox'))) {

    if(!requireNamespace("sf"))
      stop("package 'sf' is required to use an sf, sfc or bbox object as an AOI", call. = FALSE)

    # create bbox around sf/sfc/bbox, create simple feature collection, cast to Spatial
    p <- sf::as_Spatial(sf::st_as_sfc(sf::st_bbox(obj)))
    rm(obj)

  } else if(inherits(obj, 'Spatial'))  {
    # note: the spatial inherits method will bug out with a bbox, so it must come second

    # re-name for simpler code
    p <- obj
    rm(obj)

  # explicitly check the presence of the two list elements, and length/type of aoi
  } else if (!is.null(obj$aoi) & !is.null(obj$crs) &
             is.numeric(obj$aoi) & (length(obj$aoi) == 4)) {
    # note that vector is re-arranged to form a legal extent: xmin, xmax, ymin, ymax
    # craft an extent object
    e <- extent(
      obj$aoi[1],
      obj$aoi[3],
      obj$aoi[2],
      obj$aoi[4]
      )

    # convert to BBOX polygon and set CRS
    p <- as(e, 'SpatialPolygons')
    proj4string(p) <- obj$crs

  } else {
   stop('invalid `aoi` specification', call. = FALSE)
  }

  # ISSR-800 and gNATSGO CRS
  # EPSG:6350
  crs <- '+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'

  # double-check, likely a better approach
  p <- suppressWarnings(spTransform(p, crs))

  # AOI and image calculations in native CRS
  e.native <- extent(p)

  # create BBOX used for WMS
  # xmin, ymin, xmax, ymax
  aoi.native <- round(c(e.native[1], e.native[3], e.native[2], e.native[4]))

  # these are useful for testing image dimensions > allowed image dimensions
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


# ISSR-800 layers and basic metadata
.ISSR800.spec <- list(

  'ph_05cm' = list(
    dsn = 'ph_05cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'pH 0-5cm depth',
    na = -9999,
    rat = NULL
  ),

  'ph_025cm' = list(
    dsn = 'ph_025cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'pH 5-25cm depth',
    na = -9999,
    rat = NULL
  ),

  'ph_2550cm' = list(
    dsn = 'ph_2550cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'pH 25-50cm depth',
    na = -9999,
    rat = NULL
  ),

  'ph_3060cm' = list(
    dsn = 'ph_3060cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'pH 30-60cm depth',
    na = -9999,
    rat = NULL
  ),

  'clay_05cm' = list(
    dsn = 'clay_05cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'clay 0-5cm depth',
    na = -9999,
    rat = NULL
  ),

  'clay_025cm' = list(
    dsn = 'clay_025cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'clay 5-25cm depth',
    na = -9999,
    rat = NULL
  ),

  'clay_2550cm' = list(
    dsn = 'clay_2550cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'clay 25-50cm depth',
    na = -9999,
    rat = NULL
  ),

  'clay_3060cm' = list(
    dsn = 'clay_3060cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'clay 30-60cm depth',
    na = -9999,
    rat = NULL
  ),

  'sand_05cm' = list(
    dsn = 'sand_05cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'sand 0-5cm depth',
    na = -9999,
    rat = NULL
  ),

  'sand_025cm' = list(
    dsn = 'sand_025cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'sand 5-25cm depth',
    na = -9999,
    rat = NULL
  ),

  'sand_2550cm' = list(
    dsn = 'sand_2550cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'sand 25-50cm depth',
    na = -9999,
    rat = NULL
  ),

  'sand_3060cm' = list(
    dsn = 'sand_3060cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'sand 30-60cm depth',
    na = -9999,
    rat = NULL
  ),

  'silt_05cm' = list(
    dsn = 'silt_05cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'silt 0-5cm depth',
    na = -9999,
    rat = NULL
  ),

  'silt_025cm' = list(
    dsn = 'silt_025cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'silt 5-25cm depth',
    na = -9999,
    rat = NULL
  ),

  'silt_2550cm' = list(
    dsn = 'silt_2550cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'silt 25-50cm depth',
    na = -9999,
    rat = NULL
  ),

  'silt_3060cm' = list(
    dsn = 'silt_3060cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'silt 30-60cm depth',
    na = -9999,
    rat = NULL
  ),

  'sar' = list(
    dsn = 'sar',
    type = 'GEOTIFF_FLOAT',
    desc = 'SAR, entire profile',
    na = -9999,
    rat = NULL
  ),

  'drainage_class' = list(
    dsn = 'drainage_class',
    type = 'GEOTIFF_BYTE',
    desc = 'Soil Drainage Class',
    na = 0,
    rat = 'http://soilmap2-1.lawr.ucdavis.edu/800m_grids/RAT/drainage_class.csv'
  ),

  'weg' = list(
    dsn = 'weg',
    type = 'GEOTIFF_BYTE',
    desc = 'Wind Erodibility Group',
    na = 0,
    rat = 'http://soilmap2-1.lawr.ucdavis.edu/800m_grids/RAT/weg.csv'
  ),

  'wei' = list(
    dsn = 'wei',
    type = 'GEOTIFF_16',
    na = 0,
    desc = 'Wind Erodibility Index',
    rat = NULL
  ),

  'str' = list(
    dsn = 'str',
    type = 'GEOTIFF_BYTE',
    desc = 'Soil Temperature Regime',
    na = 0,
    rat = 'http://soilmap2-1.lawr.ucdavis.edu/800m_grids/RAT/str.csv'
  )


)


.mukey.spec <- list(

  'gnatsgo' = list(
    dsn = 'gnatsgo',
    type = 'GEOTIFF_FLOAT',
    desc = 'gNATSGO map unit keys',
    na = 2147483647,
    rat = NULL
  ),

  'gssurgo' = list(
    dsn = 'gssurgo',
    type = 'GEOTIFF_FLOAT',
    desc = 'gSSURGO map unit keys',
    na = 2147483647,
    rat = NULL
  )
)
