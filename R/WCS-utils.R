

#' @title Web Coverage Services Details
#' 
#' @description List variables or databases provided by soilDB web coverage service (WCS) abstraction. These lists will be expanded in future versions.
#'
#' @param wcs a WCS label ('mukey' or 'ISSR800')
#'
#' @return a \code{data.frame}
#' @export
#'
#' @examples
#' 
#' WCS_details(wcs = 'ISSR800')
WCS_details <- function(wcs = c('mukey', 'ISSR800')) {
  
  # select a WCS
  wcs <- match.arg(wcs)
  
  spec <- switch(wcs,
                 'mukey' = .mukey.spec,
                 'ISSR800' = .ISSR800.spec
                 )
  
  ## TODO: spec names / DSN are assumed to be identical
  ##       it would be nice to allow for more connotative names
  # all(names(spec) == sapply(spec, '[[', 'dsn'))
  
  # extract relevant details
  v <- lapply(spec, function(i) {
    data.frame(
      var = i[['dsn']],
      description = i[['desc']],
      stringsAsFactors = FALSE
    )
  })
  
  # flatten / re-format
  v <- do.call('rbind', v)
  row.names(v) <- as.character(1:nrow(v))
  
  # alpha order simpler to use
  v <- v[order(v[['var']]), ]
  
  # ugh, mukey.wcs() uses 'db' vs 'var 
  if(wcs == 'mukey') {
    names(v)[1] <- 'db'
  }
  
  return(v)
}



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
    
  } else if(inherits(obj, 'RasterLayer'))  {
    
    p <- as(raster::extent(obj), 'SpatialPolygons')
    proj4string(p) <- raster::crs(obj)
      
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
  # EPSG:6350 NAD83 (2011)
  # EPSG:5070 NAD83
  # we will use EPSG:5070 for now (https://github.com/ncss-tech/soilDB/issues/205)
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


## TODO: next release include suggested colors

# ISSR-800 layers and basic metadata
.ISSR800.spec <- list(
  
  'statsgo_pct' = list(
    dsn = 'statsgo_pct',
    type = 'GEOTIFF_FLOAT',
    desc = 'STATSGO data available, fraction of 800x800m grid cell',
    na = -9999,
    rat = NULL
  ),
  
  'ssurgo_pct' = list(
    dsn = 'ssurgo_pct',
    type = 'GEOTIFF_FLOAT',
    desc = 'SSURGO data available, fraction of 800x800m grid cell',
    na = -9999,
    rat = NULL
  ),
  
  'om_kg_sq_m' = list(
    dsn = 'om_kg_sq_m',
    type = 'GEOTIFF_FLOAT',
    desc = 'Total Soil Organic Matter (kg/m^2)',
    na = -9999,
    rat = NULL
  ),
  
  'caco3_kg_sq_m' = list(
    dsn = 'caco3_kg_sq_m',
    type = 'GEOTIFF_FLOAT',
    desc = 'Total CaCO3 (kg/m^2)',
    na = -9999,
    rat = NULL
  ),
  
  'ec_05cm' = list(
    dsn = 'ec_05cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'EC 0-5cm depth (dS/m)',
    na = -9999,
    rat = NULL
  ),
  
  'ec_025cm' = list(
    dsn = 'ec_025cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'EC 0-25cm depth (dS/m)',
    na = -9999,
    rat = NULL
  ),
  
  'cec_05cm' = list(
    dsn = 'cec_05cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'CEC at pH 7 0-5cm depth (cmol[+]/kg)',
    na = -9999,
    rat = NULL
  ),
  
  'cec_025cm' = list(
    dsn = 'cec_025cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'CEC at pH 7 0-25cm depth (cmol[+]/kg)',
    na = -9999,
    rat = NULL
  ),
  
  'cec_050cm' = list(
    dsn = 'cec_050cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'CEC at pH 7 0-50cm depth (cmol[+]/kg)',
    na = -9999,
    rat = NULL
  ),
  
  'paws' = list(
    dsn = 'paws',
    type = 'GEOTIFF_FLOAT',
    desc = 'total plant available water storage (cm water)',
    na = -9999,
    rat = NULL
  ),
  
  'paws_050cm' = list(
    dsn = 'paws_050cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'plant available water storage 0-50cm depth (cm water)',
    na = -9999,
    rat = NULL
  ),
  
  'paws_025cm' = list(
    dsn = 'paws_025cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'plant available water storage 0-25cm depth (cm water)',
    na = -9999,
    rat = NULL
  ),
  
  'ph_05cm' = list(
    dsn = 'ph_05cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'pH 1:1 H2O 0-5cm depth',
    na = -9999,
    rat = NULL
  ),

  'ph_025cm' = list(
    dsn = 'ph_025cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'pH 1:1 H2O 0-25cm depth',
    na = -9999,
    rat = NULL
  ),

  'ph_2550cm' = list(
    dsn = 'ph_2550cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'pH 1:1 H2O 25-50cm depth',
    na = -9999,
    rat = NULL
  ),

  'ph_3060cm' = list(
    dsn = 'ph_3060cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'pH 1:1 H2O 30-60cm depth',
    na = -9999,
    rat = NULL
  ),

  'clay_05cm' = list(
    dsn = 'clay_05cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'clay percent 0-5cm depth',
    na = -9999,
    rat = NULL
  ),

  'clay_025cm' = list(
    dsn = 'clay_025cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'clay percent 0-25cm depth',
    na = -9999,
    rat = NULL
  ),

  'clay_2550cm' = list(
    dsn = 'clay_2550cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'clay percent 25-50cm depth',
    na = -9999,
    rat = NULL
  ),

  'clay_3060cm' = list(
    dsn = 'clay_3060cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'clay percent 30-60cm depth',
    na = -9999,
    rat = NULL
  ),

  'sand_05cm' = list(
    dsn = 'sand_05cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'sand percent 0-5cm depth',
    na = -9999,
    rat = NULL
  ),

  'sand_025cm' = list(
    dsn = 'sand_025cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'sand percent 0-25cm depth',
    na = -9999,
    rat = NULL
  ),

  'sand_2550cm' = list(
    dsn = 'sand_2550cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'sand percent 25-50cm depth',
    na = -9999,
    rat = NULL
  ),

  'sand_3060cm' = list(
    dsn = 'sand_3060cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'sand percent 30-60cm depth',
    na = -9999,
    rat = NULL
  ),

  'silt_05cm' = list(
    dsn = 'silt_05cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'silt percent 0-5cm depth',
    na = -9999,
    rat = NULL
  ),

  'silt_025cm' = list(
    dsn = 'silt_025cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'silt percent 0-25cm depth',
    na = -9999,
    rat = NULL
  ),

  'silt_2550cm' = list(
    dsn = 'silt_2550cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'silt percent 25-50cm depth',
    na = -9999,
    rat = NULL
  ),

  'silt_3060cm' = list(
    dsn = 'silt_3060cm',
    type = 'GEOTIFF_FLOAT',
    desc = 'silt percent 30-60cm depth',
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
  
  'series_name' = list(
    dsn = 'series_name',
    type = 'GEOTIFF_16',
    desc = 'Soil Series Name',
    na = 0,
    rat = 'http://soilmap2-1.lawr.ucdavis.edu/800m_grids/RAT/series_name.csv'
  ),
  
  'hydgrp' = list(
    dsn = 'hydgrp',
    type = 'GEOTIFF_BYTE',
    desc = 'Hydrologic Soil Group',
    na = 0,
    rat = 'http://soilmap2-1.lawr.ucdavis.edu/800m_grids/RAT/hydgrp.csv'
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
  ),
  
  'soilorder' = list(
    dsn = 'soilorder',
    type = 'GEOTIFF_BYTE',
    desc = 'Soil Taxonomy: Soil Order',
    na = 0,
    rat = 'http://soilmap2-1.lawr.ucdavis.edu/800m_grids/RAT/soilorder.csv'
  ),
  
  'suborder' = list(
    dsn = 'suborder',
    type = 'GEOTIFF_BYTE',
    desc = 'Soil Taxonomy: Suborder',
    na = 0,
    rat = 'http://soilmap2-1.lawr.ucdavis.edu/800m_grids/RAT/suborder.csv'
  ),
  
  'greatgroup' = list(
    dsn = 'greatgroup',
    type = 'GEOTIFF_BYTE',
    desc = 'Soil Taxonomy: Greatgroup',
    na = 0,
    rat = 'http://soilmap2-1.lawr.ucdavis.edu/800m_grids/RAT/greatgroup.csv'
  ),
  
  'texture_05cm' = list(
    dsn = 'texture_05cm',
    type = 'GEOTIFF_BYTE',
    desc = 'Soil Texture Class, 0-5cm',
    na = 0,
    rat = 'http://soilmap2-1.lawr.ucdavis.edu/800m_grids/RAT/texture_05.csv'
  ),
  
  'texture_025cm' = list(
    dsn = 'texture_025cm',
    type = 'GEOTIFF_BYTE',
    desc = 'Soil Texture Class, 0-25cm',
    na = 0,
    rat = 'http://soilmap2-1.lawr.ucdavis.edu/800m_grids/RAT/texture_025.csv'
  ),
  
  'texture_2550cm' = list(
    dsn = 'texture_2550cm',
    type = 'GEOTIFF_BYTE',
    desc = 'Soil Texture Class, 25-50cm',
    na = 0,
    rat = 'http://soilmap2-1.lawr.ucdavis.edu/800m_grids/RAT/texture_2550.csv'
  ),
  
  'lcc_irrigated' = list(
    dsn = 'lcc_irrigated',
    type = 'GEOTIFF_BYTE',
    desc = 'Land Capability Class, irrigated',
    na = 0,
    rat = 'http://soilmap2-1.lawr.ucdavis.edu/800m_grids/RAT/lcc.csv'
  ),
  
  'lcc_nonirrigated' = list(
    dsn = 'lcc_nonirrigated',
    type = 'GEOTIFF_BYTE',
    desc = 'Land Capability Class, non-irrigated',
    na = 0,
    rat = 'http://soilmap2-1.lawr.ucdavis.edu/800m_grids/RAT/lcc.csv'
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
