#' @title Web Coverage Services Details
#' 
#' @description List variables or databases provided by soilDB web coverage service (WCS) abstraction. These lists will be expanded in future versions.
#'
#' @param wcs a WCS label ('mukey', 'ISSR800', or 'soilColor')
#'
#' @return a `data.frame`
#' @export
#'
#' @examples
#' 
#' WCS_details(wcs = 'ISSR800')
WCS_details <- function(wcs = c('mukey', 'ISSR800', 'soilColor')) {
  
  ## TODO: add md5sum + vintage
  
  # select a WCS
  wcs <- match.arg(wcs)
  
  spec <- switch(wcs,
                 'mukey' = .mukey.spec,
                 'ISSR800' = .ISSR800.spec,
                 'soilColor' = .soilColor.spec
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
  row.names(v) <- as.character(seq_len(nrow(v)))
  
  # alpha order simpler to use
  v <- v[order(v[['var']]), ]
  
  # ugh, mukey.wcs() uses 'db' vs 'var 
  if (wcs == 'mukey') {
    names(v)[1] <- 'db'
  }
  
  return(v)
}

# obj: list(aoi, crs) or Spatial*, sf, sfc, bbox object
# res: grid resolution in native CRS (meters) [ISSR-800: 800, gNATSGO: 30]
# native_crs: native CRS, usually EPSG:5070
#             outside of CONUS will be different
.prepare_AEA_AOI <- function(obj, res, native_crs = 'EPSG:5070') {
  
  return_class <- 'terra'
  
  # pre-processing of classes to terra/sf
  if (inherits(obj, 'RasterLayer') ||
      inherits(obj, 'RasterStack') ||
      inherits(obj, 'RasterBrick'))  {
    
    if (!requireNamespace("terra"))
      stop("package terra is required to convert RasterLayer objects to an AOI", call. = FALSE)
    
    obj <- terra::rast(obj)
    return_class <- 'raster'
    
  } else if (inherits(obj, 'bbox')) {
    
    # note: the  inherits(, 'Spatial') will bug out with a bbox, so it must come after
    # do nothing
    
  } else if (inherits(obj, 'Spatial'))  {
    
    if (!requireNamespace("sf"))
      stop("package sf is required to convert Spatial objects to an AOI", call. = FALSE)
    
    # convert to sf
    obj <- sf::st_as_sf(obj)
    return_class <- 'raster'
    
  }
  
  # convert AOI to sf/terra object and assign CRS
  if (inherits(obj, 'SpatRaster') || inherits(obj, 'SpatVector'))  {
    
    if (!requireNamespace("terra"))
      stop("package terra is required", call. = FALSE)
    
    p <- terra::as.polygons(terra::ext(obj), crs = terra::crs(obj))
    
  } else {
    
    if (!requireNamespace("sf"))
      stop("package 'sf' is required to use an sf, sfc or bbox object as an AOI", call. = FALSE)
    
    if (inherits(obj, c('sf', 'sfc', 'bbox'))) {
      
      # create bbox around sf/sfc/bbox, create simple feature collection
      p <- sf::st_as_sfc(sf::st_bbox(obj))
      rm(obj)
      
      # explicitly check the presence of the two list elements, and length/type of aoi
    } else if (!is.null(obj$aoi) && !is.null(obj$crs) &&
               is.numeric(obj$aoi) && (length(obj$aoi) == 4)) {
      
      p <- sf::st_as_sf(wk::rct(
        xmin = obj$aoi[1],
        ymin = obj$aoi[2],
        xmax = obj$aoi[3],
        ymax = obj$aoi[4],
        crs = obj$crs
      ))
    } else {
      stop('invalid `aoi` specification', call. = FALSE)
    }
  }
  
  
  # CONUS grids: ISSR800, gNATSGO, gSSURGO, STATSGO, RSS
  # 
  # NOTE: EPSG:6350 NAD83 (2011) v.s. EPSG:5070 NAD83
  # we use EPSG:5070 (https://github.com/ncss-tech/soilDB/issues/205)
  # NOTE: +init=epsg:XXXX syntax is deprecated in GDAL. It might return a CRS with a non-EPSG compliant axis order.
  
  
  
  # transform bounding polygon to WCS CRS
  # could be either, 
  # st_bbox is commonly converted to 'sfc'
  if (inherits(p, 'sfc') || inherits(p, 'sf')) {
    p <- sf::st_transform(p, crs = native_crs)
    e.native <- sf::st_bbox(p)
    
    # AOI and image calculations in native CRS
    # create BBOX used for WMS
    # xmin, ymin, xmax, ymax
    aoi.native <- e.native
    
    
  } else if (inherits(p, 'SpatVector')) {
    p <- terra::project(p, native_crs)
    e.native <- terra::ext(p)
    
    # AOI and image calculations in native CRS
    # create BBOX used for WMS
    # xmin, ymin, xmax, ymax
    aoi.native <- e.native[c(1,3,2,4)]
  }
  
  # these are used for calculating xmax/ymax for WCS request
  # xmax - xmin
  w <- round(abs(aoi.native[3] - aoi.native[1]) / res)
  # ymax - ymin
  h <- round(abs(aoi.native[4] - aoi.native[2]) / res)
  
  res <- list(bbox = aoi.native,
              width = w,
              height = h)
  attr(res, '.input_class') <- return_class
  res
}


## moist soil colors
# these are 16bit (unsigned) integers
# TODO: consider moving soil color LUT to wcs-files
# these maps all share the same RAT
.soilColorRAT <- 'http://casoilresource.lawr.ucdavis.edu/wcs/soilcolor/unique-moist-color-LUT.csv'
.soilColor.spec <- list(
  
  'sc005cm' = list(
    dsn = 'sc005cm',
    type = 'GEOTIFF_16',
    desc = 'Moist soil color, 5cm (270m)',
    na = 0,
    rat = .soilColorRAT
  ),
  
  'sc010cm' = list(
    dsn = 'sc010cm',
    type = 'GEOTIFF_16',
    desc = 'Moist soil color, 10cm (270m)',
    na = 0,
    rat = .soilColorRAT
  ),
  
  'sc015cm' = list(
    dsn = 'sc015cm',
    type = 'GEOTIFF_16',
    desc = 'Moist soil color, 15cm (270m)',
    na = 0,
    rat = .soilColorRAT
  ),
  
  'sc025cm' = list(
    dsn = 'sc025cm',
    type = 'GEOTIFF_16',
    desc = 'Moist soil color, 25cm (270m)',
    na = 0,
    rat = .soilColorRAT
  ),
  
  'sc050cm' = list(
    dsn = 'sc050cm',
    type = 'GEOTIFF_16',
    desc = 'Moist soil color, 50cm (270m)',
    na = 0,
    rat = .soilColorRAT
  ),
  
  'sc075cm' = list(
    dsn = 'sc075cm',
    type = 'GEOTIFF_16',
    desc = 'Moist soil color, 75cm (270m)',
    na = 0,
    rat = .soilColorRAT
  ),
  
  'sc100cm' = list(
    dsn = 'sc100cm',
    type = 'GEOTIFF_16',
    desc = 'Moist soil color, 100cm (270m)',
    na = 0,
    rat = .soilColorRAT
  ),
  
  'sc125cm' = list(
    dsn = 'sc125cm',
    type = 'GEOTIFF_16',
    desc = 'Moist soil color, 125cm (270m)',
    na = 0,
    rat = .soilColorRAT
  ),
  
  
  'sc005cm_hr' = list(
    dsn = 'sc005cm_hr',
    type = 'GEOTIFF_16',
    desc = 'Moist soil color, 5cm (30m)',
    na = 0,
    rat = .soilColorRAT
  ),
  
  'sc010cm_hr' = list(
    dsn = 'sc010cm_hr',
    type = 'GEOTIFF_16',
    desc = 'Moist soil color, 10cm (30m)',
    na = 0,
    rat = .soilColorRAT
  ),
  
  'sc015cm_hr' = list(
    dsn = 'sc015cm_hr',
    type = 'GEOTIFF_16',
    desc = 'Moist soil color, 15cm (30m)',
    na = 0,
    rat = .soilColorRAT
  ),
  
  'sc025cm_hr' = list(
    dsn = 'sc025cm_hr',
    type = 'GEOTIFF_16',
    desc = 'Moist soil color, 25cm (30m)',
    na = 0,
    rat = .soilColorRAT
  ),
  
  'sc050cm_hr' = list(
    dsn = 'sc050cm_hr',
    type = 'GEOTIFF_16',
    desc = 'Moist soil color, 50cm (30m)',
    na = 0,
    rat = .soilColorRAT
  ),
  
  'sc075cm_hr' = list(
    dsn = 'sc075cm_hr',
    type = 'GEOTIFF_16',
    desc = 'Moist soil color, 75cm (30m)',
    na = 0,
    rat = .soilColorRAT
  ),
  
  'sc100cm_hr' = list(
    dsn = 'sc100cm_hr',
    type = 'GEOTIFF_16',
    desc = 'Moist soil color, 100cm (30m)',
    na = 0,
    rat = .soilColorRAT
  ),
  
  'sc125cm_hr' = list(
    dsn = 'sc125cm_hr',
    type = 'GEOTIFF_16',
    desc = 'Moist soil color, 125cm (30m)',
    na = 0,
    rat = .soilColorRAT
  )
)


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
    rat = 'http://casoilresource.lawr.ucdavis.edu/800m_grids/RAT/series_name.csv'
  ),
  
  'hydgrp' = list(
    dsn = 'hydgrp',
    type = 'GEOTIFF_BYTE',
    desc = 'Hydrologic Soil Group',
    na = 0,
    rat = 'http://casoilresource.lawr.ucdavis.edu/800m_grids/RAT/hydgrp.csv'
  ),
  
  
  'drainage_class' = list(
    dsn = 'drainage_class',
    type = 'GEOTIFF_BYTE',
    desc = 'Soil Drainage Class',
    na = 0,
    rat = 'http://casoilresource.lawr.ucdavis.edu/800m_grids/RAT/drainage_class.csv'
  ),
  
  'weg' = list(
    dsn = 'weg',
    type = 'GEOTIFF_BYTE',
    desc = 'Wind Erodibility Group',
    na = 0,
    rat = 'http://casoilresource.lawr.ucdavis.edu/800m_grids/RAT/weg.csv'
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
    rat = 'http://casoilresource.lawr.ucdavis.edu/800m_grids/RAT/str.csv'
  ),
  
  'soilorder' = list(
    dsn = 'soilorder',
    type = 'GEOTIFF_BYTE',
    desc = 'Soil Taxonomy: Soil Order',
    na = 0,
    rat = 'http://casoilresource.lawr.ucdavis.edu/800m_grids/RAT/soilorder.csv'
  ),
  
  'suborder' = list(
    dsn = 'suborder',
    type = 'GEOTIFF_BYTE',
    desc = 'Soil Taxonomy: Suborder',
    na = 0,
    rat = 'http://casoilresource.lawr.ucdavis.edu/800m_grids/RAT/suborder.csv'
  ),
  
  'greatgroup' = list(
    dsn = 'greatgroup',
    type = 'GEOTIFF_BYTE',
    desc = 'Soil Taxonomy: Greatgroup',
    na = 0,
    rat = 'http://casoilresource.lawr.ucdavis.edu/800m_grids/RAT/greatgroup.csv'
  ),
  
  'texture_05cm' = list(
    dsn = 'texture_05cm',
    type = 'GEOTIFF_BYTE',
    desc = 'Soil Texture Class, 0-5cm',
    na = 0,
    rat = 'http://casoilresource.lawr.ucdavis.edu/800m_grids/RAT/texture_05.csv'
  ),
  
  'texture_025cm' = list(
    dsn = 'texture_025cm',
    type = 'GEOTIFF_BYTE',
    desc = 'Soil Texture Class, 0-25cm',
    na = 0,
    rat = 'http://casoilresource.lawr.ucdavis.edu/800m_grids/RAT/texture_025.csv'
  ),
  
  'texture_2550cm' = list(
    dsn = 'texture_2550cm',
    type = 'GEOTIFF_BYTE',
    desc = 'Soil Texture Class, 25-50cm',
    na = 0,
    rat = 'http://casoilresource.lawr.ucdavis.edu/800m_grids/RAT/texture_2550.csv'
  ),
  
  'lcc_irrigated' = list(
    dsn = 'lcc_irrigated',
    type = 'GEOTIFF_BYTE',
    desc = 'Land Capability Class, irrigated',
    na = 0,
    rat = 'http://casoilresource.lawr.ucdavis.edu/800m_grids/RAT/lcc.csv'
  ),
  
  'lcc_nonirrigated' = list(
    dsn = 'lcc_nonirrigated',
    type = 'GEOTIFF_BYTE',
    desc = 'Land Capability Class, non-irrigated',
    na = 0,
    rat = 'http://casoilresource.lawr.ucdavis.edu/800m_grids/RAT/lcc.csv'
  )
  
  
)


##
## TODO:
##       
##  * host WCS specifications as JSON on server so that changes don't require a soilDB release
##  * each suite of products should have their own spec
##  * remove xx.spec lists from soilDB
##  * top-level names in specs via JSON are used to determine available products
##  * are we just re-inventing a crummy version of STAC?
##

.mukey.spec <- list(
  
  # FY24 data were exported as INT32 instead of UINT32
  # NODATA is -2147483648
  'gnatsgo' = list(
    dsn = 'gnatsgo',
    type = 'GEOTIFF_FLOAT',
    desc = 'gNATSGO map unit keys',
    vintage = 'FY2024',
    na = 2147483647L,
    res = 30,
    rat = NULL
  ),
  
  'gssurgo' = list(
    dsn = 'gssurgo',
    type = 'GEOTIFF_FLOAT',
    desc = 'gSSURGO map unit keys',
    vintage = 'FY2024',
    na = 2147483647L,
    res = 30,
    rat = NULL
  ),
  
  'hi_ssurgo' = list(
    dsn = 'hi_ssurgo',
    type = 'GEOTIFF_FLOAT',
    desc = 'HI map unit keys',
    vintage = 'FY2024',
    na = 4294967295,
    res = 30,
    rat = NULL
  ),
  
  'pr_ssurgo' = list(
    dsn = 'pr_ssurgo',
    type = 'GEOTIFF_FLOAT',
    desc = 'PR map unit keys',
    vintage = 'FY2024',
    na = 4294967295,
    res = 30,
    rat = NULL
  ),
  
  'statsgo' = list(
    dsn = 'statsgo',
    type = 'GEOTIFF_FLOAT',
    desc = 'STATSGO2 map unit keys',
    vintage = 'FY2023',
    na = 0L,
    res = 300,
    rat = NULL
  ),
  
  'rss' = list(
    dsn = 'rss',
    type = 'GEOTIFF_FLOAT',
    desc = 'RSS map unit keys',
    vintage = 'FY2023',
    na = 0L,
    res = 10,
    rat = NULL
  )
)

