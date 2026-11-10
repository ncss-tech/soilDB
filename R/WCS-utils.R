
## TODO: ISSR800 getCapabilities still doesn't work, why?
# https://casoilresource.lawr.ucdavis.edu/cgi-bin/mapserv?map=isr800-wcs&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCapabilities

# stay in holding pattern until resolved

WCS_capabilities <- function(wcs = c('mukey', 'issr800', 'soilcolor')) {
  
  if (!requireNamespace('xml2')) {
    stop('please install the package: xml2', call. = FALSE)
  }
  
  wcs <- match.arg(wcs)
  
  .mapfile <- switch(
    wcs,
    mukey = 'mukey-wcs',
    issr800 = 'issr800-wcs',
    soilcolor = 'soilcolor-wcs'
  )
  
  .u <- sprintf('https://casoilresource.lawr.ucdavis.edu/cgi-bin/mapserv?map=%s&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCapabilities', .mapfile)
  
  .xml <- xml2::read_xml(.u)
  
  .layers <- xml2::xml_find_all(.xml, xpath = '//wcs:CoverageId')
  
  .res <- xml2::xml_text(xml2::xml_contents(.layers))
  
  return(.res)
}


WCS_describeCoverage <- function(wcs = c('mukey', 'issr800', 'soilcolor'), layer) {
  
  if (!requireNamespace('xml2')) {
    stop('please install the package: xml2', call. = FALSE)
  }
  
  wcs <- match.arg(wcs)
  
  .mapfile <- switch(
    wcs,
    mukey = 'mukey-wcs',
    issr800 = 'issr800-wcs',
    soilcolor = 'soilcolor-wcs'
  )
  
  u <- sprintf(
    'https://casoilresource.lawr.ucdavis.edu/cgi-bin/mapserv?map=%s&SERVICE=WCS&VERSION=2.0.1&REQUEST=DescribeCoverage&COVERAGEID=%s',
    .mapfile, layer
  )
  
  .xml <- xml2::read_xml(u)
  
  browser()
  
}


###





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
  
  # select a WCS
  wcs <- match.arg(wcs)
  
  # normalize to lower case
  wcs <- tolower(wcs)
  
  # load appropriate WCS specifications form SoilWeb
  .f <- switch(
    wcs,
    mukey = 'mukey-grids.json',
    issr800 = 'issr800.json',
    soilcolor = 'soilcolor.json'
  )
  
  .u <- sprintf("https://casoilresource.lawr.ucdavis.edu/wcs/%s", .f)
  
  spec <- try(
    fromJSON(.u), 
    silent = TRUE
  )
  
  if(inherits(spec, 'try-error')) {
    stop('WCS spec not available', call. = FALSE)
  }
  
  
  # extract relevant details
  v <- lapply(spec, function(i) {
    data.frame(
      var = i[['dsn']],
      vintage = i[['vintage']],
      crs = i[['crs']],
      description = i[['desc']]
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
.prepare_AOI <- function(obj, res, native_crs = 'EPSG:5070') {
  
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
    aoi.native <- e.native[c(1, 3, 2, 4)]
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


