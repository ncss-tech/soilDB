#' Get SoilGrids 2.0 Property Estimates for Points or Spatial Extent
#'
#' @description
#'
#' This function obtains [SoilGrids 2.0](https://soilgrids.org) properties
#' information (250m raster resolution) given a \code{data.frame} containing
#' site IDs, latitudes and longitudes, or a spatial extent (see `grid=TRUE`
#' argument).
#'
#' SoilGrids API and maps return values as whole (integer) numbers to minimize
#' the storage space used. These values have conversion factors applied by
#' `fetchSoilGrids()` to produce conventional units shown in the table below
#' (see Details).
#'
#' @details
#' 
#' ## Properties
#' 
#' |Name     |Description                                                                        |Mapped units   | Conversion factor|Conventional units |
#' |:--------|:----------------------------------------------------------------------------------|:--------------|-----------------:|:------------------|
#' |bdod     |Bulk density of the fine earth fraction                                            |cg/cm^3        |               100|kg/dm^3            |
#' |cec      |Cation Exchange Capacity of the soil                                               |mmol(c)/kg     |                10|cmol(c)/kg         |
#' |cfvo     |Volumetric fraction of coarse fragments (> 2 mm)                                   |cm^3/dm^3 (vol per mil)  |         10|cm^3/100cm^3 (vol%)|
#' |clay     |Proportion of clay particles (< 0.002 mm) in the fine earth fraction               |g/kg           |                10|g/100g (%)         |
#' |nitrogen |Total nitrogen (N)                                                                 |cg/kg          |               100|g/kg               |
#' |phh2o    |Soil pH                                                                            |pH*10          |                10|pH                 |
#' |sand     |Proportion of sand particles (> 0.05 mm) in the fine earth fraction                |g/kg           |                10|g/100g (%)         |
#' |silt     |Proportion of silt particles (>= 0.002 mm and <= 0.05 mm) in the fine earth fraction |g/kg           |                10|g/100g (%)         |
#' |soc      |Soil organic carbon content in the fine earth fraction                             |dg/kg          |                10|g/kg               |
#' |ocd      |Organic carbon density                                                             |hg/m^3         |                10|kg/m^3             |
#' |ocs      |Organic carbon stocks (0-30cm depth interval only)                                 |t/ha           |                10|kg/m^2             |
#' |wv0010   |Volumetric Water Content at 10kPa                                                  |0.1 v% or 1 mm/m|                10|volume (%)         |
#' |wv0033   |Volumetric Water Content at 33kPa                                                  |0.1 v% or 1 mm/m|                10|volume (%)         |
#' |wv1500   |Volumetric Water Content at 1500kPa                                                |0.1 v% or 1 mm/m|                10|volume (%)         |
#'
#' SoilGrids predictions are made for the six standard depth intervals specified
#' in the GlobalSoilMap IUSS working group and its specifications. The default
#' depth intervals returned are (in centimeters): `"0-5"`, `"5-15"`, `"15-30"`,
#' `"30-60"`, `"60-100"`, `"100-200"` for the properties `"bdod"`, `"cec"`,
#' `"cfvo"`, `"clay"`, `"nitrogen"`, `"phh2o"`, `"sand"`, `"silt"`, `"soc"`,
#' `"ocd"`, `"wv0010"`, `"wv0033"`, `"wv1500"`--each with percentiles (5th,
#' 50th, 95th), mean and uncertainty values. The summary statistic name will be
#' appended to the abbreviate variable name for each depth interval returned.
#' Soil organic carbon stocks (0-30cm) (`variables="ocs"`) are returned only for
#' `depth_intervals="0-30"`. The uncertainty values are the ratio between the
#' inter-quantile range (90% prediction interval width) and the median :
#' `(Q0.95-Q0.05)/Q0.50.` All values are converted from "mapped" to
#' "conventional" based on above table conversion factors. Point data requests
#' are made through `"properties/query"` endpoint of the [SoilGrids v2.0 REST
#' API](https://www.isric.org/explore/soilgrids/faq-soilgrids). Please check
#' ISRIC's data policy, disclaimer and citation:
#' \url{https://www.isric.org/about/data-policy}.
#'
#' Find out more information about the SoilGrids and GlobalSoilMap products
#' here:
#' 
#'  - \url{https://www.isric.org/explore/soilgrids/faq-soilgrids}
#'  - \url{https://www.isric.org/sites/default/files/GlobalSoilMap_specifications_december_2015_2.pdf}
#' 
#' @references
#'  - **Common soil chemical and physical properties:**
#' Poggio, L., de Sousa, L. M., Batjes, N. H., Heuvelink, G. B. M., Kempen, B.,
#' Ribeiro, E., and Rossiter, D.: SoilGrids 2.0: producing soil information for
#' the globe with quantified spatial uncertainty, SOIL, 7, 217–240, 2021. DOI:
#' \doi{https://doi.org/10.5194/soil-7-217-2021}
#'  - **Soil water content at different pressure heads:**
#' Turek, M.E.,  Poggio, L., Batjes, N. H., Armindo, R. A.,  de Jong van Lier,
#' Q.,  de Sousa, L.M.,  Heuvelink, G. B. M. : Global mapping of volumetric
#' water retention at 100, 330 and 15000 cm suction using the WoSIS database,
#' International Soil and Water Conservation Research, 11-2, 225-239, 2023. DOI:
#' \doi{https://doi.org/10.1016/j.iswcr.2022.08.001}
#' 
#' @importFrom utils packageVersion
#'
#' @param x A `data.frame` containing 3 columns referring to site ID, latitude
#'   and longitude. Or a spatial (sf, terra) object for which a bounding box can
#'   be calculated when `grid=TRUE`.
#' @param loc.names Optional: Column names referring to site ID, latitude and
#'   longitude. Default: `c("id", "lat", "lon")`
#' @param depth_intervals Default: `"0-5"`, `"5-15"`, `"15-30"`, `"30-60"`,
#'   `"60-100"`, `"100-200"`
#' @param variables Default: `"bdod"`, `"cec"`, `"cfvo"`, `"clay"`,
#'   `"nitrogen"`, `"phh2o"`, `"sand"`, `"silt"`, `"soc"`, `"ocd"`, `"wv0010"`,
#'   `"wv0033"`, `"wv1500"`. Optionally `"ocs"` (only for 0 to 30 cm interval).
#' @param grid Download subset of SoilGrids Cloud Optimized GeoTIFF? Default:
#'   `FALSE`
#' @param filename Only used when `grid=TRUE`. If `NULL` defaults to an
#'   in-memory raster, or temporary file if result does not fit in memory.
#' @param overwrite Only used when `grid=TRUE`. Default: `FALSE`
#' @param target_resolution Only used when `grid=TRUE`. Default: `c(250, 250)`
#'   (250m x 250m pixels)
#' @param summary_type Only used when `grid=TRUE`. One or more of `"Q0.05"`,
#'   `"Q0.5"`, `"Q0.95"`, `"mean"`; these are summary statistics that correspond
#'   to 5th, 50th, 95th percentiles, and mean value for selected `variables`.
#' @param endpoint Optional: custom API endpoint. Default:
#'   `"https://rest.isric.org/soilgrids/v2.0/properties/query"` when
#'   `grid=FALSE`; `"https://files.isric.org/soilgrids/latest/data/"` when
#'   `grid=TRUE`.
#' @param ... Additional arguments passed to `terra::writeRaster()` when
#'   `grid=TRUE`.
#' @param verbose Print messages? Default: `FALSE`
#' @param progress logical, give progress when iterating over multiple requests;
#'   Default: `FALSE`
#' 
#' @return A _SoilProfileCollection_ (or _SpatRaster_ when `grid=TRUE`). Returns
#'   `try-error` if all requests fail. Any error messages resulting from parsing
#'   will be echoed when `verbose=TRUE`.
#' @export fetchSoilGrids
#' @author Andrew G. Brown
#' @examplesIf curl::has_internet() && requireNamespace("aqp")
#' \dontrun{
#'   library(aqp)
#'   
#'   your.points <- data.frame(id  = c("A", "B"), 
#'                             lat = c(37.9, 38.1), 
#'                             lon = c(-120.3, -121.5), 
#'                             stringsAsFactors = FALSE)
#'   x <- try(fetchSoilGrids(your.points))
#'  
#'   if (!inherits(x, 'try-error'))
#'    aqp::plotSPC(x, name = NA, color = "socQ50")
#'  
#'   # organic carbon stocks use 0-30cm interval
#'   y <- try(fetchSoilGrids(your.points[1, ], 
#'                           depth_interval = c("0-5", "0-30", "5-15", "15-30"),
#'                           variables = c("soc", "bdod", "ocd", "ocs")))
#'                           
#'   # extract horizons from a SoilProfileCollection where horizon 2 overlaps 1, 3, and 4
#'   h <- aqp::horizons(y)
#'   
#'   # "ocs" (organic carbon stock 0-30cm interval)
#'   h[2, ]
#'   
#'   h$thickness_meters <- ((h$hzdepb - h$hzdept) / 100)
#'
#'   # estimate "ocs" from modeled organic carbon and bulk density in 0-5, 5-15, 15-30 intervals
#'   #  (sum the product of soc, bdod, and thickness in meters)
#'   #  (1 gram per cubic decimeter = 1 kilogram per cubic meter)
#'   sum(h$socmean * h$bdodmean * h$thickness_meters, na.rm = TRUE)
#'   
#'   # estimate "ocs" from modeled organic carbon density in 0-5, 5-15, 15-30 intervals
#'   #  (sum the product of "ocd" and thickness in meters)
#'   sum(h$ocdmean * h$thickness_meters, na.rm = TRUE)
#'  
#' }
fetchSoilGrids <- function(x,
                           loc.names = c("id", "lat", "lon"),
                           depth_intervals = c("0-5", "5-15", "15-30", 
                                               "30-60", "60-100", "100-200"),
                           variables = c("bdod", "cec", "cfvo", "clay", 
                                         "nitrogen", "phh2o", "sand", "silt", 
                                         "soc", "ocd", "wv0010", "wv0033", "wv1500"),
                           grid = FALSE,
                           filename = NULL, 
                           overwrite = TRUE, 
                           target_resolution = c(250, 250),
                           summary_type = c("Q0.05", "Q0.5", "Q0.95", "mean"),
                           endpoint = ifelse(!grid, 
                                             "https://rest.isric.org/soilgrids/v2.0/properties/query",
                                             "https://files.isric.org/soilgrids/latest/data/"),
                           ...,
                           verbose = FALSE,
                           progress = FALSE) {
  
  locations <- x
  spatial_input <- FALSE
  
  if (inherits(locations, 'sf') || inherits(locations, 'Spatial')) {
    if (requireNamespace("sf")) {
      if (inherits(locations, 'Spatial') ||
          inherits(locations, 'SpatVector')) {
        # convert sp -> sf & terra -> sf
        locations <- sf::st_as_sf(locations)
      }
    }
  }
  
  if (grid) {
    return(.get_soilgrids(locations, 
                          filename = filename, 
                          overwrite = overwrite, 
                          variables = variables,
                          target_resolution = target_resolution,
                          depth = depth_intervals,
                          summary_type = summary_type,
                          endpoint = endpoint,
                          ...,
                          verbose = verbose))
  } else if (!inherits(locations, 'data.frame')) {
    if (inherits(locations, 'SpatVector')) {
      locations <- sf::st_as_sf(locations)
    }
    # only supporting POINT geometry for now
    if (inherits(sf::st_geometry(locations), 'sfc_POINT')) {
      if (is.na(sf::st_crs(locations)$wkt)) {
        message("CRS is missing; assuming WGS84 decimal degrees (EPSG:4326)")
        sf::st_crs(locations) <- sf::st_crs(4326)
      }
      locations <- data.frame(id = seq_len(nrow(locations)),
                              do.call('rbind', sf::st_geometry(locations)))
      spatial_input <- TRUE
      colnames(locations) <- c("id", "lon", "lat")
      loc.names <- c("id", "lat", "lon")
    } else {
      stop("only POINT geometries are supported as input", call. = FALSE)
    }
  }
  
  if (is.null(loc.names)) {
    loc.names <- c("id", "lat", "lon")
  }
  
  if (length(loc.names) != 3 | !all(loc.names %in% colnames(locations))) {
    stop("argument `loc.names` must contain three column names: site ID, latitude and longitude", call. = FALSE)
  }
  
  if (!is.numeric(locations[[loc.names[2]]]) || 
      !is.numeric(locations[[loc.names[3]]])) {
    stop(sprintf("latitude (%s) and longitude (%s) must be numeric values in decimal degrees", loc.names[2], loc.names[3]), call. = FALSE)
  }
  
  if (progress) {
    pb <- txtProgressBar(min = 0, max = nrow(locations), style = 3)
  }
  
  res <- vector('list', nrow(locations))
  locsplit <- split(locations, f = locations[[loc.names[1]]])
  
  for (i in seq_along(res)) {
    
    yd <- locsplit[[i]]
    id <- as.character(yd[[loc.names[1]]])
    lat <- as.numeric(yd[[loc.names[2]]])
    lon <- as.numeric(yd[[loc.names[3]]])
    
    if (any(length(lat) == 0, length(lon) == 0)) {
      if (verbose)
        message(sprintf("skipped site ID (%s); 0-length coordinates", id))
      res[[i]] <- NULL    
    }
    
    response <- try(httr::GET(sprintf(paste0(endpoint, "?lat=%s&lon=%s"), lat, lon)), silent = !verbose)
    
    if (inherits(response, 'try-error')) {
      if (verbose)
        message(sprintf("SoilGrids API request failed for %s, %s", lat, lon))
      res[[i]] <- NULL    
    }
    
    r.content <- try(httr::content(response, as = "text", encoding = "UTF-8"), silent = !verbose)
    if (inherits(r.content, 'try-error')) {
      res[[i]] <- NULL   
    }

    jres <- try(jsonlite::fromJSON(r.content), silent = !verbose)
    if (inherits(jres, 'try-error')) {
      jres <- list(jres)
      res[[i]] <- NULL   
    }
    
    # add handling for messages from api about erroneous input
    if (!is.null(jres$detail)) {
      if (!is.null(jres$detail$msg)) {
        if (!is.null(jres$detail$loc) && length(jres$detail$loc) > 0)
          message("Check ", 
                  paste0(jres$detail$loc[[1]], collapse = " "), 
                  " (", loc.names[1], ":", locations[[loc.names[1]]][i], "); ",
                  jres$detail$msg)
      }
    }
    
    # create new horizon data, merge in each property using standard depth labels
    depth.intervals <- match.arg(gsub(" ", "", depth_intervals), 
                                 c("0-5", "5-15", "15-30", "0-30",
                                   "30-60", "60-100", "100-200"), 
                                 several.ok = TRUE)
    hz.data0 <- data.frame(id = id, latitude = lat, longitude = lon, 
                          label = depth.intervals, stringsAsFactors = FALSE)

    # values returned for each layer include the following properties
    data.types <- match.arg(gsub(" ", "", variables), c("bdod", "cec", "cfvo", 
                                                        "clay", "nitrogen", "phh2o",
                                                        "sand", "silt", "soc",
                                                        "ocd", "ocs", 
                                                        "wv0010", "wv0033", "wv1500"), several.ok = TRUE)
    
    # numeric values are returned as integers that need to be scaled to match typical measurement units
    data.factors <- c(bdod = 0.01, cec = 0.1, cfvo = 0.1, clay = 0.1, nitrogen = 0.01, 
                      phh2o = 0.1, sand = 0.1, silt = 0.1, soc = 0.1, ocd = 0.1, ocs = 0.1, 
                      wv0010 = 0.1, wv0033 = 0.1, wv1500 = 0.1)
    data.factor <- data.factors[data.types]
    hz.data <- hz.data0
    all_x <- FALSE
    for (d in seq_along(data.types)) {
      if (nrow(hz.data0) > 0 && !inherits(jres[[1]], 'try-error')) {
        hz.data <- merge(
            merge(hz.data0, hz.data, by = c("label", "id", "latitude", "longitude"),
                  sort = FALSE, all.x = TRUE),
            .extractSGLayerProperties(jres, data.types[d], data.factor[d]),
            all.x = all_x, sort = FALSE, by = "label"
        )
      }
      all_x <- TRUE
    }
    
    rownames(hz.data) <- NULL
    
    if (progress) {
      setTxtProgressBar(pb, i)
    }
    
    res[[i]] <- hz.data
    
    if ((i %% 5) == 0) {
      # ISRIC states "Fair Use" of API should not exceed 5 requests per minute.
      # This should prevent requests for more than 5 sites from erroring out, but will not affect requests <5 sites
      Sys.sleep(61)
    }
  }
  
  # combine horizon data together
  spc <- data.frame(data.table::rbindlist(res, fill = TRUE))
  
  # calculate top and bottom depths from label
  labelsplit <- strsplit(as.character(spc$label), split = "-")
  
  #  note sort order of labels may differ from sort order of depths
  spc$hzdept <- as.numeric(lapply(labelsplit, function(x) x[1]))
  spc$hzdepb <- as.numeric(lapply(labelsplit, function(x) x[2]))
  spc <- spc[order(spc$id, spc$hzdept, spc$hzdepb), ]
  
  if (!requireNamespace("aqp")) {
    stop("package 'aqp' is required", call. = FALSE)
  }
  
  # promote to SoilProfileCollection
  aqp::depths(spc) <- id ~ hzdept + hzdepb
  
  # move location information to site
  aqp::site(spc) <- ~ longitude + latitude
  
  # requires aqp > 2.0 (now set in DESCRIPTION)
  aqp::initSpatial(spc, crs = "OGC:CRS84") <- ~ longitude + latitude
  
  # merge the rest of the sf object into the site table 
  if (spatial_input) {
    aqp::site(spc) <- cbind(id = seq_len(nrow(x)), sf::st_drop_geometry(x))
  }
  
  if (ncol(aqp::horizons(spc)) == 5) {
    res <- try(stop("SoilGrids API is not accessible", call. = FALSE), silent = !verbose)
    return(invisible(res))
    # this occurs if all requests fail to retrieve data/JSON response
  }
  
  return(spc)
}

.extractSGLayerProperties <- function(jsonres, x, scaling.factor = 1) {
  out <-  jsonres$properties$layers[jsonres$properties$layers$name == x,]$depths[[1]]
  
  # rescale integer values to common units
  sgvalues <-  out[, "values"][, c("Q0.05", "Q0.5", "Q0.95", "mean")] * scaling.factor
  
  # uncertainty is always scaled by a factor of 10, even for bdod and nitrogen (whose data are scaled by 100)
  uncertainty <- out[, "values"][, "uncertainty"] * 0.1
  
  out[["values"]] <- cbind(sgvalues, uncertainty)
  
  # fix names and labels for downstream
  cn <- colnames(out)[grep("range", colnames(out), invert = TRUE)]
  if (length(cn) > 0) {
    out <- out[, cn]
    out <- cbind(label = gsub("cm", "", out$label), values = out$values)
    colnames(out) <- gsub("\\.Q0\\.", "Q", colnames(out))
    colnames(out) <- gsub("Q5", "Q50", colnames(out))
    colnames(out) <- gsub("values", x, colnames(out))
    colnames(out) <- gsub("\\.", "", colnames(out))
  }
  return(out)
}

.get_soilgrids <- function(x, 
                           filename = NULL, 
                           overwrite = TRUE, 
                           target_resolution = c(250, 250),
                           depth = c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200"),
                           summary_type = c("Q0.05", "Q0.5", "Q0.95", "mean"),
                           variables = c("bdod", "cec", "cfvo", "clay", "nitrogen", 
                                         "phh2o", "sand", "silt", "soc", "wv0010", "wv0033", "wv1500"),
                           endpoint = "https://files.isric.org/soilgrids/latest/data/",
                           ...,
                           verbose = TRUE) {
  
  stopifnot(requireNamespace("terra"))
  stopifnot(requireNamespace("sf"))
  
  if (!all(substr(depth, nchar(depth) - 1, nchar(depth)) == "cm")) {
    depth <- paste0(depth, "cm")
  }
  
  if (inherits(x, 'Spatial')) {
    x <- sf::st_as_sf(x) # sp support
  }
  
  if (!inherits(x, 'bbox')) {
    # sf/sfc/stars/Raster*/SpatVector/SpatRaster support
    xbbox <- sf::st_bbox(x)
  } else xbbox <- x
  
  sg_crs <- '+proj=igh'
  
  # specifying vsicurl parameters appears to not work with GDAL 3.2.1
  # sg_url <- paste0("/vsicurl?max_retry=", max_retry,
  #                  "&retry_delay=", retry_delay,
  #                  "&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/")
  
  # WORKS
  sg_url <- paste0("/vsicurl/", endpoint)
  
  # calculate homolosine bbox
  xbbox <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(xbbox), sg_crs)) 
  
  # numeric values are returned as integers that need to be scaled to match typical measurement units
  data.factor <- c(0.01, 0.1, 0.1, 0.1, 0.01, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
  names(data.factor) <- c("bdod", "cec", "cfvo", "clay", "nitrogen", 
                          "phh2o", "sand", "silt", "soc", "wv0010", "wv0033", "wv1500")
  
  # calculate temporary file names for each variable*depth*summary grid
  vardepth <- apply(expand.grid(variables, summary_type, depth), 1, paste0, collapse = "_")
  tfs <- tempfile(pattern = paste0(vardepth, "_"), fileext = ".tif")
  names(tfs) <- vardepth
  
  # helper function for gdal_utils options
  .gdal_utils_opts <- function(lst) do.call('c', lapply(names(lst), function(y) c(y, lst[[y]])))
  
  # iterate over variable*depth
  for (i in seq_along(tfs)) {
    
    # translate remote .vrt -> local .tif
    vd <- strsplit(vardepth[i], "_")[[1]]
    v <- vd[1]; d <- vd[2]; s <- vd[3]
    
    sf::gdal_utils(
      util = "translate",
      source = paste0(sg_url, paste0(v, '/', v, '_', s, '_', d, '.vrt')),
      destination = tfs[i],
      options = .gdal_utils_opts(
        list(
          "-of" = "GTiff",
          "-tr" = target_resolution,
          "-projwin" = as.numeric(xbbox)[c(1, 4, 3, 2)],
          "-projwin_srs" = sg_crs
        )
      ),
      quiet = !verbose
    )
  }
  
  stk <- terra::rast(tfs)
  names(stk) <- vardepth
  
  if (!is.null(filename)) {
    terra::writeRaster(stk, filename = filename, overwrite = overwrite, ...)
  }
  
  # apply conversion factor
  stk2 <- terra::app(stk, function(x) x * data.factor[gsub("([a-z]+)_.*", "\\1", names(x))])
  
  stk2
}
