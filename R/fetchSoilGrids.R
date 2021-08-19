#' Get SoilGrids 250m properties information from point locations
#'
#' This function obtains SoilGrids properties information (250m raster resolution) given a \code{data.frame} containing site IDs, latitudes and longitudes. SoilGrids API and maps return values as whole (integer) numbers to minimize the storage space used. These values are converted by to produce conventional units by `fetchSoilGrids()``
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
#' |silt     |Proportion of silt particles (= 0.002 mm and = 0.05 mm) in the fine earth fraction |g/kg           |                10|g/100g (%)         |
#' |soc      |Soil organic carbon content in the fine earth fraction                             |dg/kg          |                10|g/kg               |
#' |ocd      |Organic carbon density                                                             |hg/m^3         |                10|kg/m^3             |
#' |ocs      |Organic carbon stocks                                                              |t/ha           |                10|kg/m^2             |
#' 
#' SoilGrids predictions are made for the six standard depth intervals specified in the GlobalSoilMap IUSS working group and its specifications. The depth intervals returned are: \code{"0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm"} and the properties returned are \code{"bdod", "cec", "cfvo", "clay", "nitrogen", "phh2o", "sand", "silt", "soc"} -- each with 5th, 50th, 95th, mean and uncertainty values. The uncertainty values are the ratio between the inter-quantile range (90% prediction interval width) and the median : `(Q0.95-Q0.05)/Q0.50.` Point data requests are made through \code{properties/query} endpoint of the [SoilGrids v2.0 REST API](https://www.isric.org/explore/soilgrids/faq-soilgrids). Please check ISRIC's data policy, disclaimer and citation: \url{https://www.isric.org/about/data-policy}.
#' 
#' Find out more information about the SoilGrids and GlobalSoilMap products here: 
#' 
#'  - \url{https://www.isric.org/explore/soilgrids/faq-soilgrids}
#'  - \url{https://www.isric.org/sites/default/files/GlobalSoilMap_specifications_december_2015_2.pdf}
#' 
#' @references Poggio, L., de Sousa, L. M., Batjes, N. H., Heuvelink, G. B. M., Kempen, B., Ribeiro, E., and Rossiter, D.: SoilGrids 2.0: producing soil information for the globe with quantified spatial uncertainty, SOIL, 7, 217-240, 2021. \doi{https://doi.org/10.5194/soil-7-217-2021}
#'
#' @param x A \code{data.frame} containing 3 columns referring to site ID, latitude and longitude.
#' @param loc.names Optional: Column names referring to site ID, latitude and longitude. Default: \code{c("id","lat","lon")}
#' @param verbose Print messages? Default: `FALSE`
#' @param progress logical, give progress when iterating over multiple requests; Default: `FALSE`
#' 
#' @return A SoilProfileCollection
#' @export fetchSoilGrids
#' 
#' @author Andrew G. Brown
#' 
#' @examples
#' \donttest{
#'  if(requireNamespace("curl") &
#'    curl::has_internet()) {
#'   
#'   library(aqp)
#' 
#'   your.points <- data.frame(id  = c("A", "B"), 
#'                            lat = c(37.9, 38.1), 
#'                            lon = c(-120.3, -121.5), 
#'                            stringsAsFactors = FALSE)
#'
#'   x <- try(fetchSoilGrids(your.points))
#'  
#'   if (!inherits(x, 'try-error'))
#'    plotSPC(x, name = NA, color = "socQ50")
#'  }
#' }
fetchSoilGrids <- function(x,
                           loc.names = c("id", "lat", "lon"),
                           verbose = FALSE,
                           progress = FALSE) {
  
  locations <- x
  spatial_input <- FALSE
  
  if (inherits(locations, 'sf') || inherits(locations, 'Spatial')) {
    if (requireNamespace("sf")) {
      if (inherits(locations, 'Spatial')) {
        # convert sp -> sf
        locations <- sf::st_as_sf(locations)
      }
      
      # only supporting POINT geometry for now
      if (inherits(sf::st_geometry(locations), 'sfc_POINT')) {
        if (is.na(sf::st_crs(locations)$wkt)) {
          message("CRS is missing; assuming WGS84 decimal degrees (EPSG:4326)")
          sf::st_crs(locations) <- sf::st_crs(4326)
        }
        locations <- data.frame(id = 1:nrow(locations), 
                                do.call('rbind', sf::st_geometry(locations)))
        spatial_input <- TRUE
        colnames(locations) <- c("id", "lon", "lat")
        loc.names <- c("id", "lat", "lon")
      } else {
        stop("only POINT geometries are supported as input", call. = FALSE) 
      }
    }
  }
  
  if (is.null(loc.names)) {
    loc.names <- c("id", "lat", "lon")
  }
  
  if (length(loc.names) != 3 | any(!loc.names %in% colnames(locations))) {
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
    
    response <- try(httr::GET(sprintf("https://rest.isric.org/soilgrids/v2.0/properties/query?lat=%s&lon=%s", lat, lon)), silent = !verbose)
    
    if (inherits(response, 'try-error')) {
      if (verbose)
        message(sprintf("SoilGrids API request failed for %s, %s", lat, lon))
      res[[i]] <- NULL    
    }
    
    r.content <- httr::content(response, as = "text", encoding = "UTF-8")
    jres <- jsonlite::fromJSON(r.content)
    
    # create new horizon data, merge in each property using standard depth labels
    depth.intervals <-  c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
    hz.data <- data.frame(id = id, latitude = lat, longitude = lon, label = depth.intervals, stringsAsFactors = FALSE)

    # values returned for each layer include the following properties
    data.types <- c("bdod", "cec", "cfvo", "clay", "nitrogen", "phh2o", "sand", "silt", "soc")
    
    # numeric values are returned as integers that need to be scaled to match typical measurement units
    data.factor <- c(0.01, 0.1, 0.1, 0.1, 0.01, 0.1, 0.1, 0.1, 0.1)
    
    for (d in 1:length(data.types)) {
      hz.data <- merge(hz.data, .extractSGLayerProperties(jres, data.types[d], data.factor[d]), by = "label")
    }
    
    rownames(hz.data) <- NULL
    
    if (progress) {
      setTxtProgressBar(pb, i)
    }
    
    res[[i]] <- hz.data
  }
  
  # combine horizon data together
  spc <- do.call('rbind', res)
  
  # calculate top and bottom depths from label
  labelsplit <- strsplit(as.character(spc$label), split = "-")
  
  spc$hzdept <- as.numeric(lapply(labelsplit, function(x) x[1]))
  spc$hzdepb <- as.numeric(lapply(labelsplit, function(x) x[2]))
  
  # promote to SoilProfileCollection
  depths(spc) <- id ~ hzdept + hzdepb
  
  # move location information to site
  site(spc) <- ~ longitude + latitude
  coordinates(spc) <- ~ longitude + latitude
  proj4string(spc) <- "EPSG:4326"
 
  # merge the rest of the sf object into the site table 
  if (spatial_input) {
    site(spc) <- cbind(id = 1:nrow(x), sf::st_drop_geometry(x))
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
  out <- out[,colnames(out)[grep("range", colnames(out), invert = TRUE)]]
  out <- data.frame(label = gsub("cm", "", out$label), values = out$values)
  colnames(out) <- gsub("\\.Q0\\.", "Q", colnames(out))
  colnames(out) <- gsub("Q5", "Q50", colnames(out))
  colnames(out) <- gsub("values", x, colnames(out))
  colnames(out) <- gsub("\\.", "", colnames(out))
  
  return(out)
}
