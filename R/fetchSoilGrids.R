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
#' SoilGrids predictions are made for the six standard depth intervals specified in the GlobalSoilMap IUSS working group and its specifications. The depth intervals returned are: \code{"0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm"} and the properties returned are \code{"bdod", "cec", "cfvo", "clay", "nitrogen", "phh2o", "sand", "silt", "soc"} -- each with 5th, 50th, 95th, mean and uncertainty values. The uncertainty values are the ratio between the inter-quantile range (90% prediction interval width) and the median : `(Q0.95-Q0.05)/Q0.50.` Point data requests are made through \code{properties/query} endpoint of the SoilGrids v2.0 REST API: \url{https://rest.isric.org/soilgrids/v2.0/docs}. 
#' 
#' Find out more information about the SoilGrids and GlobalSoilMap products here: 
#' 
#'  - \url{https://www.isric.org/explore/soilgrids/faq-soilgrids}
#'  - \url{https://www.isric.org/sites/default/files/GlobalSoilMap_specifications_december_2015_2.pdf}
#' 
#' @references Poggio, L., de Sousa, L. M., Batjes, N. H., Heuvelink, G. B. M., Kempen, B., Ribeiro, E., and Rossiter, D.: SoilGrids 2.0: producing soil information for the globe with quantified spatial uncertainty, SOIL, 7, 217-240, 2021. \doi{https://doi.org/10.5194/soil-7-217-2021}
#'
#' @param locations A \code{data.frame} containing 3 columns referring to site ID, latitude and longitude.
#' 
#' @param loc.names Optional: Column names referring to site ID, latitude and longitude. Default: \code{c("id","lat","lon")}
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
fetchSoilGrids <- function(locations, loc.names = c("id","lat","lon")) {
  
  if (is.null(loc.names))
    loc.names <- c("id","lat","lon")
  
  if (length(loc.names) != 3 | any(!loc.names %in% colnames(locations)))
    stop("argument `loc.names` must contain three column names: site ID, latitude and longitude", 
         call. = FALSE)
  
  res <- lapply(split(locations, f = locations[[loc.names[[1]]]]), function(yd) {
    id <- as.character(yd[[loc.names[[1]]]])
    lat <- as.numeric(yd[[loc.names[[2]]]])
    lon <- as.numeric(yd[[loc.names[[3]]]])
    
    response <- httr::GET(sprintf("https://rest.isric.org/soilgrids/v2.0/properties/query?lat=%s&lon=%s", lat, lon))
    r.content <- httr::content(response, as = "text", encoding = "UTF-8")
    res <- jsonlite::fromJSON(r.content)
    
    # create new horizon data, merge in each property using standard depth labels
    depth.intervals <-  c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
    hz.data <- data.frame(id = id, latitude = lat, longitude = lon, label = depth.intervals, stringsAsFactors = FALSE)

    # values returned for each layer include the following properties
    data.types <- c("bdod", "cec", "cfvo", "clay", "nitrogen", "phh2o", "sand", "silt", "soc")
    
    # numeric values are returned as integers that need to be scaled to match typical measurement units
    data.factor <- c(0.01, 0.1, 0.1, 0.1, 0.01, 0.1, 0.1, 0.1, 0.1)
    
    for (d in 1:length(data.types))
      hz.data <- merge(hz.data, .extractSGLayerProperties(res, data.types[d], data.factor[d]), by = "label")
    
    rownames(hz.data) <- NULL
    
    return(hz.data)
  })
  
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
  proj4string(spc) <- "+proj=longlat +datum=WGS84"
  
  return(spc)
}

.extractSGLayerProperties <- function(jsonres, x, scaling.factor = 1) {
  out <-  jsonres$properties$layers[jsonres$properties$layers$name == x,]$depths[[1]]
  
  # rescale integer values to common scales
  out[["values"]] <- out[,"values"] * scaling.factor
  
  # fix names and labels for downstream
  out <- out[,colnames(out)[grep("range", colnames(out), invert = TRUE)]]
  out <- data.frame(label = gsub("cm", "", out$label), values = out$values)
  colnames(out) <- gsub("\\.Q0\\.", "Q", colnames(out))
  colnames(out) <- gsub("Q5", "Q50", colnames(out))
  colnames(out) <- gsub("values", x, colnames(out))
  colnames(out) <- gsub("\\.", "", colnames(out))
  
  return(out)
}
