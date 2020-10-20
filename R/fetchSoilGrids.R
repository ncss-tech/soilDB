#' Fetch SoilGrids 250m properties information from point locations
#'
#' This function obtains SoilGrids properties information (250m raster resolution) given a \code{data.frame} containing site IDs, latitudes and longitudes. 
#' 
#' The depth intervals returned are: \code{"0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm"} and the properties returned are \code{"bdod", "cec", "cfvo", "clay", "nitrogen", "phh2o", "sand", "silt", "soc"} -- each with 5th, 50th, 95th, mean and uncertainty values. Point data requests are made through \code{properties/query} endpoint of the SoilGrids v2.0 REST API: https://rest.soilgrids.org/soilgrids/v2.0/docs
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
#'   x <- fetchSoilGrids(your.points)
#'  
#'   plotSPC(x, name = NA, color = "socQ50")
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
    
    response <- httr::GET(sprintf("https://rest.soilgrids.org/soilgrids/v2.0/properties/query?lat=%s&lon=%s", lat, lon))
    r.content <- httr::content(response, as = "text", encoding = "UTF-8")
    res <- jsonlite::fromJSON(r.content)
    
    # create new horizon data, merge in each property using standard depth labels
    depth.intervals <-  c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
    hz.data <- data.frame(id = id, latitude = lat, longitude = lon, label = depth.intervals, stringsAsFactors = FALSE)

    # values returned for each layer include the following properties
    data.types <- c("bdod", "cec", "cfvo", "clay", "nitrogen", "phh2o", "sand", "silt", "soc")
    # numeric values are returned as integers that need to be scaled to match typical measurement units
    data.factor <- c(0.01, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
    
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
