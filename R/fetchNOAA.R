# fetchNOAA
# helper functions for using the NOAA API

# fetchNOAA
# TODO: will provide a variety of aggregation and batch application options
#  -- given Spatial* object, SoilProfileCollection, Coordinates

#' Query the NOAA API to get station data near a given latitude and longitude
#'
#' @description Query the NOAA API to get station data (limit 1000 records) near a point. Default extent is plus or minus 0.5 degrees (bounding box) (with \code{bbox = 1}) around the specified point \[lat, lng].
#'
#' In order to use this function, you must obtain an API token from this website: https://www.ncdc.noaa.gov/cdo-web/token
#'
#' @param lat Latitude
#' @param lng Longitude
#' @param apitoken API key token for NOAA NCDC web service
#' @param bbox Optional: Dimension of the bounding box centered at \code{lat}, \code{lng}.
#'
#' @return data.frame containing station information for all stations within a bounding box around \code{lat}, \code{lng}.
#' @export
#'
#' @examples
#'
#' ## in order to use this function, you must obtain an API token from this website:
#' ##  https://www.ncdc.noaa.gov/cdo-web/token
#'
#' # stations <- get_NOAA_stations_nearXY(lat = 37, lng = -120,
#' #                                      apitoken = "yourtokenhere")
#'
get_NOAA_stations_nearXY <- function(lat, lng, apitoken, bbox = 1) {

  if(!requireNamespace("httr"))
    stop("package `httr` is required")

  # determine dimension in each direction to build bbox
  coord <- data.frame(lat = lat, lng = lng)
  coordinates(coord) <- ~ lng + lat
  bdim <- bbox / 2

  # build Google Maps API V3 LatLngBounds.toUrlValue string
  ext_string <- sprintf("%s,%s,%s,%s", lat - bdim, lng - bdim, lat + bdim, lng + bdim)

  # construct GET request
  r <- httr::GET(url = sprintf(
    "https://www.ncdc.noaa.gov/cdo-web/api/v2/stations?extent=%s&limit=1000",
    ext_string
  ), httr::add_headers(token = apitoken))

  # retrieve content
  r.content <- httr::content(r, as = "text", encoding = "UTF-8")

  # convert JSON to data.frame
  d <- jsonlite::fromJSON(r.content)

  if(nrow(d$results) == 1000)
    message("maximum record limit reached (n = 1000) -- try a smaller bounding box (bbox) value to return fewer stations")

  return(d$results)
}

.get_NOAA_GHCND_by_stationyear <- function(stationid, year, datatypeid, apitoken) {

  if(!requireNamespace("httr"))
    stop("package `httr` is required")

  # generate ISO format start/end date from year
  startdate <- sprintf("%s-01-01", year)
  enddate <- sprintf("%s-12-31", year)

  message(sprintf('Downloading NOAA GHCND data for %s over interval %s to %s...',
                  stationid, startdate, enddate))

  # build multi-datatype URL
  datatypeids <- sprintf("&datatypeid=%s", datatypeid)
  datatypeid.url <- paste0(datatypeids, collapse="&")

  # construct GET request
  r <- httr::GET(url = paste0(sprintf(
    "https://www.ncdc.noaa.gov/cdo-web/api/v2/data?datasetid=GHCND&stationid=%s&startdate=%s&enddate=%s&limit=1000",
    stationid,
    startdate,
    enddate), datatypeid.url), httr::add_headers(token = apitoken))

  # retrieve content
  r.content <- httr::content(r, as = "text", encoding = "UTF-8")

  # convert JSON to data.frame
  d <- jsonlite::fromJSON(r.content)

  if (length(d$results) == 0) {
    message("empty result set")
    return(NULL)
  }

  if (nrow(d$results) == 1000)
    message("maximum record limit reached (n = 1000)")

  return(d$results)
}

#' Get Global Historical Climatology Network Daily (GHCND) data from NOAA API for given datatype(s), station IDs and years.
#'
#' @description Obtain daily climatic summary data for a set of station IDs, years, and datatypes.
#'
#' Note that typically results from the NOAA API are limited to 1000 records. However, by "chunking" up data into individual station*year*datatypeid combinations, record results generally do not exceed 365 records for daily summaries.
#'
#' In order to use this function, you must obtain an API token from this website: https://www.ncdc.noaa.gov/cdo-web/token
#'
#' @param stations Station ID (e.g. \code{GHCND:USC00388786})
#' @param years One or more years (e.g. 2017:2020)
#' @param datatypeids One or more NOAA GHCND data type IDs (e.g \code{c("PRCP","SNOW")})
#' @param apitoken API key token for NOAA NCDC web services (https://www.ncdc.noaa.gov/cdo-web/token)
#'
#' @return A data.frame containing the GHCND data requested (limit 1000 records)
#' @export get_NOAA_GHCND
#'
#' @examples
#'
#' #' ## in order to use this function, you must obtain an API token from this website:
#' ##  https://www.ncdc.noaa.gov/cdo-web/token
#'
#' # get_NOAA_GHCND(c("GHCND:USC00388786", "GHCND:USC00388787"),
#' #                years = 2017:2020,
#' #                datatypeids = c("PRCP","SNOW"),
#' #                apitoken = "yourtokenhere")
#'
get_NOAA_GHCND <- function(stations, years, datatypeids, apitoken) {
  do.call('rbind', lapply(stations, function(s)
    do.call('rbind', lapply(years, function(y)
      do.call('rbind', lapply(datatypeids, function(d)
       .get_NOAA_GHCND_by_stationyear(s, y, d, apitoken)))))))
}
