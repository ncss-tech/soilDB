# currently only queries SoilWeb for mapunit-level data


#' Get SSURGO Data via Spatial Query
#' 
#' Get SSURGO Data via Spatial Query to SoilWeb
#' 
#' Data are currently available from SoilWeb. These data are a snapshot of the
#' "official" data. The snapshot date is encoded in the "soilweb_last_update"
#' column in the function return value. Planned updates to this function will
#' include a switch to determine the data source: "official" data via USDA-NRCS
#' servers, or a "snapshot" via SoilWeb.
#' 
#' @param bbox a bounding box in WGS84 geographic coordinates, see examples
#' @param coords a coordinate pair in WGS84 geographic coordinates, see
#' examples
#' @param what data to query, currently ignored
#' @param source the data source, currently ignored
#' @return The data returned from this function will depend on the query style.
#' See examples below.
#' @note This function should be considered experimental; arguments, results,
#' and side-effects could change at any time. SDA now supports spatial queries,
#' consider using \code{\link{SDA_query_features}} instead.
#' @author D.E. Beaudette
#' @keywords manip
#' @examples
#' 
#' \donttest{
#' if(requireNamespace("curl") &
#'     curl::has_internet()) {
#'     
#'     # query by bbox
#'     SoilWeb_spatial_query(bbox=c(-122.05, 37, -122, 37.05))
#'     
#'     # query by coordinate pair
#'     SoilWeb_spatial_query(coords=c(-121, 38))
#' }
#' }
#' 
#' @export SoilWeb_spatial_query
SoilWeb_spatial_query <- function(bbox=NULL, coords=NULL, what='mapunit', source='soilweb') {
  
  # check for required packages
  if(!requireNamespace('jsonlite', quietly = TRUE))
    stop('please install the `jsonlite` package', call.=FALSE)
  
  # no factors in resulting DFs
  options(stringsAsFactors=FALSE)
  
  # sanity-check: user must supply some kind of criteria
  if(missing(coords) & missing(bbox))
    stop('you must provide some filtering criteria')
  
  # can't provide both coords and bbox
  if(! missing(coords) & ! missing(bbox))
    stop('query cannot include both bbox and point coordinates')
  
  # init empty filter
  f <- vector()
  
  # init empty pieces
  res <- NULL
  
  # process filter components
  if(!missing(coords)) {
    f <- c(f, paste('&lon=', coords[1], '&lat=', coords[2], sep=''))
  }
  
  if(!missing(bbox)) {
    bbox <- paste(bbox, collapse=',')
    f <- c(f, paste('&bbox=', bbox, sep=''))
  }
  
  # build URL
  the.url <- paste('https://casoilresource.lawr.ucdavis.edu/soil_web/api/ssurgo.php?what=mapunit', f, sep='')
  
  ## TODO: this isn't robust
  # load data from JSON
  suppressWarnings(try(res <- jsonlite::fromJSON(the.url), silent=TRUE))
  
  # report missing data
  if(is.null(res)) {
    stop('query returned no data', call.=FALSE)
  }
  
  return(res)
}
