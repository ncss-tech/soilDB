# currently only queries SoilWeb for mapunit-level data


#' @title Get SSURGO Data via Spatial Query
#' 
#' @description Get SSURGO Data via Spatial Query to SoilWeb
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
#' @note SDA now supports spatial queries, consider using [SDA_spatialQuery()] instead.
#' @author D.E. Beaudette
#' @keywords manip
#' @examplesIf curl::has_internet() && requireNamespace("httr", quietly = TRUE)
#' 
#' \donttest{
#'     # query by bbox
#'     SoilWeb_spatial_query(bbox=c(-122.05, 37, -122, 37.05))
#'     
#'     # query by coordinate pair
#'     SoilWeb_spatial_query(coords=c(-121, 38))
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
  if(missing(coords) & missing(bbox)) {
    stop('you must provide some filtering criteria')
  }
    
  # can't provide both coords and bbox
  if(! missing(coords) & ! missing(bbox)) {
    stop('query cannot include both bbox and point coordinates')
  }
  
  # process filter components
  if(!missing(coords)) {
    f <- paste0('&lon=', coords[1], '&lat=', coords[2])
  }
  
  if(!missing(bbox)) {
    bbox <- paste(bbox, collapse=',')
    f <- paste0('&bbox=', bbox)
  }
  
  # build URL
  the.url <- paste0('https://casoilresource.lawr.ucdavis.edu/soil_web/api/ssurgo.php?what=mapunit', f)
  
  # attempt to load data from URL/JSON
  # note: this may fail when done over gov VPN
  suppressWarnings(res <- try(jsonlite::fromJSON(the.url), silent=TRUE))
  
  # trap errors
  if(inherits(res, 'try-error')) {
    message(as.character(res))
    return(NULL)
  }
  
  # report no results
  if(inherits(res, 'logical')) {
    message('query returned no data')
    return(NULL)
  }
  
  return(res)
}
