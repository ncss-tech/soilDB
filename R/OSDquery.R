
# https://www.postgresql.org/docs/9.5/static/textsearch-controls.html
# these are all parameters expected by the SoilWeb OSD Fulltext search

#' Search full text of Official Series Description on SoilWeb 
#' 
#' @description This is the R interface to \href{https://casoilresource.lawr.ucdavis.edu/osd-search/}{OSD search by Section} and \href{https://casoilresource.lawr.ucdavis.edu/osd-search/search-entire-osd.php}{OSD Search} APIs provided by SoilWeb.
#' 
#' OSD records are searched with the \href{https://www.postgresql.org/docs/9.5/textsearch.html}{PostgreSQL fulltext indexing} and query system (\href{https://www.postgresql.org/docs/9.5/datatype-textsearch.html}{syntax details}). Each search field (except for the "brief narrative" and MLRA) corresponds with a section header in an OSD. The results may not include every OSD due to formatting errors and typos. Results are scored based on the number of times search terms match words in associated sections. 
#' 
#' @param everything search entire OSD text (default is NULL), `mlra` may also be specified, all other arguments are ignored
#' @param mlra a comma-delimited string of MLRA to search ('17,18,22A')
#' @param taxonomic_class search family level classification
#' @param typical_pedon search typical pedon section
#' @param brief_narrative search brief narrative
#' @param ric search range in characteristics section
#' @param use_and_veg search use and vegetation section
#' @param competing_series search competing series section
#' @param geog_location search geographic setting section
#' @param geog_assoc_soils search geographically associated soils section
#' 
#' @details 
#' See \href{https://casoilresource.lawr.ucdavis.edu/osd-search/}{this webpage} for more information.
#'
#'  - family level taxa are derived from SC database, not parsed OSD records
#'  
#'  - MLRA are derived via spatial intersection (SSURGO x MLRA polygons)
#'  
#'  - MLRA-filtering is only possible for series used in the current SSURGO snapshot (component name)
#'  
#'  - logical AND: &
#'  
#'  - logical OR: |
#'  
#'  - wildcard, e.g. rhy-something rhy:*
#'  
#'  - search terms with spaces need doubled single quotes: ''san joaquin''
#'  
#'  - combine search terms into a single expression: (grano:* | granite)
#' 
#' Related documentation can be found in the following tutorials
#'  - [overview of all soil series query functions](http://ncss-tech.github.io/AQP/soilDB/soil-series-query-functions.html)
#'  - [competing soil series](https://ncss-tech.github.io/AQP/soilDB/competing-series.html)
#'  - [siblings](https://ncss-tech.github.io/AQP/soilDB/siblings.html)
#'
#'
#' @references \url{https://www.nrcs.usda.gov/wps/portal/nrcs/detailfull/soils/home/?cid=nrcs142p2_053587}
#'
#' @author D.E. Beaudette
#'
#' @note SoilWeb maintains a snapshot of the Official Series Description data.
#'
#' @seealso \code{\link{fetchOSD}, \link{siblings}, \link{fetchOSD}}
#'
#' @keywords manip
#'
#' @return a \code{data.frame} object containing soil series names that match patterns supplied as arguments.
#' @export
#'
#' @examples
#'
#'
#' \donttest{
#' if(requireNamespace("curl") &
#'    curl::has_internet() &
#'    require(aqp)) {
#'
#'   # find all series that list Pardee as a geographically associated soil.
#'   s <- OSDquery(geog_assoc_soils = 'pardee')
#'
#'   # get data for these series
#'   x <- fetchOSD(s$series, extended = TRUE, colorState = 'dry')
#'
#'   # simple figure
#'   par(mar=c(0,0,1,1))
#'   plot(x$SPC)
#' }
#' }
#' 
OSDquery <- function(everything = NULL, mlra='', taxonomic_class='', typical_pedon='', brief_narrative='', ric='', use_and_veg='', competing_series='', geog_location='', geog_assoc_soils='') {
 
  # check for required packages
  if(!requireNamespace('httr', quietly=TRUE) | !requireNamespace('jsonlite', quietly=TRUE))
    stop('please install the `httr` and `jsonlite` packages', call.=FALSE)

  # sanity checks
  
  # mode selection
  if(is.null(everything)) {
    
    ## searching by sections
    
    # build parameters list
    parameters=list(
      json = 1,
      mlra = mlra,
      taxonomic_class = taxonomic_class, 
      typical_pedon = typical_pedon, 
      brief_narrative = brief_narrative, 
      ric = ric, 
      use_and_veg = use_and_veg, 
      competing_series = competing_series, 
      geog_location = geog_location, 
      geog_assoc_soils = geog_assoc_soils
    )
    
    # API URL
    # note: this is the load-balancer
    u <- 'https://casoilresource.lawr.ucdavis.edu/osd-search/index.php'
    
  } else {
    
    ## searching entire OSD text
    
    # build parameters list
    parameters=list(
      json = 1,
      query = everything,
      mlra = mlra
    )
    
    # API URL
    # note: this is the load-balancer
    u <- 'https://casoilresource.lawr.ucdavis.edu/osd-search/search-entire-osd.php'
  }
 
  # submit via POST
  res <- httr::POST(u, body = parameters, encode='form')
  
  # TODO: figure out what an error state looks like
  # trap errors, likely related to SQL syntax errors
  request.status <- try(httr::stop_for_status(res), silent = TRUE)

  # the result is JSON
  # should simplify to data.frame nicely
  r.content <- httr::content(res, as = 'text', encoding = 'UTF-8')
  d <- jsonlite::fromJSON(r.content)

  # results will either be: data.frame, empty list, or NULL

  # ensure result is either data.frame or NULL
  if(inherits(d, 'list') & length(d) < 1)
    return(NULL)

  return(d)
}


