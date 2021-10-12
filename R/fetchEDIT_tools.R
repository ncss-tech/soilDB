# fetchEDIT_tools: tools for getting information from Jornada EDIT web services
# More information: https://edit.jornada.nmsu.edu/resources/esd

###
### New function to build a JSON service URL that matches XYZ criteria
###
###
#' Make Ecological Dynamics Interpretive Tool (EDIT) web services URL
#'
#' @description Construct a URL for Ecological Dynamics Interpretive Tool (EDIT) web services (`https://edit.jornada.nmsu.edu/services/...`) to return PDF, TXT or JSON results.
#'
#' @details See the following official EDIT developer resources to see which endpoints are available for Ecological Site Description (ESD) or Ecological Site Group (ESG) catalogs:
#'
#' - \url{https://edit.jornada.nmsu.edu/resources/esd}
#'
#' - \url{https://edit.jornada.nmsu.edu/resources/esg}
#'
#' @param src One of: `descriptions`, `downloads`, `plant-community-tables`, `models`, `keys`
#' @param catalog Catalog ID. One of: `esd` or `esg`
#' @param geoUnit Geographic unit ID. For example: `022A`
#' @param ecoclass Ecological class ID. For example: `F022AX101CA`
#' @param landuse Optional: Used only for `src = "plant-community-tables"`
#' @param state Optional: Used only for `src = "plant-community-tables"`
#' @param community Optional: Used only for `src = "plant-community-tables"`
#' @param key Optional: Key number. All keys will be returned if not specified.
#' @param endpoint Optional: Specific endpoint e.g. `overview.json`, `class-list.json`, `soil-features.json`
#' @param querystring Optional: Additional request parameters specified as a query string `?param1=value&param2=value`. 
#'
#' @return A character vector containing URLs with specified parameters. This function is vectorized.
#'
#' @seealso get_EDIT_ecoclass_by_geoUnit
#' @export
#'
#' @examples
#' # url for all geoUnit keys as PDF
#' make_EDIT_service_URL(src = "descriptions",
#'                       catalog = "esd",
#'                       geoUnit = "039X")
#'
#' # url for a single key within geoUnit as PDF
#' make_EDIT_service_URL(src = "descriptions",
#'                       catalog = "esd",
#'                       geoUnit = "039X",
#'                       key = "1")
#'
#' # query for "full" description in JSON
#' desc <-  make_EDIT_service_URL(src = "descriptions",
#'                                catalog = "esd",
#'                                geoUnit = "039X",
#'                                endpoint = "R039XA109AZ.json")
#'
#' # query for "overview"
#' desc_ov <- make_EDIT_service_URL(src = "descriptions",
#'                                  catalog = "esd",
#'                                  geoUnit = "039X",
#'                                  ecoclass = "R039XA109AZ",
#'                                  endpoint = "overview.json")
#'
#' # query for specific section, e.g. "water features"
#' desc_wf <- make_EDIT_service_URL(src = "descriptions",
#'                                  catalog = "esd",
#'                                  geoUnit = "039X",
#'                                  ecoclass = "R039XA109AZ",
#'                                  endpoint = "water-features.json")
#'
#' # construct the URLs -- that is a query essentially
#' # then download the result with read_json
#'
#' #full <- jsonlite::read_json(desc)
#' #overview <- jsonlite::read_json(desc_ov)
#' #waterfeature <- jsonlite::read_json(desc_wf)
#'
make_EDIT_service_URL <- function(src = c("descriptions", "downloads",
                                          "plant-community-tables",
                                          "models", "keys"),
                                  catalog = c("esd", "esg"),
                                  geoUnit = NULL,
                                  ecoclass = NULL,
                                  landuse = NULL,
                                  state = NULL,
                                  community = NULL,
                                  key = NULL,
                                  endpoint = NULL,
                                  querystring = NULL) {
  # base URL
  base_url <- "https://edit.jornada.nmsu.edu"

  # root services URL
  service_url <- "services"


  src <- match.arg(src, c("descriptions", "downloads",
                          "plant-community-tables",
                          "models", "keys"))

  catalog <- match.arg(catalog, c("esd", "esg"))

  built_url <- file.path(base_url, service_url, src, catalog)

  if (!is.null(geoUnit)) {
    built_url <- file.path(built_url, geoUnit)
  }

  if (!is.null(ecoclass)) {
    built_url <- file.path(built_url, ecoclass)

    if (all(!is.null(landuse) && !is.null(state) && !is.null(community))) {
      built_url <- file.path(built_url, landuse, state, community)
    }
  }

  if (!is.null(endpoint)) {
    built_url <- file.path(built_url, endpoint)
  }

  # key PDF override
  if (!is.null(key)) {
    built_url <- paste0(file.path(base_url, service_url, src, catalog, geoUnit, key), ".pdf")
  } else if (is.null(key) && is.null(ecoclass) && is.null(endpoint)) {
    built_url <- paste0(file.path(base_url, service_url, src, catalog, geoUnit), ".pdf")
  }

  # append query string
  if (!is.null(querystring)) {
    built_url <- paste0(built_url,
                        ifelse(startsWith(querystring, "?"), "", "?"),
                        querystring)
  }

  built_url
}

#' Get Ecological Dynamics Information Tool (EDIT) ecological sites by catalog (ESD/ESG) and MLRA 
#'  
#' @description  Data are accessed via Ecological Dynamics Interpretive Tool (EDIT) web services: https://edit.jornada.nmsu.edu/resources/esd. `geoUnit` refers to MLRA codes, possibly with a leading zero and trailing "X" for two digit MLRA symbols.
#'
#' @param geoUnit A character vector of `geoUnit` codes e.g. `c("018X","022A")` for MLRAs 18 and 22A.
#' @param catalog Catalog ID. One of: `"esd"` or `"esg"`
#'
#' @return A `data.frame` containing: `geoUnit`, `id`, `legacyId`, `name`. `NULL` if no result.
#'
#' @export
#'
#' @examples
#' \donttest{
#'  if(requireNamespace("curl") &
#'    curl::has_internet()) {
#'    get_EDIT_ecoclass_by_geoUnit(c("018X","022A"))
#'  }
#' }
get_EDIT_ecoclass_by_geoUnit <- function(geoUnit, catalog = c("esd", "esg")) {
  # essentially vectorized application of make_EDIT_service_URL / read_json and
  # then combine by rows
  data.frame(do.call('rbind', lapply(geoUnit, function(aUnit){
    desclist <-  make_EDIT_service_URL(src = "downloads",
                                       catalog = catalog,
                                       geoUnit = aUnit,
                                       endpoint = "class-list.json")
    thelist <- try(jsonlite::read_json(desclist))
    if (inherits(thelist, 'try-error')) return(NULL)
    do.call('rbind', lapply(thelist[[2]], data.frame))
  })))
}
