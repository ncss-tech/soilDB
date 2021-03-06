# code to make a data.tree of EDIT API

library(xml2)
library(rvest)

# list all the available /services in the developer documentation

path <- read_html("https://edit.jornada.nmsu.edu/resources/esd/") %>%
  html_nodes("span") %>%
  html_text() %>%
  (function(x) {x[grepl("^/services.*", x)]})()
n <- data.tree::as.Node(data.frame(pathString = path), pathDelimiter = '/')
print(n)

#>                                                           levelName
#> 1  services
#> 2   ¦--descriptions
#> 3   ¦   °--{catalog}
#> 4   ¦       ¦--{geoUnit}
#> 5   ¦       ¦   ¦--{ecoclass}.json
#> 6   ¦       ¦   °--{ecoclass}
#> 7   ¦       ¦       ¦--overview.json
#> 8   ¦       ¦       ¦--climatic-features.json
#> 9   ¦       ¦       ¦--ecological-dynamics.json
#> 10  ¦       ¦       ¦--general-information.json
#> 11  ¦       ¦       ¦--interpretations.json
#> 12  ¦       ¦       ¦--physiographic-features.json
#> 13  ¦       ¦       ¦--reference-sheet.json
#> 14  ¦       ¦       ¦--soil-features.json
#> 15  ¦       ¦       ¦--supporting-information.json
#> 16  ¦       ¦       ¦--water-features.json
#> 17  ¦       ¦       °--{section}.pdf
#> 18  ¦       °--{geoUnit}.pdf
#> 19  ¦--downloads
#> 20  ¦   °--{catalog}
#> 21  ¦       ¦--{geoUnit}
#> 22  ¦       ¦   ¦--class-list.json
#> 23  ¦       ¦   ¦--class-list.txt
#> 24  ¦       ¦   ¦--climatic-features.txt
#> 25  ¦       ¦   ¦--landforms.txt
#> 26  ¦       ¦   ¦--physiographic-interval-properties.txt
#> 27  ¦       ¦   ¦--physiographic-nominal-properties.txt
#> 28  ¦       ¦   ¦--physiographic-ordinal-properties.txt
#> 29  ¦       ¦   ¦--annual-production.txt
#> 30  ¦       ¦   ¦--forest-overstory.txt
#> 31  ¦       ¦   ¦--forest-understory.txt
#> 32  ¦       ¦   ¦--rangeland-plant-composition.txt
#> 33  ¦       ¦   ¦--soil-surface-cover.txt
#> 34  ¦       ¦   ¦--soil-parent-material.txt
#> 35  ¦       ¦   ¦--soil-interval-properties.txt
#> 36  ¦       ¦   ¦--soil-nominal-properties.txt
#> 37  ¦       ¦   ¦--soil-ordinal-properties.txt
#> 38  ¦       ¦   ¦--soil-profile-properties.txt
#> 39  ¦       ¦   ¦--soil-surface-textures.txt
#> 40  ¦       ¦   ¦--model-state-narratives.txt
#> 41  ¦       ¦   °--model-transition-narratives.txt
#> 42  ¦       ¦--geo-unit-list.json
#> 43  ¦       ¦--class-list.txt
#> 44  ¦       °--geo-unit-list.txt
#> 45  ¦--plant-community-tables
#> 46  ¦   °--{catalog}
#> 47  ¦       °--{geoUnit}
#> 48  ¦           °--{ecoclass}
#> 49  ¦               °--{landUse}
#> 50  ¦                   °--{state}
#> 51  ¦                       °--{community}
#> 52  ¦                           ¦--annual-production.json
#> 53  ¦                           ¦--canopy-structure.json
#> 54  ¦                           ¦--forest-overstory.json
#> 55  ¦                           ¦--forest-understory.json
#> 56  ¦                           ¦--ground-cover.json
#> 57  ¦                           ¦--rangeland-plant-composition.json
#> 58  ¦                           ¦--snag-count.json
#> 59  ¦                           ¦--soil-surface-cover.json
#> 60  ¦                           °--woody-ground-cover.json
#> 61  ¦--models
#> 62  ¦   °--{catalog}
#> 63  ¦       °--{geoUnit}
#> 64  ¦           °--{ecoclass}
#> 65  ¦               ¦--states.json
#> 66  ¦               °--transitions.json
#> 67  °--keys
#> 68      °--{catalog}
#> 69          °--{geoUnit}
#> 70              °--{key}.pdf

###
### New function to build a JSON service URL that matches XYZ criteria
###
#' make_EDIT_service_URL
#'
#' @param src One of: `"descriptions", "downloads", "plant-community-tables", "models", "keys"``
#' @param catalog Catalog ID; default `"esd"`
#' @param geoUnit Geographic unit ID e.g. `022A`
#' @param ecoclass Ecological class ID e.g. `F022AX101CA`
#' @param landuse Optional: Used only for `src = "plant-community-tables"`
#' @param state Optional: Used only for `src = "plant-community-tables"`
#' @param community Optional: Used only for `src = "plant-community-tables"`
#' @param key Optional: Key number. All keys will be returned if not specified.
#' @param endpoint Optional: Specific endpoint e.g. `"overview.json"`, `"class-list.json"`
#' @param querystring Optional: Additional (optional) request parameters for e.g. `/services/downloads/{catalog}/{geoUnit}/class-list.json` endpoint or PDF key output.
#'
#' @return
#' @export
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
                          catalog = "esd",
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

  catalog <- match.arg(catalog, c("esd"))

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


#' Get data.frame of all ecoclass for multiple EDIT geoUnit
#'
#' Supply a vector of target `geoUnit`. Data are accessed via Ecological Dynamics Interpretive Tool (EDIT) web services: https://edit.jornada.nmsu.edu/resources/esd
#'
#' @param geoUnit A character vector of `geoUnit` codes e.g. `c("018X","022A")` for MLRAs 18 and 22A.
#'
#' @return A `data.frame` containing: `geoUnit`, `id`, `legacyId`, `name`
#'
#' @export
#'
#' @examples
#' \donttest{
#' get_EDIT_classlist(c("018X","022A"))
#' }
get_EDIT_classlist <- function(geoUnit) {
  # essentially vectorized application of make_EDIT_service_URL / read_json and
  # then combine by rows
  data.frame(do.call('rbind', lapply(geoUnit, function(aUnit){
    desclist <-  make_EDIT_service_URL(src = "downloads",
                                    catalog = "esd",
                                    geoUnit = aUnit,
                                    endpoint = "class-list.json")
    thelist <- jsonlite::read_json(desclist)
    do.call('rbind', lapply(thelist[[2]], data.frame))
  })))
}
