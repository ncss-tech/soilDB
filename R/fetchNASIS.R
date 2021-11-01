# convenient interface to local NASIS data
# from: pedons | components | lab | ???
# ... : arguments passed on to helper functions


#' Get a pedon or component data SoilProfileCollection from NASIS
#'
#' Fetch commonly used site/pedon/horizon data or component from NASIS,
#' returned as a SoilProfileCollection object.
#'
#' This function imports data from NASIS into R as a
#' \code{SoilProfileCollection} object. It "flattens" NASIS pedon and component
#' tables, including their child tables, into several more easily manageable
#' data frames. Primarily these functions access the local NASIS database using
#' an ODBC connection. However using the \code{fetchNASIS()} argument
#' \code{from = "pedon_report"}, data can be read from the NASIS Report
#' 'fetchNASIS', as either a txt file or url. The primary purpose of
#' \code{fetchNASIS(from = "pedon_report")} is to facilitate importing datasets
#' larger than 8000+ pedons/components.
#'
#' The value of \code{nullFragsAreZero} will have a significant impact on the
#' rock fragment fractions returned by fetchNASIS. Set \code{nullFragsAreZero =
#' FALSE} in those cases where there are many data-gaps and \code{NULL} rock
#' fragment values should be interpreted as \code{NULL}. Set
#' \code{nullFragsAreZero = TRUE} in those cases where \code{NULL} rock
#' fragment values should be interpreted as 0.
#'
#' This function attempts to do most of the boilerplate work when extracting
#' site/pedon/horizon or component data from a local NASIS database. Pedons
#' that are missing horizon data, or have errors in their horizonation are
#' excluded from the returned object, however, their IDs are printed on the
#' console. Pedons with combination horizons (e.g. B/C) are erroneously marked
#' as errors due to the way in which they are stored in NASIS as two
#' overlapping horizon records.
#'
#' Tutorials:
#'
#'  - [fetchNASIS Pedons Tutorial](http://ncss-tech.github.io/AQP/soilDB/fetchNASIS-mini-tutorial.html)
#'  - [fetchNASIS Components Tutorial](http://ncss-tech.github.io/AQP/soilDB/NASIS-component-data.html)
#'
#' @aliases fetchNASIS get_phorizon_from_NASIS_db
#' get_component_copm_data_from_NASIS_db
#' get_component_horizon_data_from_NASIS_db
#' get_component_correlation_data_from_NASIS_db
#' get_component_cogeomorph_data_from_NASIS_db
#' get_component_esd_data_from_NASIS_db
#' get_component_otherveg_data_from_NASIS_db get_copedon_from_NASIS_db
#' get_legend_from_NASISget_lmuaoverlap_from_NASIS get_mapunit_from_NASIS
#' get_projectmapunit_from_NASIS get_component_diaghz_from_NASIS_db
#' get_mutext_from_NASIS_db get_phfmp_from_NASIS_db get_RMF_from_NASIS_db
#' get_concentrations_from_NASIS_db
#' get_cotext_from_NASIS_db
#' @param from determines what objects should fetched? ('pedons' | 'components' | 'pedon_report')
#' @param url string specifying the url for the NASIS pedon_report (default:
#' `NULL`)
#' @param SS fetch data from the currently loaded selected set in NASIS or from
#' the entire local database (default: `TRUE`)
#' @param rmHzErrors should pedons with horizon depth errors be removed from
#' the results? (default: `TRUE`)
#' @param nullFragsAreZero should fragment volumes of `NULL` be interpreted as `0`?
#' (default: `TRUE`), see details
#' @param soilColorState which colors should be used to generate the
#' convenience field `soil_color`? (`'moist'` or `'dry'`)
#' @param lab should the `phlabresults` child table be fetched with
#' site/pedon/horizon data (default: `FALSE`)
#' @param fill include pedon or component records without horizon data in result? (default: `FALSE`)
#' @param stringsAsFactors logical: should character vectors be converted to
#' factors? This argument is passed to the `uncode()` function. It does not
#' convert those vectors that have been set outside of `uncode()` (i.e. hard
#' coded).
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: `NULL`
#' @return A SoilProfileCollection object
#' @author D. E. Beaudette, J. M. Skovlin, S.M. Roecker, A.G. Brown
#' @export fetchNASIS
fetchNASIS <- function(from = 'pedons',
                       url = NULL,
                       SS = TRUE,
                       rmHzErrors = TRUE,
                       nullFragsAreZero = TRUE,
                       soilColorState = 'moist',
                       lab = FALSE,
                       fill = FALSE,
                       stringsAsFactors = default.stringsAsFactors(),
                       dsn = NULL) {

  res <- NULL

  # TODO: do we need _View_1 tables in the sqlite table snapshot? Could be handy for
  #       specialized selected sets crafted by NASIS/CVIR stuff; currently you are allowed
  #       to specify the selected set for a SQLite database, and I suppose the convention
  #       should be for those tables to be there, even if empty

  # if (!is.null(dsn))
  #   SS <- FALSE

  # sanity check
  if (!from %in% c('pedons', 'components', 'pedon_report')) {
    stop('Must specify: pedons, components or pedon_report', call. = FALSE)
  }

  if (from == 'pedons') {
    # pass arguments through
    res <- .fetchNASIS_pedons(SS = SS,
                              fill = fill, 
                              rmHzErrors = rmHzErrors,
                              nullFragsAreZero = nullFragsAreZero,
                              soilColorState = soilColorState,
                              lab = lab,
                              stringsAsFactors = stringsAsFactors,
                              dsn = dsn)
  }

  if (from == 'components') {
    # pass arguments through
    res <- .fetchNASIS_components(SS = TRUE,
                                  rmHzErrors = rmHzErrors,
                                  nullFragsAreZero = nullFragsAreZero,
                                  fill = fill,
                                  stringsAsFactors = stringsAsFactors,
                                  dsn = dsn)
  }

  if (from == 'pedon_report') {
    # pass arguments through
    res <- .fetchNASIS_report(url              = url,
                              rmHzErrors       = rmHzErrors,
                              nullFragsAreZero = nullFragsAreZero,
                              soilColorState   = soilColorState,
                              stringsAsFactors = stringsAsFactors
                              )
  }

  return(res)

}
