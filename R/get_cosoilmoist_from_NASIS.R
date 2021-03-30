#' Read and Flatten the Component Soil Moisture Tables
#'
#' Read and flatten the component soil moisture month tables from a local NASIS
#' Database.
#'
#' The component soil moisture tables within NASIS house monthly data on
#' flooding, ponding, and soil moisture status. The soil moisture status is
#' used to specify the water table depth for components (e.g. \code{status ==
#' "Moist"}).
#'
#' @param SS fetch data from the currently loaded selected set in NASIS or from
#' the entire local database (default: `TRUE`)
#' @param impute replace missing (i.e. `NULL`) values with `"Not_Populated"` for
#' categorical data, or the "RV" for numeric data or `201` cm if the "RV" is also
#' `NULL` (default: `TRUE`)
#' @param stringsAsFactors logical: should character vectors be converted to
#' factors? This argument is passed to the `uncode()` function. It does not
#' convert those vectors that have set outside of `uncode()` (i.e. hard coded).
#' The 'factory-fresh' default is TRUE, but this can be changed by setting
#' options(`stringsAsFactors = FALSE`)
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: `NULL`
#' @return A data.frame.
#' @note This function currently works only on Windows.
#' @author S.M. Roecker
#' @seealso \link{fetchNASIS}, \link{get_cosoilmoist_from_NASISWebReport},
#' \link{get_cosoilmoist_from_SDA}, \code{get_comonth_from_SDA}
#' @keywords manip
#' @examples
#'
#' \donttest{
#' if(local_NASIS_defined()) {
#'  # load cosoilmoist (e.g. water table data)
#'  test <- try(get_cosoilmoist_from_NASIS())
#'
#'  # inspect
#'  if(!inherits(test, 'try-error')) {
#'    head(test)
#'  }
#' }
#' }
#' @export get_cosoilmoist_from_NASIS
get_cosoilmoist_from_NASIS <- function(SS = TRUE,
                                       impute = TRUE,
                                       stringsAsFactors = default.stringsAsFactors(),
                                       dsn = NULL) {


  q.cosoilmoist <- "SELECT dmuiidref AS dmuiid, coiid, compname, comppct_r, drainagecl, month, flodfreqcl, floddurcl, pondfreqcl, ponddurcl, cosoilmoistiid, soimoistdept_l, soimoistdept_r, soimoistdept_h, soimoistdepb_l, soimoistdepb_r, soimoistdepb_h, soimoiststat

  FROM component_View_1 co LEFT OUTER JOIN
       comonth_View_1 com ON com.coiidref = co.coiid LEFT OUTER JOIN
       cosoilmoist_View_1 cosm ON cosm.comonthiidref = com.comonthiid

  ORDER BY dmuiid, comppct_r DESC, compname, month, soimoistdept_r
  ;"

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.cosoilmoist <- gsub(pattern = '_View_1', replacement = '', x = q.cosoilmoist, fixed = TRUE)
  }

  # exec query
  d.cosoilmoist <- dbQueryNASIS(channel, q.cosoilmoist)

  # recode metadata domains
  d.cosoilmoist <- uncode(d.cosoilmoist, stringsAsFactors = stringsAsFactors, dsn = dsn)

  # prep dataset: rename columns, impute empty values, stringsAsFactors
  d.cosoilmoist <- suppressWarnings(.cosoilmoist_prep(d.cosoilmoist, impute = impute, stringsAsFactors = stringsAsFactors))

  # done
  return(d.cosoilmoist)
}
