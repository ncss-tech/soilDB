#' Get NASIS Record IDs from Soil Data Access
#'
#' Query the Soil Data Access (SDA) API for NASIS record identifiers
#' (`nasiscoiid`, `nasischiid`) associated with map units, components, or
#' horizons. These identifiers are available in SDA beginning with NASIS version
#' 7.4.3 and are useful for linking SDA data to NASIS backend records.
#'
#' This function is intended for use with SDA (`dsn = NULL`). When used with
#' local SSURGO snapshots or other data sources, these columns may not be
#' present.
#'
#' @param x A character vector of keys to match (`mukey`, `cokey`, or `chkey`).
#' @param by A character string indicating the type of key provided in `x`. Must
#'   be one of `"mukey"`, `"cokey"`, or `"chkey"`.
#' @param include_chorizon Logical. If `TRUE`, join results to `chorizon` table
#'   to include `"nasischiid"`. Default: `FALSE` but always `TRUE` for
#'   `by="chkey"`.
#' @param query_string Logical. If `TRUE`, return the SQL query string instead
#'   of executing it. Default: `FALSE`.
#' @param dsn Optional. Path to a local database or connection object. If `NULL`
#'   (default), the SDA web service is used.
#'
#' @return A `data.frame` containing `mukey`, `cokey`, `chkey`, `nasiscoiid`,
#'   and `nasischiid`, or a character string if `query_string = TRUE`.
#'
#' @note These NASIS record ID columns are only available via SDA after the
#'   October 1, 2025 refresh. They are not included in SSURGO snapshot data
#'   from Web Soil Survey unless manually added by the user.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get NASIS component IDs for a set of map unit keys
#' get_SDA_NASIS_key(c("469093"), by = "mukey")
#' 
#' # Get NASIS component and chorizon IDs for a set of map unit keys
#' get_SDA_NASIS_key(c("469093"), by = "mukey", include_chorizon = TRUE)
#' }
get_SDA_NASIS_key <- function(x,
                              by = "mukey",
                              include_chorizon = FALSE,
                              query_string = FALSE,
                              dsn = NULL) {

  by <- match.arg(tolower(by), c("mukey", "cokey", "chkey"))

  INCLUDE_CHIID <- ""
  INCLUDE_CHORIZON <- ""
  
  if (by %in% c("mukey", "cokey")) {
    WHERE <- paste(paste0("component.", by),
                   "IN",
                   format_SQL_in_statement(x))
  } else if (by %in% c("chkey")) {
    WHERE <- paste(paste0("chorizon", by),
                   "IN",
                   format_SQL_in_statement(x))
    include_chorizon <- TRUE
  }

  if (include_chorizon) {
    INCLUDE_CHIID <- ", chorizon.chkey, nasischiid"
    INCLUDE_CHORIZON <- "LEFT JOIN chorizon ON chorizon.cokey = component.cokey"
  }
  
  q <- .gluelite(
    "SELECT component.mukey, component.cokey, nasiscoiid {INCLUDE_CHIID}
    FROM component {INCLUDE_CHORIZON}
    WHERE {WHERE}"
  )

  if (isTRUE(query_string)) {
    return(q)
  }

  SDA_query(q, dsn = dsn)
}
