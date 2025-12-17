#' Get Pedon Horizon Bulk Density Summmary From NASIS
#'
#' Get data from the Pedon Horizon Bulk Density (`phdb`) child table of
#' Pedon Horizon.
#'
#' @param SS Fetch data from the currently loaded selected set in NASIS
#'   (default; `TRUE`) or from the entire Local database
#' @param dsn Optional: path or _DBIConnection_ to
#'   \link[=NASISLocalDatabase]{local database containing NASIS table
#'   structure}; default: `NULL`
#'
#' @returns data.frame
#' @export
get_phdb_from_NASIS_db <- function(SS = TRUE, dsn = NULL) {
  q <- "SELECT
          peiidref AS peiid, phiidref AS phiid,
          phdb_View_1.seqnum, bddepthtop, bddepthbottom, bdmethod, datacollector,
          samplevolfieldmoist, totalsamplewtfm, totalsamplewtairdry,
          coarsefragwtfm, coarsefragwtairdry, coarsefragdensity, coarsefragvolmeasured,
          subsamplewtairdry, subsamplewtod,
          obsgrsoimoist, obsgravsoilmoistfe,
          bdfieldmoistfineearth, bdfieldmoistwhole, bdovendrywhole, bdovendryfineearth, bdsatiated, phbulkdensityiid
        FROM phorizon_View_1
        INNER JOIN phdb_View_1 ON phdb_View_1.phiidref = phorizon_View_1.phiid
        ORDER BY bddepthtop, seqnum"
  soilDB::uncode(soilDB::dbQueryNASIS(soilDB::NASIS(dsn = dsn), q), dsn = dsn)
}
