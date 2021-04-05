# Based on ssurgoOnDemand by chad ferguson and jason nemecek
# SDA_muaggatt.R: translation of muaggatt.py into soilDB-style R function by andrew brown
# last update: 2021/04/03

#' Get map unit aggregate attribute information from Soil Data Access
#'
#' @param areasymbols vector of soil survey area symbols
#' @param mukeys vector of map unit keys
#' @author Jason Nemecek, Chad Ferguson, Andrew Brown
#' @return a data.frame
#' @export
#' @importFrom soilDB format_SQL_in_statement SDA_query
get_SDA_muaggatt <- function(areasymbols = NULL, mukeys = NULL) {

  stopifnot(!is.null(areasymbols) | !is.null(mukeys))

  if (!is.null(areasymbols))
    areasymbols <- soilDB::format_SQL_in_statement(areasymbols)

  if (!is.null(mukeys))
    mukeys <- soilDB::format_SQL_in_statement(mukeys)

  where_clause <- switch(as.character(is.null(areasymbols)),
                         "TRUE" = sprintf("mu.mukey IN %s", mukeys),
                         "FALSE" = sprintf("l.areasymbol IN %s", areasymbols))


  q <- sprintf("SELECT ma.musym,ma.muname,ma.mustatus,ma.slopegraddcp,ma.slopegradwta,ma.brockdepmin,ma.wtdepannmin,ma.wtdepaprjunmin,ma.flodfreqdcd,ma.flodfreqmax,ma.pondfreqprs,ma.aws025wta,ma.aws050wta,ma.aws0100wta,
        ma.aws0150wta,ma.drclassdcd,ma.drclasswettest,ma.hydgrpdcd,ma.iccdcd,ma.iccdcdpct,ma.niccdcd,ma.niccdcdpct,ma.engdwobdcd,ma.engdwbdcd,ma.engdwbll,ma.engdwbml,ma.engstafdcd,ma.engstafll,ma.engstafml,ma.engsldcd,ma.engsldcp,
        ma.englrsdcd,ma.engcmssdcd,ma.engcmssmp,ma.urbrecptdcd,ma.urbrecptwta,ma.forpehrtdcp,ma.hydclprs,ma.awmmfpwwta,ma.mukey
        FROM legend AS l
        INNER JOIN mapunit mu ON mu.lkey = l.lkey
        INNER JOIN muaggatt ma ON mu.mukey = ma.mukey
        WHERE %s", where_clause)

  # execute query
  res <- soilDB::SDA_query(q)

  # stop if bad
  if (inherits(res, 'try-error')) {
    warnings()
    stop(attr(res, 'condition'))
  }

  return(res)
}
