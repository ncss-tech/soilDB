#' Get records from the Soil Classification (SC) database
#'
#' These functions return records from the Soil Classification database, either
#' from the local NASIS database (all series) or via web report (named series
#' only).
#'
#' @aliases get_soilseries_from_NASIS get_soilseries_from_NASISWebReport
#'
#' @param stringsAsFactors logical: should character vectors be converted to
#' factors? This argument is passed to the `uncode()` function. It does not
#' convert those vectors that have set outside of `uncode()` (i.e. hard coded).
#'
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: `NULL`
#'
#' @return A \code{data.frame}
#'
#' @author Stephen Roecker
#'
#' @keywords manip
#'
#' @export get_soilseries_from_NASIS
get_soilseries_from_NASIS <- function(stringsAsFactors = default.stringsAsFactors(),
                                      dsn = NULL) {

  q.soilseries <- "
  SELECT soilseriesname, soilseriesstatus, benchmarksoilflag, statsgoflag, mlraoffice, areasymbol, areatypename, taxclname, taxorder, taxsuborder, taxgrtgroup, taxsubgrp, taxpartsize, taxpartsizemod, taxceactcl, taxreaction, taxtempcl, originyear, establishedyear, soiltaxclasslastupdated, soilseriesiid

  FROM
  soilseries ss

  INNER JOIN
      area       a  ON a.areaiid      = ss.typelocstareaiidref
  INNER JOIN
      areatype   at ON at.areatypeiid = ss.typelocstareatypeiidref

  ORDER BY soilseriesname
  ;"

  # LEFT OUTER JOIN
  #     soilseriestaxmineralogy sstm ON sstm.soilseriesiidref = ss.soilseriesiid

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # exec query
  d.soilseries <- dbQueryNASIS(channel, q.soilseries)

  # recode metadata domains
  d.soilseries <- uncode(d.soilseries, stringsAsFactors = stringsAsFactors, dsn = dsn)

  # prep
  d.soilseries$soiltaxclasslastupdated <- format(d.soilseries$soiltaxclasslastupdated, "%Y")

  # done
  return(d.soilseries)
}

get_soilseries_from_NASISWebReport <- function(soils, stringsAsFactors = default.stringsAsFactors()) {

  url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_soilseries_from_NASISWebReport"

  d.ss <- lapply(soils, function(x) {
    args = list(p_soilseriesname = x)
    d    = parseWebReport(url, args)
  })
  d.ss <- do.call("rbind", d.ss)

  # set factor levels according to metadata domains
  d.ss[!names(d.ss) %in% c("mlraoffice", "taxminalogy")] <- uncode(d.ss[!names(d.ss) %in% c("mlraoffice", "taxminalogy")],
                                                                   db = "SDA", stringsAsFactors = stringsAsFactors)

  d.ss[names(d.ss) %in% c("mlraoffice")] <- uncode(d.ss[names(d.ss) %in% c("mlraoffice")],
                                                   db = "LIMS", stringsAsFactors = stringsAsFactors)

  # return data.frame
  return(d.ss)
}
