#' Get records from the Soil Classification (SC) database
#'
#' These functions return records from the Soil Classification database, either
#' from the local NASIS database (all series) or via web report (named series
#' only).
#'
#' @aliases get_soilseries_from_NASIS get_soilseries_from_NASISWebReport
#' @param stringsAsFactors deprecated
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: `NULL`
#'
#' @param delimiter _character_. Used to collapse `taxminalogy` records where multiple values are used to describe strongly contrasting control sections. Default `" over "` creates combination mineralogy classes as they would be used in the family name.
#'
#' @return A \code{data.frame}
#'
#' @author Stephen Roecker
#'
#' @keywords manip
#'
#' @export get_soilseries_from_NASIS
get_soilseries_from_NASIS <- function(stringsAsFactors = NULL,
                                      dsn = NULL, delimiter = " over ") {
  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }
  
  q.soilseries <- "
  SELECT soilseriesname, soilseriesstatus, benchmarksoilflag, soiltaxclasslastupdated, mlraoffice, taxclname, taxorder, taxsuborder, taxgrtgroup, taxsubgrp, taxpartsize, taxpartsizemod, taxceactcl, taxreaction, taxtempcl, taxfamhahatmatcl, originyear, establishedyear, descriptiondateinitial, descriptiondateupdated, statsgoflag,  soilseriesiid, areasymbol, areaname, areaacres, obterm, areatypename, soilseriesedithistory
  FROM soilseries ss
  INNER JOIN area a ON a.areaiid = ss.typelocstareaiidref
  INNER JOIN areatype at ON at.areatypeiid = ss.typelocstareatypeiidref
  ORDER BY soilseriesname;"

  q.min <- "SELECT soilseriesiidref, minorder, taxminalogy FROM soilseriestaxmineralogy
            ORDER BY soilseriesiidref, minorder;"

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # exec query
  d.soilseries <- dbQueryNASIS(channel, q.soilseries, close = FALSE)
  d.soilseriesmin <- dbQueryNASIS(channel, q.min)

  # recode metadata domains
  d.soilseries <- uncode(d.soilseries, dsn = dsn)
  d.soilseriesmin <- uncode(d.soilseriesmin, dsn = dsn)

  # prep
  d.soilseries$soiltaxclassyearlastupdated <- format(as.Date(d.soilseries$soiltaxclasslastupdated), "%Y")

  # aggregate mineralogy data (ordered by minorder, combined with "over")
  d.minagg <- aggregate(d.soilseriesmin$taxminalogy,
                        list(soilseriesiid = d.soilseriesmin$soilseriesiidref),
                        paste0, collapse = delimiter)
  colnames(d.minagg) <- c("soilseriesiid", "taxminalogy")

  res <- merge(
      d.soilseries,
      d.minagg,
      by = "soilseriesiid",
      all.x = TRUE,
      incomparables = NA,
      sort = FALSE
    )

  # reorder column names
  return(res[,c("soilseriesiid", "soilseriesname", "soilseriesstatus", "benchmarksoilflag",
                "soiltaxclasslastupdated", "mlraoffice", "taxclname", "taxorder",
                "taxsuborder", "taxgrtgroup", "taxsubgrp", "taxpartsize", "taxpartsizemod",
                "taxceactcl", "taxreaction", "taxtempcl", "taxminalogy", "taxfamhahatmatcl",
                "originyear", "establishedyear", "descriptiondateinitial", "descriptiondateupdated",
                "statsgoflag", "soilseriesedithistory", "areasymbol", "areaname", 
                "areaacres", "obterm", "areatypename")])
}

get_soilseries_from_NASISWebReport <- function(soils, stringsAsFactors = NULL) {

  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }
  
  url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_soilseries_from_NASISWebReport"

  d.ss <- lapply(soils, function(x) {
    args = list(p_soilseriesname = x)
    d    = parseWebReport(url, args)
  })
  d.ss <- do.call("rbind", d.ss)

  # set factor levels according to metadata domains
  d.ss[!names(d.ss) %in% c("mlraoffice", "taxminalogy")] <- uncode(d.ss[!names(d.ss) %in% c("mlraoffice", "taxminalogy")],
                                                                   db = "SDA")

  d.ss[names(d.ss) %in% c("mlraoffice")] <- uncode(d.ss[names(d.ss) %in% c("mlraoffice")],
                                                   db = "LIMS")

  # return data.frame
  return(d.ss)
}
