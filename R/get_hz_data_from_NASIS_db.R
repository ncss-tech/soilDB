## TODO: when multiple textures have been defined, only the first one is returned (alphabetical ?)


#' Get Horizon Data from a local NASIS Database
#'
#' Get horizon-level data from a local NASIS database.
#'
#' @param SS fetch data from Selected Set in NASIS or from the entire local database (default: `TRUE`)
#' 
#' @param fill include pedons without horizon data in result? default: `FALSE`

#' @param stringsAsFactors deprecated
#'
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: `NULL`
#'
#' @return A data.frame.
#'
#' @note `NULL` total rock fragment values are assumed to represent an _absence_ of rock fragments, and set to 0.
#'
#' @author Jay M. Skovlin and Dylan E. Beaudette
#'
#' @seealso \code{\link{get_hz_data_from_NASIS_db}}, \code{\link{get_site_data_from_NASIS_db}}
#' @keywords manip
#' @export get_hz_data_from_NASIS_db
get_hz_data_from_NASIS_db <- function(SS = TRUE,
                                      fill = FALSE,
                                      stringsAsFactors = NULL,
                                      dsn = NULL) {
  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }
  
  .soilDB_warn_deprecated_aliases(c("upedonid" = "pedon_id", "claytotest" = "clay", "silttotest" = "silt", "sandtotest" = "sand"))
  
  q <- sprintf("SELECT peiid, phiid, upedonid AS pedon_id, upedonid,
    hzname, dspcomplayerid, hzdept, hzdepb,
    bounddistinct, boundtopo, claytotest, silttotest, sandtotest, 
    claytotest AS clay,
    CASE WHEN silttotest IS NULL THEN 100 - (claytotest + sandtotest) ELSE silttotest END AS silt,
    sandtotest AS sand,
    fragvoltot, texture, texcl, lieutex, phfield, effclass, phs.labsampnum, 
    rupresblkdry, rupresblkmst, rupresblkcem, stickiness, plasticity, ksatpedon
  FROM pedon_View_1 p
  %s JOIN phorizon_View_1 ph ON ph.peiidref = p.peiid
  LEFT OUTER JOIN phsample_View_1 phs ON phs.phiidref = ph.phiid
  LEFT OUTER JOIN
  (
  SELECT phiidref, MIN(texcl) AS texcl, MIN(lieutex) as lieutex
  FROM phtexture_View_1
  GROUP BY phiidref
  ) AS pht ON pht.phiidref = ph.phiid

  ORDER BY p.upedonid, ph.hzdept ASC;", ifelse(fill, "LEFT", "INNER"))

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (isFALSE(SS)) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # exec query
  d <- dbQueryNASIS(channel, q)

  # uncode metadata domains
  d <- uncode(d, dsn = dsn)

  # re-implement texture_class column, with lieutex in cases where texcl is missing
  d$texture_class <- ifelse(is.na(d$texcl) & ! is.na(d$lieutex), as.character(d$lieutex), as.character(d$texcl))

  # test for multiple lab samples per genetic horizon
  hz.tab <- table(d$phiid)
  mult.labsampnum.phiid <- names(hz.tab[which(hz.tab > 1)])

  if (length(mult.labsampnum.phiid) > 0) {
    message(paste0('NOTE: some phiid have multiple lab sample IDs (labsampnum)'))
    assign("multiple.labsampnum.per.phiid", value = mult.labsampnum.phiid, envir = get_soilDB_env())
  }

  # done
  return(d)
}

