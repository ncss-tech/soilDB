## TODO: when multiple textures have been defined, only the first one is returned (alphabetical ?)
#


#' Extract Horizon Data from a local NASIS Database
#'
#' Get horizon-level data from a local NASIS database.
#'
#' @param SS fetch data from Selected Set in NASIS or from the entire local database (default: `TRUE`)
#'
#' @param stringsAsFactors logical: should character vectors be converted to
#' factors? This argument is passed to the `uncode()` function. It does not
#' convert those vectors that have been set outside of `uncode()` (i.e. hard
#' coded).
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
                                      stringsAsFactors = default.stringsAsFactors(),
                                      dsn = NULL) {

  q <- "SELECT peiid, phiid, upedonid as pedon_id,
  hzname, dspcomplayerid as genhz, hzdept, hzdepb,
  bounddistinct, boundtopo,
  claytotest AS clay, CASE WHEN silttotest IS NULL THEN 100 - (claytotest + sandtotest) ELSE silttotest END AS silt,
  sandtotest AS sand, fragvoltot, texture, texcl, lieutex, phfield, effclass, phs.labsampnum, rupresblkdry, rupresblkmst, rupresblkcem, stickiness, plasticity, ksatpedon

  FROM

  pedon_View_1 p
  INNER JOIN phorizon_View_1 ph ON ph.peiidref = p.peiid
  LEFT OUTER JOIN phsample_View_1 phs ON phs.phiidref = ph.phiid
  LEFT OUTER JOIN
  (
  SELECT phiidref, MIN(texcl) AS texcl, MIN(lieutex) as lieutex
  FROM phtexture_View_1
  GROUP BY phiidref
  ) AS pht ON pht.phiidref = ph.phiid

  ORDER BY p.upedonid, ph.hzdept ASC;"

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # exec query
  d <- dbQueryNASIS(channel, q)

  # uncode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors, dsn = dsn)

  # re-implement texture_class column, with lieutex in cases where texcl is missing
  d$texture_class <- ifelse(is.na(d$texcl) & ! is.na(d$lieutex), as.character(d$lieutex), as.character(d$texcl))

  # test for duplicate horizons:
  #  bugs in our queries
  #   multiple lab samples / genetic horizon
  hz.tab <- table(d$phiid)
  dupe.hz <- which(hz.tab > 1)
  dupe.hz.phiid <- names(hz.tab[dupe.hz])
  dupe.hz.pedon.ids <- d$pedon_id[d$phiid %in% dupe.hz.phiid]

  if (length(dupe.hz) > 0) {
    message(paste0('NOTICE: multiple `labsampnum` values / horizons; see pedon IDs:\n',
                   paste(unique(dupe.hz.pedon.ids), collapse = ',')))
  }

  # done
  return(d)
}

