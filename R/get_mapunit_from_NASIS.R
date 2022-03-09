#' Get Legend, Mapunit and Legend Mapunit Area Overlap Tables
#' 
#'
#' @param SS fetch data from the currently loaded selected set in NASIS or from the entire local database (default: `TRUE`)
#' @param droplevels Drop unused levels from `farmlndcl` and other factor levels from NASIS domains?
#' @param stringsAsFactors deprecated
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: `NULL`
#' 
#' @export
get_mapunit_from_NASIS <- function(SS = TRUE, droplevels = TRUE, stringsAsFactors = NULL, dsn = NULL) {
  
  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }
  
  q.mapunit <- paste("
                     SELECT
                     ng.grpname, areasymbol, areatypename, liid, lmapunitiid,
                     nationalmusym, muiid, musym, muname, mukind, mutype, mustatus, dmuinvesintens, muacres,
                     farmlndcl, dmuiid, pct_component, pct_hydric, n_component, n_majcompflag

                     FROM
                         area            a                               INNER JOIN
                         legend_View_1   l   ON l.areaiidref = a.areaiid INNER JOIN
                         lmapunit_View_1 lmu ON lmu.liidref = l.liid     INNER JOIN
                         mapunit_View_1  mu  ON mu.muiid = lmu.muiidref

                    INNER JOIN
                         areatype at  ON at.areatypeiid = areatypeiidref

                    INNER JOIN
                        nasisgroup  ng ON ng.grpiid = mu.grpiidref

                    LEFT OUTER JOIN
                    --components
                    (SELECT
                     cor.muiidref cor_muiidref, dmuiid, dmuinvesintens,
                     SUM(comppct_r)                                                pct_component,
                     SUM(comppct_r * CASE WHEN hydricrating = 1 THEN 1 ELSE 0 END) pct_hydric,
                     COUNT(*)                                                      n_component,
                     SUM(CASE WHEN majcompflag  = 1 THEN 1 ELSE 0 END)             n_majcompflag

                     FROM
                         component_View_1   co                                  LEFT OUTER JOIN
                         datamapunit_View_1 dmu ON dmu.dmuiid    = co.dmuiidref LEFT OUTER JOIN
                         correlation_View_1 cor ON cor.dmuiidref = dmu.dmuiid   AND
                                                   cor.repdmu    = 1

                     GROUP BY cor.muiidref, dmuiid, dmuinvesintens
                    ) co ON co.cor_muiidref = mu.muiid

                     WHERE
                         areatypename IN ('Non-MLRA Soil Survey Area', 'MLRA Soil Survey Area')

                     ORDER BY areasymbol, musym
                     ;")
  
  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.mapunit <- gsub(pattern = '_View_1', replacement = '', x = q.mapunit, fixed = TRUE)
  }
  
  channel <- dbConnectNASIS(dsn)
  
  if (inherits(channel, 'try-error'))
    return(data.frame())
  
  # exec query
  d.mapunit <- dbQueryNASIS(channel, q.mapunit)
  
  # recode metadata domains
  d.mapunit <- uncode(d.mapunit,
                      db = "NASIS",
                      droplevels = droplevels,
                      dsn = dsn)
  
  # hacks to make R CMD check --as-cran happy:
  metadata <- NULL
  
  # load local copy of metadata
  load(system.file("data/metadata.rda", package="soilDB")[1])
  
  # transform variables and metadata
  d.mapunit <- within(d.mapunit, {
    farmlndcl = factor(farmlndcl,
                       levels = metadata[metadata$ColumnPhysicalName == "farmlndcl", "ChoiceValue"],
                       labels = metadata[metadata$ColumnPhysicalName == "farmlndcl", "ChoiceLabel"]
    )
    if (stringsAsFactors == FALSE) {
      farmlndcl = as.character(farmlndcl)
    }
    if (droplevels == TRUE & is.factor(farmlndcl)) {
      farmlndcl = droplevels(farmlndcl)
    }
  })
  
  # cache original column names
  orig_names <- names(d.mapunit)
  
  
  # done
  return(d.mapunit)
}


#' @export
#' @rdname get_mapunit_from_NASIS
get_legend_from_NASIS <- function(SS = TRUE,
                                  droplevels = TRUE,
                                  stringsAsFactors = NULL,
                                  dsn = NULL) {
  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }
  
  q.legend  <- paste("
                     SELECT
                     mlraoffice,
                     areasymbol, areaname, areatypename, CAST(areaacres AS INTEGER) AS areaacres, ssastatus,
                     CAST(projectscale AS INTEGER) projectscale, cordate,
                     CAST(liid AS INTEGER) liid, COUNT(lmu.lmapunitiid) n_lmapunitiid, legendsuituse

                     FROM
                     area     a                                  INNER JOIN
                     legend_View_1   l      ON l.areaiidref = a.areaiid INNER JOIN
                     lmapunit_View_1 lmu    ON lmu.liidref = l.liid

                     INNER JOIN
                     areatype at  ON at.areatypeiid = areatypeiidref

                     WHERE
                         areatypename IN ('Non-MLRA Soil Survey Area', 'MLRA Soil Survey Area')

                     GROUP BY mlraoffice, areasymbol, areaname, areatypename, areaacres, ssastatus, projectscale, legendsuituse, cordate, liid

                     ORDER BY mlraoffice, areasymbol
                     ;")
  
  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q.legend <- gsub(pattern = '_View_1', replacement = '', x = q.legend, fixed = TRUE)
  }
  
  channel <- dbConnectNASIS(dsn)
  
  if (inherits(channel, 'try-error'))
    return(data.frame())
  
  # exec query
  d.legend <- dbQueryNASIS(channel, q.legend)
  
  # recode metadata domains
  d.legend <- uncode(d.legend,
                     db = "NASIS",
                     droplevels = droplevels,
                     dsn = dsn)
  
  # done
  return(d.legend)
}



#' @export
#' @rdname get_mapunit_from_NASIS
get_lmuaoverlap_from_NASIS <- function(SS = TRUE,
                                       droplevels = TRUE,
                                       stringsAsFactors = NULL,
                                       dsn = NULL) {
  
  q <- "SELECT
             a.areasymbol, a.areaname, a.areaacres,
             at2.areatypename lao_areatypename, a2.areasymbol lao_areasymbol, a2.areaname lao_areaname, lao.areaovacres lao_areaovacres,
             lmapunitiid, musym, nationalmusym, muname, mustatus, muacres,
             lmuao.areaovacres lmuao_areaovacres

             FROM
             legend_View_1   l                                             INNER JOIN
             lmapunit_View_1 lmu   ON lmu.liidref          = l.liid        INNER JOIN
             mapunit_View_1  mu    ON mu.muiid             = lmu.muiidref

             INNER JOIN
                 area     a  ON a.areaiid      = l.areaiidref INNER JOIN
                 areatype at ON at.areatypeiid = a.areatypeiidref

             LEFT OUTER JOIN
                 laoverlap_View_1  lao ON lao.liidref      = l.liid         INNER JOIN
                 area              a2  ON a2.areaiid       = lao.areaiidref INNER JOIN
                 areatype          at2  ON at2.areatypeiid = a2.areatypeiidref

             LEFT OUTER JOIN
                 lmuaoverlap_View_1 lmuao ON lmuao.lmapunitiidref = lmu.lmapunitiid
                                     AND lmuao.lareaoviidref  = lao.lareaoviid

             WHERE 
                 at.areatypename IN ('Non-MLRA Soil Survey Area', 'MLRA Soil Survey Area')

             ORDER BY a.areasymbol, lmu.musym, lao_areatypename
             ;"
  
  
  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }
  
  channel <- dbConnectNASIS(dsn)
  
  if (inherits(channel, 'try-error'))
    return(data.frame())
  
  d <- dbQueryNASIS(channel, q)
  
  d$musym <- as.character(d$musym)
  
  # recode metadata domains
  d <- uncode(d,
              db = "NASIS",
              droplevels = droplevels,
              dsn = dsn)
  
  # done
  return(d)
}
