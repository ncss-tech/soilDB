##
## December, 2017
## D.E. Beaudette
## J.M. Skovlin
## S.M. Roecker
##


## component diagnostic features
get_component_diaghz_from_NASIS_db <- function(SS=TRUE) {

  channel <- .openNASISchannel()
  if (channel == -1)
    return(data.frame())

  # query diagnostic horizons, usually a 1:many relationship with pedons
  q <- "SELECT coiidref as coiid, featkind, featdept_l, featdept_r, featdept_h, featdepb_l, featdepb_r, featdepb_h, featthick_l, featthick_r, featthick_h FROM codiagfeatures_View_1 AS cdf ORDER BY cdf.coiidref, cdf.featdept_r;"

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)

  # close connection
  RODBC::odbcClose(channel)

  # convert codes
  d <- uncode(d)
}

## component diagnostic features
get_component_restrictions_from_NASIS_db <- function(SS = TRUE) {

  channel <- .openNASISchannel()
  if (channel == -1)
    return(data.frame())

  # query restrictions, can be 1:many relationship with pedons
  q <- "SELECT coiidref as coiid, reskind, resdept_l, resdept_r, resdept_h, resdepb_l, resdepb_r, resdepb_h, resthk_l, resthk_r, resthk_h, reskind, reshard FROM corestrictions_View_1 AS cr ORDER BY cr.coiidref, cr.resdept_r;"

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)

  # close connection
  RODBC::odbcClose(channel)

  # convert codes
  return(uncode(d))
}

## get map unit text from local NASIS
get_mutext_from_NASIS_db <- function(SS=TRUE, fixLineEndings=TRUE) {

  q <- "SELECT mu.muiid, mu.mukind, mu.mutype, mu.muname, mu.nationalmusym,
  mut.seqnum, mut.recdate, mut.recauthor, mut.mapunittextkind, mut.textcat, mut.textsubcat, CAST(mut.textentry AS ntext) AS textentry

  FROM
  mapunit_View_1 AS mu
  INNER JOIN mutext_View_1 AS mut ON mu.muiid = mut.muiidref;"

  channel <- .openNASISchannel()
  if (channel == -1)
    return(data.frame())

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)

  # close connection
  RODBC::odbcClose(channel)

  # convert codes
  d <- uncode(d)

  # replace tabs with spaces
  # tabs at the beginning of a line will confuse the MD parser, generating <code><pre> blocks
  d$textentry <- gsub(d$textentry, pattern = '\t', replacement = ' ', fixed = TRUE)

  # optionally convert \r\n -> \n
  if(fixLineEndings){
    d$textentry <- gsub(d$textentry, pattern = '\r\n', replacement = '\n', fixed = TRUE)
  }


  # done
  return(d)
}




## just the component records, nothing above or below
get_component_data_from_NASIS_db <- function(SS=TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)

  q <- "SELECT dmudesc, compname, comppct_r, compkind, majcompflag, localphase, drainagecl, hydricrating, elev_l, elev_r, elev_h, slope_l, slope_r, slope_h, aspectccwise, aspectrep, aspectcwise, map_l, map_r, map_h, airtempa_l as maat_l, airtempa_r as maat_r, airtempa_h as maat_h, soiltempa_r as mast_r, reannualprecip_r, ffd_l, ffd_r, ffd_h, tfact, wei, weg, nirrcapcl, nirrcapscl, nirrcapunit, irrcapcl, irrcapscl, irrcapunit, frostact, hydricrating, hydgrp, corcon, corsteel, taxclname, taxorder, taxsuborder, taxgrtgroup, taxsubgrp, taxpartsize, taxpartsizemod, taxceactcl, taxreaction, taxtempcl, taxmoistscl, taxtempregime, soiltaxedition, coiid, dmuiid

  FROM
  datamapunit_View_1 AS dmu
  INNER JOIN component_View_1 AS co ON co.dmuiidref = dmu.dmuiid

  ORDER BY dmudesc, comppct_r DESC, compname ASC;"

  channel <- .openNASISchannel()
  if (channel == -1)
    return(data.frame())

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)

  # close connection
  RODBC::odbcClose(channel)

  # test for duplicate coiids
  idx <- which(table(d$coiid) > 1)
  if(length(idx) > 0) {
    dupes <- names(idx)
    assign('dupe.coiids', value=dupes, envir=soilDB.env)
    message("-> QC: duplicate coiids, this should not happen. Use `get('dupe.coiids', envir=soilDB.env)` for related coiid values.")
  }

  # uncode metadata domains
  if(nrow(d) > 0) {
    d <- uncode(d, stringsAsFactors = stringsAsFactors)
  }

  # done
  return(d)
}


get_legend_from_NASIS <- function(SS = TRUE, droplevels = TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)

  q.legend  <- paste("
                     SELECT
                     mlraoffice,
                     areasymbol, areaname, areatypename, CAST(areaacres AS INTEGER) AS areaacres, ssastatus,
                     CAST(projectscale AS INTEGER) projectscale, cordate,
                     CAST(liid AS INTEGER) liid, COUNT(lmu.lmapunitiid) n_lmapunitiid

                     FROM
                     area     a                                  INNER JOIN
                     legend_View_1   l      ON l.areaiidref = a.areaiid INNER JOIN
                     lmapunit_View_1 lmu    ON lmu.liidref = l.liid

                     INNER JOIN
                     areatype at  ON at.areatypeiid = areatypeiidref

                     WHERE
                         legendsuituse = 3  AND
                         mustatus IN (2, 3) AND
                         areatypename = 'Non-MLRA Soil Survey Area'

                     GROUP BY mlraoffice, areasymbol, areaname, areatypename, areaacres, ssastatus, projectscale, cordate, liid

                     ORDER BY mlraoffice, areasymbol
                     ;")

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q.legend <- gsub(pattern = '_View_1', replacement = '', x = q.legend, fixed = TRUE)
  }

  channel <- .openNASISchannel()
  if (channel == -1)
    return(data.frame())

  # exec query
  d.legend <- RODBC::sqlQuery(channel, q.legend, stringsAsFactors=FALSE)

  # close connection
  RODBC::odbcClose(channel)

  # recode metadata domains
  d.legend <- uncode(d.legend,
                     db = "NASIS",
                     droplevels = droplevels,
                     stringsAsFactors = stringsAsFactors
                     )


  # done
  return(d.legend)
}



get_lmuaoverlap_from_NASIS <- function(SS = TRUE, droplevels = TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)

  q <- paste("SELECT
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

             WHERE legendsuituse = 3  AND
                   mustatus IN (2, 3) AND
                   at.areatypename = 'Non-MLRA Soil Survey Area'

             ORDER BY a.areasymbol, lmu.musym, lao_areatypename
             ;"
  )

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  channel <- .openNASISchannel()
  if (channel == -1)
    return(data.frame())

  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)

  # close connection
  RODBC::odbcClose(channel)

  d$musym <- as.character(d$musym)

  # recode metadata domains
  d <- uncode(d,
              db = "NASIS",
              droplevels = droplevels,
              stringsAsFactors = stringsAsFactors
  )

  # done
  return(d)
}



get_mapunit_from_NASIS <- function(SS = TRUE, droplevels = TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)

  q.mapunit <- paste("
                     SELECT
                     ng.grpname, areasymbol, liid, lmapunitiid,
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
                         areatypename = 'Non-MLRA Soil Survey Area'

                     ORDER BY areasymbol, musym
                     ;")

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q.mapunit <- gsub(pattern = '_View_1', replacement = '', x = q.mapunit, fixed = TRUE)
  }

  channel <- .openNASISchannel()
  if (channel == -1)
    return(data.frame())

  # exec query
  d.mapunit <- RODBC::sqlQuery(channel, q.mapunit, stringsAsFactors=FALSE)

  # close connection
  RODBC::odbcClose(channel)

  # recode metadata domains
  d.mapunit <- uncode(d.mapunit,
                      db = "NASIS",
                      droplevels = droplevels,
                      stringsAsFactors = stringsAsFactors
  )

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


# return all rows from correlation -- map unit -- legend map unit -- dmu / legend -- area
# note that all of these "target tables" have to be selected
get_component_correlation_data_from_NASIS_db <- function(SS=TRUE, dropAdditional=TRUE, dropNotRepresentative=TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)

  q <- "SELECT lmapunitiid, mu.muiid, musym, nationalmusym, mu.muname, mukind, mutype, mustatus, muacres, farmlndcl, repdmu, dmuiid, areasymbol, areaname, ssastatus, cordate

  FROM  mapunit_View_1 AS mu

  LEFT OUTER JOIN correlation_View_1 AS cor ON cor.muiidref = mu.muiid
  LEFT OUTER JOIN datamapunit_View_1 AS dmu ON cor.dmuiidref = dmu.dmuiid
  LEFT OUTER JOIN lmapunit_View_1 AS lm ON lm.muiidref = mu.muiid
  LEFT OUTER JOIN legend_View_1 AS leg ON leg.liid = lm.liidref
  LEFT OUTER JOIN area_View_1 AS a ON a.areaiid = leg.areaiidref

  ORDER BY nationalmusym, dmuiid;"

  channel <- .openNASISchannel()
  if (channel == -1)
    return(data.frame())

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)

  # close connection
  RODBC::odbcClose(channel)

  ## TODO: is this a good idea?
  # test for no data
  if(nrow(d) == 0)
    warning('there are no records in your selected set!', call. = FALSE)

  # recode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors)

  # optionally drop additional | NA mustatus
  if(dropAdditional) {
    idx <- which(d$mustatus == 'additional')
    if(length(idx) > 0) {
      d <- d[-idx, ]
    }
  }

  # optionally drop not-representative
  if(dropNotRepresentative) {
    d <- d[which(d$repdmu == 1), ]
  }

  # check for non-unique MUs
  idx <- which(table(d$muiid) > 1)
  if(length(idx) > 0) {
    dupes <- names(idx)
    assign('dupe.muiids', value=dupes, envir=soilDB.env)
    message("-> QC: duplicate muiids: multiple 'representative' DMU / MU?. Use `get('dupe.muiids', envir=soilDB.env)` for related muiid values.")
  }


  # check for multiple DMUs:
  idx <- which(table(d$dmuiid) > 1)
  if(length(idx) > 0) {
    dupes <- names(idx)
    assign('multiple.mu.per.dmu', value=dupes, envir=soilDB.env)
    message("-> QC: DMUs assigned to multiple MU. Use `get('multiple.mu.per.dmu', envir=soilDB.env)` for related dmuiid values.")
  }


  # done
  return(d)
}

# get geomorphic desc for each component
get_component_cogeomorph_data_from_NASIS_db <- function(SS=TRUE) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)


  q <- "SELECT cogeo.coiidref as coiid, cogeo.geomfmod, geomorfeat.geomfname, cogeo.geomfeatid, cogeo.existsonfeat, cogeo.geomfiidref, lower(geomorfeattype.geomftname) as geomftname

  FROM
  component_View_1 AS co
  INNER JOIN cogeomordesc_View_1 AS cogeo ON co.coiid = cogeo.coiidref
  INNER JOIN geomorfeat ON geomorfeat.geomfiid = cogeo.geomfiidref
  INNER JOIN geomorfeattype ON geomorfeattype.geomftiid = geomorfeat.geomftiidref

  ORDER BY coiid, geomfeatid ASC;"

  channel <- .openNASISchannel()
  if (channel == -1)
    return(data.frame())

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)

  # close connection
  RODBC::odbcClose(channel)

  # done
  return(d)
}


# get copm for each component
get_component_copm_data_from_NASIS_db <- function(SS=TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)


  q <- "SELECT cpmg.coiidref as coiid, cpm.seqnum as seqnum, pmorder, pmdept_r, pmdepb_r, pmmodifier, pmgenmod, pmkind, pmorigin

  FROM
  component_View_1 AS co
  INNER JOIN copmgrp_View_1 AS cpmg ON cpmg.coiidref = co.coiid
  INNER JOIN copm_View_1 AS cpm ON cpm.copmgrpiidref = cpmg.copmgrpiid

  ORDER BY coiidref, seqnum, pmorder, copmgrpiid ASC;"

  channel <- .openNASISchannel()
  if (channel == -1)
    return(data.frame())

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)

  # close connection
  RODBC::odbcClose(channel)

  # uncode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors)

  # done
  return(d)
}



# get ESD information for each component
get_component_esd_data_from_NASIS_db <- function(SS=TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)

  q <- "SELECT coiidref as coiid, ecositeid, ecositenm,
  ecositeorigin, ecositetype, ecositemlra, ecositelru, ecositenumber, ecositestate

  FROM coecosite_View_1 AS coecosite

  INNER JOIN ecologicalsite AS es ON es.ecositeiid = coecosite.ecositeiidref

  ORDER BY coiid;"

  channel <- .openNASISchannel()
  if (channel == -1)
    return(data.frame())

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)

  # close connection
  RODBC::odbcClose(channel)

  # check for more than 1 record / coiid
  idx <- which(table(d$coiid) > 1)
  dupes <- names(idx)
  assign('multiple.ecosite.per.coiid', value=dupes, envir=soilDB.env)
  if(length(idx) > 0) {
    message("-> QC: multiple ecosites / component. Use `get('multiple.ecosite.per.coiid', envir=soilDB.env)` for related coiid values.")
  }

  # uncode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors)

  # done
  return(d)
}

## TODO: convert any multiple entries into a comma delimited string
# get OtherVeg information for each component
get_component_otherveg_data_from_NASIS_db <- function(SS=TRUE) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)

  q <- "SELECT coiidref as coiid, ovegclid, ovegclname, coothvegcl.recwlupdated
  FROM coothvegclass_View_1 coothvegcl
  INNER JOIN othvegclass as ovc ON ovc.ovegcliid = coothvegcl.ovegcliidref
  ORDER BY coiid;"

  # setup connection local NASIS
  channel <- .openNASISchannel()
  if (channel == -1)
    return(data.frame())

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)

  # close connection
  RODBC::odbcClose(channel)

  # check for more than 1 record / coiid
  idx <- which(table(d$coiid) > 1)
  if(length(idx) > 0) {
    dupes <- names(idx)
    assign('multiple.otherveg.per.coiid', value=dupes, envir=soilDB.env)
    message("-> QC: multiple othervegclasses / component. Use `get('multiple.otherveg.per.coiid', envir=soilDB.env)` for related coiid values.")
  }

  # uncode metadata domains
  #d <- uncode(d)

  # done
  return(d)
}

get_comonth_from_NASIS_db <- function(SS=TRUE, fill=FALSE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)

  q <- "SELECT coiidref AS coiid, month, flodfreqcl, floddurcl, pondfreqcl, ponddurcl, ponddep_l, ponddep_r, ponddep_h, dlyavgprecip_l, dlyavgprecip_r, dlyavgprecip_h, comonthiid
  FROM comonth_View_1 AS comonth;"

  channel <- .openNASISchannel()
  if (channel == -1)
    return(data.frame())

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)

  # uncode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors)

  # optionally fill missing coiids
  if(fill) {
    q <- "SELECT coiid
    FROM component_View_1
    ORDER BY coiid;"

    # toggle selected set vs. local DB
    if(SS == FALSE) {
      q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
    }

    # exec query
    d.coiid <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  }

  # close connection
  RODBC::odbcClose(channel)

  # re-format month names
  # only if > 0 rows of data
  if(nrow(d) > 0) {
    # using 3-letter month names
    d$month <- months(as.Date(paste0("2016-", d$month, "-01"), format="%Y-%B-%d"), abbreviate = TRUE)
  }

  ## these degrade gracefully when comonth data are missing
  # calendar order
  d$month <- factor(d$month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

  # fix other factor levels
  d$flodfreqcl <- factor(d$flodfreqcl, levels=c('none', 'very rare', 'rare', 'occasional', 'common', 'frequent', 'very frequent'))
  d$floddurcl <- factor(d$floddurcl, levels=c('extremely brief', 'very brief', 'brief', 'long', 'very long'), labels=c('Extremely brief (0.1 to 4 hours)', 'Very brief (4 to 48 hours)', 'Brief (2 to 7 days)', 'Long (7 to 30 days)', 'Very long (more than 30 days)'))

  d$pondfreqcl <- factor(d$pondfreqcl, levels=c('none', 'rare', 'occasional', 'common', 'frequent'))
  d$ponddurcl <- factor(d$ponddurcl, levels=c('very brief', 'brief', 'long', 'very long'), labels=c('Very brief (4 to 48 hours)', 'Brief (2 to 7 days)', 'Long (7 to 30 days)', 'Very long (more than 30 days)'))


  # optionally fill missing coiids
  if(fill) {
    # make a new DF with all coiids and months
    nd <- expand.grid(coiid=d.coiid$coiid, month=levels(d$month))
    nd$month <- factor(nd$month, levels=levels(d$month))

    # join full version to comonth records
    # nd contains the full set of component records IDs
    d <- join(nd, d, by=c('coiid', 'month'), type='left')

    ## this isn't likely needed, will re-visit after some testing

    # # add "not-populated" to rows that have been filled
    # levels(d$flodfreqcl) <- c("not populated", levels(d$flodfreqcl))
    # d$flodfreqcl[is.na(d$flodfreqcl)] <- 'not populated'
    #
    # levels(d$floddurcl) <- c("not populated", levels(d$floddurcl))
    # d$floddurcl[is.na(d$floddurcl)] <- 'not populated'
    #
    # levels(d$pondfreqcl) <- c("not populated", levels(d$pondfreqcl))
    # d$pondfreqcl[is.na(d$pondfreqcl)] <- 'not populated'
    #
    # levels(d$ponddurcl) <- c("not populated", levels(d$ponddurcl))
    # d$ponddurcl[is.na(d$ponddurcl)] <- 'not populated'
  }

  # re-order by coiid, then months
  d <- d[order(d$coiid, as.numeric(d$month)), ]

  # done
  return(d)
}





# get linked pedons by peiid and user pedon ID
# note that there may be >=1 pedons / coiid
get_copedon_from_NASIS_db <- function(SS=TRUE) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)

  q <- "SELECT coiidref as coiid, peiidref as peiid, upedonid as pedon_id, rvindicator as representative

  FROM copedon_View_1 copedon

  LEFT OUTER JOIN pedon_View_1 p ON p.peiid = copedon.peiidref;
  "
  channel <- .openNASISchannel()
  if (channel == -1)
    return(data.frame())

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)

  # close connection
  RODBC::odbcClose(channel)

  # missing pedon ID suggests records not in the selected set or local database
  if(nrow(d) > 0 & any(is.na(d$pedon_id))) {
    message('some linked pedons not in selected set or local database')
  }

  # done
  return(d)
}


## TODO: better documentation for "fill" argument
# https://github.com/ncss-tech/soilDB/issues/50
get_component_horizon_data_from_NASIS_db <- function(SS=TRUE, fill = FALSE) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)

  q <- "SELECT coiid, chiid, hzname, hzdept_r, hzdepb_r, texture, fragvoltot_l, fragvoltot_r, fragvoltot_h, sandtotal_l, sandtotal_r, sandtotal_h, silttotal_l, silttotal_r, silttotal_h, claytotal_l, claytotal_r, claytotal_h, om_l, om_r, om_h, structgrpname, dbthirdbar_l, dbthirdbar_r, dbthirdbar_h, ksat_l, ksat_r, ksat_h, awc_l, awc_r, awc_h, lep_l, lep_r, lep_h, ll_l, ll_r, ll_h, pi_l, pi_r, pi_h, sieveno4_l, sieveno4_r, sieveno4_h, sieveno10_l, sieveno10_r, sieveno10_h, sieveno40_l, sieveno40_r, sieveno40_h, sieveno200_l, sieveno200_r, sieveno200_h, sar_l, sar_r, sar_h, ec_l, ec_r, ec_h, cec7_l, cec7_r, cec7_h, sumbases_l, sumbases_r, sumbases_h, ecec_l, ecec_r, ecec_h, ph1to1h2o_l, ph1to1h2o_r, ph1to1h2o_h, ph01mcacl2_l, ph01mcacl2_r, ph01mcacl2_h, caco3_l, caco3_r, caco3_h, kffact, kwfact, aashind_l, aashind_r, aashind_h

  FROM
  component_View_1 co
  LEFT OUTER JOIN chorizon_View_1 ch ON ch.coiidref = co.coiid
  LEFT OUTER JOIN chtexturegrp_View_1 cht ON cht.chiidref = ch.chiid AND cht.rvindicator = 1
  LEFT OUTER JOIN chstructgrp_View_1 chs ON chs.chiidref = ch.chiid AND chs.rvindicator = 1

  -- why is this here?
  INNER JOIN datamapunit_View_1 dmu ON dmu.dmuiid = co.dmuiidref

  ORDER BY dmudesc, comppct_r DESC, compname ASC, hzdept_r ASC;"

  channel <- .openNASISchannel()
  if (channel == -1)
    return(data.frame())

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)

  # close connection
  RODBC::odbcClose(channel)

  ## TODO: better documentation for "fill" argument
  # https://github.com/ncss-tech/soilDB/issues/50
  # remove records what are missing horizon data
  if (fill == FALSE) {
    d <- d[!is.na(d$chiid), ]
  }

  # done
  return(d)
}

