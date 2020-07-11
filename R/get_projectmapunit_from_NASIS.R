get_projectmapunit_from_NASIS <- function(SS = TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)

  q <- paste("SELECT p.projectiid, p.uprojectid, p.projectname, pmu.seqnum pmu_seqnum, a2.areasymbol, lmu.musym, lmu.lmapunitiid AS mukey, mu.nationalmusym, mutype, lmu.mustatus, muname, muacres

             FROM
             project_View_1 p INNER JOIN
             projectmapunit_View_1 pmu ON pmu.projectiidref = p.projectiid LEFT OUTER JOIN
             mapunit_View_1 mu ON mu.muiid = pmu.muiidref LEFT OUTER JOIN
             lmapunit_View_1 lmu ON lmu.muiidref = mu.muiid LEFT OUTER JOIN
             legend_View_1 l ON l.liid = lmu.liidref

             INNER JOIN
               area a ON a.areaiid = p.mlrassoareaiidref
             LEFT OUTER JOIN
               area a2 ON a2.areaiid = l.areaiidref

             WHERE l.legendsuituse != 1 OR l.legendsuituse IS NULL

             ORDER BY p.projectname, a.areasymbol, lmu.musym;"
  )

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  channel <- .openNASISchannel()
  if (channel == -1)
    return(data.frame())

  # exec query
  d.project <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)

  # test is selected set is empty
  if (nrow(d.project) == 0) message("your selected set is missing the project table, please load it and try again")

  # uncode metadata domains
  d.project <- uncode(d.project, stringsAsFactors = stringsAsFactors)


  # close connection
  RODBC::odbcClose(channel)


  # done
  return(d.project)
  }
