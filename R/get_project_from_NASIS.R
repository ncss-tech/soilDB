
#' Get NASIS Project Information
#' 
#' Helper functions for accessing the NASIS Project object.
#' 
#' @param SS _logical_. Use selected set? Default: `TRUE`. Set `FALSE` for local database.
#' @param stringsAsFactors Deprecated.
#' @param dsn Optional: path or _DBIConnection_ to \link[=NASISLocalDatabase]{local database containing NASIS table structure}; default: `NULL`
#'
#' @return `get_projectmilestone_from_NASIS()`: _data.frame_ containing project and project milestone information
#' @export
#' @rdname get_project_from_NASIS
get_projectmilestone_from_NASIS <- function(SS = TRUE, stringsAsFactors = NULL, dsn = NULL) {

  q <- "SELECT p.projectiid, p.uprojectid, p.projectname, pt.projecttypename, pst.projectsubtypename, 
               a.areasymbol AS mlra_sso, pms.seqnum, mst.milestonetypename, pms.milestonedesc, 
               pms.scheduledstartdate, pms.scheduledcompletiondate, 
               pms.milestonedatestarted, pms.milestonedatecompleted
        FROM project_View_1 p
        LEFT OUTER JOIN projecttype pt ON pt.projecttypeiid = p.projecttypeiidref
        LEFT OUTER JOIN projectsubtype pst ON pst.projectsubtypeiid = p.projectsubtypeiidref
        LEFT OUTER JOIN area a ON a.areaiid = p.mlrassoareaiidref
        INNER JOIN projectmilestone pms ON pms.projectiidref = p.projectiid
        LEFT OUTER JOIN milestonetype mst ON mst.milestonetypeiid = pms.milestonetypeiidref
        ORDER BY p.projectname, pms.seqnum;"
  
  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }
  
  channel <- dbConnectNASIS(dsn)
  
  if (inherits(channel, 'try-error'))
    return(data.frame())
  
  # exec query
  d.project <- dbQueryNASIS(channel, q)
  
  # test is selected set is empty
  if (nrow(d.project) == 0)
    message("Your selected set or local database is missing data in the project table, please load it and try again")
  
  # uncode metadata domains
  d.project <- uncode(d.project, dsn = dsn)
  
  # done
  return(d.project)
}

#' @return `get_projectmapunit_from_NASIS()`: _data.frame_ containing project and project mapunit information
#' @export
#' @rdname get_project_from_NASIS
get_projectmapunit_from_NASIS <- function(SS = TRUE, stringsAsFactors = NULL, dsn = NULL) {

  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }
  
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
  if (SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # exec query
  d.project <- dbQueryNASIS(channel, q)

  # test is selected set is empty
  if (nrow(d.project) == 0)
    message("Your selected set or local database is missing data in the project table, please load it and try again")
  
  # uncode metadata domains
  d.project <- uncode(d.project, dsn = dsn)

  # done
  return(d.project)
}
