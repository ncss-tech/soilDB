
#' Get component tables from NASIS Web Reports
#'
#' @aliases fetchNASISWebReport get_project_from_NASISWebReport
#' get_progress_from_NASISWebReport get_project_correlation_from_NASISWebReport
#' get_legend_from_NASISWebReport get_mapunit_from_NASISWebReport
#' get_projectmapunit_from_NASISWebReport
#' get_projectmapunit2_from_NASISWebReport get_component_from_NASISWebReport
#' get_chorizon_from_NASISWebReport get_cosoilmoist_from_NASISWebReport
#' get_sitesoilmoist_from_NASISWebReport get_lmuaoverlap_from_NASISWebReport
#'
#' @param projectname text string vector of project names to be inserted into a
#' SQL WHERE clause (default: `NA`)
#' @param mlraoffice text string value identifying the MLRA Regional Soil
#' Survey Office group name inserted into a SQL WHERE clause (default: `NA`)
#' @param mlrassoarea text string value identifying the MLRA Soil Survey Office
#' areasymbol symbol inserted into a SQL WHERE clause (default: `NA`)
#' @param fiscalyear text string value identifying the fiscal year inserted
#' into a SQL WHERE clause (default: `NA`)
#' @param projecttypename text string value identifying the project type name
#' inserted into a SQL WHERE clause (default: `NA`)
#' @param areasymbol text string value identifying the area symbol (e.g.
#' `IN001` or `IN%`) inserted into a SQL WHERE clause (default: `NA`)
#' `NULL` (default: `TRUE`)
#' @param fill should rows with missing component ids be removed (default: `FALSE`)
#' @param rmHzErrors should pedons with horizonation errors be removed from the
#' results? (default: `FALSE`)
#' @param droplevels logical: indicating whether to drop unused levels in
#' classifying factors. This is useful when a class has large number of unused
#' classes, which can waste space in tables and figures.
#' @param impute replace missing (i.e. `NULL`) values with `"Not_Populated"` for
#' categorical data, or the "RV" for numeric data or `201` cm if the "RV" is also
#' `NULL` (default: `TRUE`)
#' @param usiteid character: User Site IDs
#' @return A data.frame or list with the results.
#' @author Stephen Roecker
#' @keywords manip
#'
#' @export fetchNASISWebReport
fetchNASISWebReport <- function(projectname, rmHzErrors = FALSE, fill = FALSE) {
  
  if (!requireNamespace("aqp")) {
    stop("package 'aqp' is required", call. = FALSE)
  }
  
  # load data in pieces
  f.mapunit   <- get_projectmapunit_from_NASISWebReport(projectname)
  f.component <- get_component_from_NASISWebReport(projectname)
  f.chorizon  <- get_chorizon_from_NASISWebReport(projectname, fill)

  # return NULL if one of the required pieces is missing
  if(is.null(f.mapunit) | is.null(f.component) | is.null(f.chorizon)) {
    message("One or more inputs for fetchNASISWebReport (mapunit, component, or horizon) is NULL, returning NULL.")
    return(NULL)
  }

  # optionally test for bad horizonation... flag, and remove
  if (rmHzErrors) {
    f.chorizon.test <- aqp::checkHzDepthLogic(f.chorizon,
                                              hzdepths =  c('hzdept_r', 'hzdepb_r'),
                                              idname = 'coiid', fast = TRUE)

    # which are the good (valid) ones?
    good.ids <- as.character(f.chorizon.test$coiid[which(f.chorizon.test$valid)])
    bad.ids  <- as.character(f.chorizon.test$coiid[which(! f.chorizon.test$valid)])

    # keep the good ones
    f.chorizon <- f.chorizon[which(f.chorizon$coiid %in% good.ids), ]

    # keep track of those components with horizonation errors
    if(length(bad.ids) > 0)
      assign('component.hz.problems', value=bad.ids, envir=get_soilDB_env())
  }

  # upgrade to SoilProfilecollection
  aqp::depths(f.chorizon) <- coiid ~ hzdept_r + hzdepb_r

  # add site data to object
  aqp::site(f.chorizon) <- f.component # left-join via coiid

  # set NASIS-specific horizon identifier
  aqp::hzidname(f.chorizon) <- 'chiid'

  # print any messages on possible data quality problems:
  if (exists('component.hz.problems', envir=get_soilDB_env()))
    message("-> QC: horizon errors detected:\n\tUse `get('component.hz.problems', envir=get_soilDB_env())` for component keys (cokey)")

  # done, return SPC
  return(list(spc = f.chorizon, mapunit = f.mapunit))

}

#' @rdname fetchNASISWebReport
#' @export
get_component_from_NASISWebReport <- function(projectname) {
  
  url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_component_from_NASISWebReport"

  d.component <- lapply(projectname, function(x) {
    message("getting project '", x, "' from NasisReportsWebSite \n", sep = "")
    args <- list(p_projectname = x)
    d <- tryCatch(parseWebReport(url, args),
                     error = function(e) {
                       message(e)
                       return(NULL)
                     })
  })

  d.component <- do.call("rbind", d.component)

  if (is.null(d.component))
    return(NULL)

  # set factor levels according to metadata domains
  d.component <- uncode(d.component)

  # prep
  d.component <- .cogmd_prep(d.component, db = "LIMS")

  # return data.frame
  return(d.component)

}


#' @rdname fetchNASISWebReport
#' @export
get_chorizon_from_NASISWebReport <- function(projectname, fill = FALSE) {
  
  url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_chorizon_from_NASISWebReport"

  d.chorizon <- lapply(projectname, function(x) {
    args <- list(p_projectname = x)
    d <- parseWebReport(url, args)
  })
  d.chorizon <- do.call("rbind", d.chorizon)

  metadata <- get_NASIS_metadata()
  
  # transform variables and metadata
  if (!all(is.na(d.chorizon$chiid))) {
    d.chorizon <- within(d.chorizon, {
      texture <- tolower(texture)
      if (getOption("stringsAsFactors", default = FALSE)) {
        texcl <- factor(texcl,
                       levels = metadata[metadata$ColumnPhysicalName == "texcl", "ChoiceValue"],
                       labels = metadata[metadata$ColumnPhysicalName == "texcl", "ChoiceName"]
        )
      }
    })
  }

  # fill
  if (isFALSE(fill)) {
    d.chorizon <- d.chorizon[!is.na(d.chorizon$chiid), ]
  }

  # return data.frame
  return(d.chorizon)

}



#' @rdname fetchNASISWebReport
#' @export
get_legend_from_NASISWebReport <- function(mlraoffice, areasymbol, droplevels = TRUE) {
  
  url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_legend_from_NASISWebReport"

  args <- list(p_mlraoffice = mlraoffice, p_areasymbol = areasymbol)

  d.legend <- parseWebReport(url, args)

  # set factor levels according to metadata domains
  d.legend <- uncode(d.legend, droplevels = droplevels)

  # date
  d.legend$cordate <- as.Date(d.legend$cordate)

  # return data.frame
  return(d.legend)

}



#' @rdname fetchNASISWebReport
#' @export
get_lmuaoverlap_from_NASISWebReport <- function(areasymbol, droplevels = TRUE) {  
  
  url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_lmuaoverlap_from_NASISWebReport"

  d <- lapply(areasymbol, function(x) {
    message("getting legend for '", x, "' from NasisReportsWebSite \n", sep = "")
    args <- list(p_areasymbol = x)
    d <- parseWebReport(url, args)
  })
  d <- do.call("rbind", d)


  # set factor levels according to metadata domains
  d <- uncode(d, droplevels = droplevels)

  # return data.frame
  return(d)

}



#' @rdname fetchNASISWebReport
#' @export
get_mapunit_from_NASISWebReport <- function(areasymbol, droplevels = TRUE) {

  url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_mapunit_from_NASISWebReport"

  d.mapunit <- lapply(areasymbol, function(x) {
    message("getting map units for '", x, "' from NasisReportsWebSite \n", sep = "")
    args <- list(p_areasymbol = x)
    d <- parseWebReport(url, args)
  })
  d.mapunit <- do.call("rbind", d.mapunit)

  d.mapunit$musym = as.character(d.mapunit$musym)

  # set factor levels according to metadata domains
  d.mapunit <- uncode(d.mapunit, droplevels = droplevels
  )

  # return data.frame
  return(d.mapunit)

}


#' @rdname fetchNASISWebReport
#' @export
get_projectmapunit_from_NASISWebReport <- function(projectname) {
  
  url <-"https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_projectmapunit_from_NASISWebReport"


  d.mapunit <- lapply(projectname, function(x) {
    args <- list(p_projectname = x)
    d <- parseWebReport(url, args)
  })
  d.mapunit <- do.call("rbind", d.mapunit)

  d.mapunit$musym = as.character(d.mapunit$musym)

  # set factor levels according to metadata domains
  d.mapunit <- uncode(d.mapunit)

  # return data.frame
  return(d.mapunit)

}


#' @rdname fetchNASISWebReport
#' @export
get_projectmapunit2_from_NASISWebReport <- function(mlrassoarea, fiscalyear, projectname) {

  url <-"https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_projectmapunit2_from_NASISWebReport"

  args <- list(p_mlrassoarea = mlrassoarea, p_fy = fiscalyear, p_projectname = projectname)
  d.mapunit <- parseWebReport(url, args)

  d.mapunit$musym = as.character(d.mapunit$musym)

  # set factor levels according to metadata domains
  # data is coming back uncoded from LIMS so db is set to "SDA"
  d.mapunit <- uncode(d.mapunit)

  # return data.frame
  return(d.mapunit)

}

#' @rdname fetchNASISWebReport
#' @export
get_project_from_NASISWebReport <- function(mlrassoarea, fiscalyear) {

  url <-"https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_project_from_NASISWebReport"

  args <- list(p_mlrassoarea = mlrassoarea, p_fy = fiscalyear)

  d.project <- parseWebReport(url, args)

  # prep
  idx <- unlist(lapply(names(d.project), function(x) grepl("date_", x)))
  if (any(idx)) {
    d.project[idx] <- lapply(d.project[idx], function(x) as.Date(x, format = "%Y/%m/%d"))
  }

  # return data.frame
  return(d.project)

}


#' @rdname fetchNASISWebReport
#' @export
get_progress_from_NASISWebReport <- function(mlrassoarea, fiscalyear, projecttypename) {

  url <-"https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_progress_from_NASISWebReport"

  args <- list(p_mlrassoarea = mlrassoarea, p_fy = fiscalyear, p_projecttypename = projecttypename)

  d.progress <- parseWebReport(url, args)

  # return data.frame
  return(d.progress)

}


#' @rdname fetchNASISWebReport
#' @export
get_project_correlation_from_NASISWebReport <- function(mlrassoarea, fiscalyear, projectname) {

  # nasty hack to trick R CMD check
  musym <- NULL
  new_musym <- NULL

  url <-"https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_project_correlation_from_NASISWebReport"

  args <- list(p_mlrassoarea = mlrassoarea, p_fy = fiscalyear, p_projectname = projectname)

  d.rcor <- parseWebReport(url, args)

  # compute musym_orig for additional lmapunits, necessary to catch changes to the original musym, due to a constraint on the lmapunit table that prevents duplicate musym for additional mapunits
  if (! is.null(d.rcor)) {

    d.rcor <- within(d.rcor, {
      n <- nchar(musym)
      begin_1 <- substr(musym, 2, n)
      end_1 <- substr(musym, 1, n - 1)
      end_4 <- substr(musym, 1, n - 4)

      idx <- musym != new_musym & !is.na(new_musym)
      orig_musym <- ifelse(idx & musym != begin_1 & (new_musym == begin_1 | substr(musym, 1, 1) %in% c("x", "z")), begin_1, musym)
      # Joe recommended using |\\+${1}, but appears to be legit in some cases
      orig_musym <- ifelse(idx & musym != end_1   & new_musym == end_1 , end_1   , orig_musym)
      orig_musym <- ifelse(idx & musym != end_4   & new_musym == end_4 , end_4   , orig_musym)
    })
  }

  d.rcor[c("n", "begin_1", "end_1", "end_4", "idx")] <- NULL

  # return data.frame
  return(d.rcor)

}




