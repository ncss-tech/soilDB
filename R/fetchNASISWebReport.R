
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
#' @param stringsAsFactors logical: should character vectors be converted to
#' factors? This argument is passed to the `uncode()` function. It does not
#' convert those vectors that have been set outside of `uncode()` (i.e. hard
#' coded). The 'factory-fresh' default is TRUE, but this can be changed by
#' setting options(`stringsAsFactors = FALSE`)
#' @param droplevels logical: indicating whether to drop unused levels in
#' classifying factors. This is useful when a class has large number of unused
#' classes, which can waste space in tables and figures.
#' @return A data.frame or list with the results.
#' @author Stephen Roecker
#' @keywords manip
#' @examples
#'
#' \donttest{
#'
#' if (requireNamespace("curl") &
#'     curl::has_internet() &
#'     require("aqp") &
#'     require("ggplot2") &
#'     require("gridExtra")
#' ) {
#'   # query soil components by projectname
#'   test = fetchNASISWebReport(
#'     "EVAL - MLRA 111A - Ross silt loam, 0 to 2 percent slopes, frequently flooded",
#'     stringsAsFactors = TRUE)
#'   test = test$spc
#'
#'   # profile plot
#'   plot(test)
#'
#'   # convert the data for depth plot
#'   clay_slice = horizons(slice(test, 0:200 ~ claytotal_l + claytotal_r + claytotal_h))
#'   names(clay_slice) <- gsub("claytotal_", "", names(clay_slice))
#'
#'   om_slice = horizons(slice(test, 0:200 ~ om_l + om_r + om_h))
#'   names(om_slice) = gsub("om_", "", names(om_slice))
#'
#'   test2 = rbind(data.frame(clay_slice, var = "clay"),
#'                 data.frame(om_slice, var = "om")
#'   )
#'
#'   h = merge(test2, site(test)[c("dmuiid", "coiid", "compname", "comppct_r")],
#'             by = "coiid",
#'             all.x = TRUE
#'   )
#'
#'   # depth plot of clay content by soil component
#'   gg_comp <- function(x) {
#'     ggplot(x) +
#'       geom_line(aes(y = r, x = hzdept_r)) +
#'       geom_line(aes(y = r, x = hzdept_r)) +
#'       geom_ribbon(aes(ymin = l, ymax = h, x = hzdept_r), alpha = 0.2) +
#'       xlim(200, 0) +
#'       xlab("depth (cm)") +
#'       facet_grid(var ~ dmuiid + paste(compname, comppct_r)) +
#'       coord_flip()
#'   }
#'   g1 <- gg_comp(subset(h, var == "clay"))
#'   g2 <- gg_comp(subset(h, var == "om"))
#'
#'   grid.arrange(g1, g2)
#'
#'
#'   # query cosoilmoist (e.g. water table data) by mukey
#'   # NA depths are interpreted as (???) with impute=TRUE argument
#'   x <- get_cosoilmoist_from_NASISWebReport(
#'     "EVAL - MLRA 111A - Ross silt loam, 0 to 2 percent slopes, frequently flooded",
#'     stringsAsFactors = TRUE)
#'
#'   ggplot(x, aes(x = as.integer(month), y = dept_r, lty = status)) +
#'     geom_rect(aes(xmin = as.integer(month), xmax = as.integer(month) + 1,
#'                   ymin = 0, ymax = max(x$depb_r),
#'                   fill = flodfreqcl)) +
#'     geom_line(cex = 1) +
#'     geom_point() +
#'     geom_ribbon(aes(ymin = dept_l, ymax = dept_h), alpha = 0.2) +
#'     ylim(max(x$depb_r), 0) +
#'     xlab("month") + ylab("depth (cm)") +
#'     scale_x_continuous(breaks = 1:12, labels = month.abb, name="Month") +
#'     facet_wrap(~ paste0(compname, ' (', comppct_r , ')')) +
#'     ggtitle(paste0(x$nationalmusym[1],
#'                    ': Water Table Levels from Component Soil Moisture Month Data'))
#'
#'
#' }
#'
#'
#'
#' }
#'
#' @export fetchNASISWebReport
fetchNASISWebReport <- function(projectname, rmHzErrors = FALSE, fill = FALSE,
                                stringsAsFactors = default.stringsAsFactors()
) {

  # load data in pieces
  f.mapunit   <- get_projectmapunit_from_NASISWebReport(projectname, stringsAsFactors = stringsAsFactors)
  f.component <- get_component_from_NASISWebReport(projectname, stringsAsFactors = stringsAsFactors)
  f.chorizon  <- get_chorizon_from_NASISWebReport(projectname, fill, stringsAsFactors = stringsAsFactors)

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
      assign('component.hz.problems', value=bad.ids, envir=soilDB.env)
  }

  # upgrade to SoilProfilecollection
  depths(f.chorizon) <- coiid ~ hzdept_r + hzdepb_r


  ## TODO: this will fail in the presence of duplicates
  ## TODO: make this error more informative
  # add site data to object
  site(f.chorizon) <- f.component # left-join via coiid

  # set NASIS-specific horizon identifier
  hzidname(f.chorizon) <- 'chiid'

  # print any messages on possible data quality problems:
  if (exists('component.hz.problems', envir=soilDB.env))
    message("-> QC: horizon errors detected, use `get('component.hz.problems', envir=soilDB.env)` for related cokey values")

  # done, return SPC
  return(list(spc = f.chorizon, mapunit = f.mapunit))

}

#' @rdname fetchNASISWebReport
get_component_from_NASISWebReport <- function(projectname, stringsAsFactors = default.stringsAsFactors()) {

  url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_component_from_NASISWebReport"

  d.component <- lapply(projectname, function(x) {
    message("getting project '", x, "' from NasisReportsWebSite \n", sep = "")
    args = list(p_projectname = x)
    d    =  tryCatch(parseWebReport(url, args),
                     error = function(e) {
                       message(e)
                       return(NULL)
                     })
  })

  d.component <- do.call("rbind", d.component)

  if(is.null(d.component))
    return(NULL)

  # set factor levels according to metadata domains
  d.component <- uncode(d.component, db = "LIMS", stringsAsFactors = stringsAsFactors)

  # prep
  d.component <- .cogmd_prep(d.component, db = "LIMS")


  # return data.frame
  return(d.component)

}


#' @rdname fetchNASISWebReport
get_chorizon_from_NASISWebReport <- function(projectname, fill = FALSE, stringsAsFactors = default.stringsAsFactors()) {

  url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_chorizon_from_NASISWebReport"

  d.chorizon <- lapply(projectname, function(x) {
    args = list(p_projectname = x)
    d    =  parseWebReport(url, args)
  })
  d.chorizon <- do.call("rbind", d.chorizon)

  ## TODO: might be nice to abstract this into a new function
  # hacks to make R CMD check --as-cran happy:
  metadata <- NULL
  # load local copy of metadata
  load(system.file("data/metadata.rda", package="soilDB")[1])

  # transform variables and metadata
  if (!all(is.na(d.chorizon$chiid))) {
    d.chorizon <- within(d.chorizon, {
      texture = tolower(texture)
      if (stringsAsFactors == TRUE) {
        texcl = factor(texcl,
                       levels = metadata[metadata$ColumnPhysicalName == "texcl", "ChoiceValue"],
                       labels = metadata[metadata$ColumnPhysicalName == "texcl", "ChoiceName"]
        )
      }
    })
  }

  # fill
  if (fill == FALSE) {
    d.chorizon <- d.chorizon[!is.na(d.chorizon$chiid), ]
  }

  # return data.frame
  return(d.chorizon)

}



#' @rdname fetchNASISWebReport
get_legend_from_NASISWebReport <- function(mlraoffice, areasymbol, droplevels = TRUE, stringsAsFactors = default.stringsAsFactors()) {

  url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_legend_from_NASISWebReport"

  args <- list(p_mlraoffice = mlraoffice, p_areasymbol = areasymbol)

  d.legend <- parseWebReport(url, args)


  # set factor levels according to metadata domains
  # data is coming back uncoded from LIMS so db is set to "SDA"
  d.legend <- uncode(d.legend,
                     db = "SDA",
                     droplevels = droplevels,
                     stringsAsFactors = stringsAsFactors
  )

  # date
  d.legend$cordate <- as.Date(d.legend$cordate)

  # return data.frame
  return(d.legend)

}



#' @rdname fetchNASISWebReport
get_lmuaoverlap_from_NASISWebReport <- function(areasymbol, droplevels = TRUE, stringsAsFactors = default.stringsAsFactors()) {
  url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_lmuaoverlap_from_NASISWebReport"

  d <- lapply(areasymbol, function(x) {
    message("getting legend for '", x, "' from NasisReportsWebSite \n", sep = "")
    args = list(p_areasymbol = x)
    d    =  parseWebReport(url, args)
  })
  d <- do.call("rbind", d)


  # set factor levels according to metadata domains
  # data is coming back uncoded from LIMS so db is set to "SDA"
  d <- uncode(d,
              db = "SDA",
              droplevels = droplevels,
              stringsAsFactors = stringsAsFactors
  )

  # return data.frame
  return(d)

}



#' @rdname fetchNASISWebReport
get_mapunit_from_NASISWebReport <- function(areasymbol, droplevels = TRUE, stringsAsFactors = default.stringsAsFactors()) {
  url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_mapunit_from_NASISWebReport"

  d.mapunit <- lapply(areasymbol, function(x) {
    message("getting map units for '", x, "' from NasisReportsWebSite \n", sep = "")
    args = list(p_areasymbol = x)
    d    =  parseWebReport(url, args)
  })
  d.mapunit <- do.call("rbind", d.mapunit)

  d.mapunit$musym = as.character(d.mapunit$musym)

  # set factor levels according to metadata domains
  # data is coming back uncoded from LIMS so db is set to "SDA"
  d.mapunit <- uncode(d.mapunit,
                      db = "SDA",
                      droplevels = droplevels,
                      stringsAsFactors = stringsAsFactors
  )

  # return data.frame
  return(d.mapunit)

}


#' @rdname fetchNASISWebReport
get_projectmapunit_from_NASISWebReport <- function(projectname, stringsAsFactors = default.stringsAsFactors()) {

  url <-"https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_projectmapunit_from_NASISWebReport"


  d.mapunit <- lapply(projectname, function(x) {
    args = list(p_projectname = x)
    d    =  parseWebReport(url, args)
  })
  d.mapunit <- do.call("rbind", d.mapunit)

  d.mapunit$musym = as.character(d.mapunit$musym)

  # set factor levels according to metadata domains
  d.mapunit <- uncode(d.mapunit, db = "LIMS", stringsAsFactors = stringsAsFactors)

  # return data.frame
  return(d.mapunit)

}


#' @rdname fetchNASISWebReport
get_projectmapunit2_from_NASISWebReport <- function(mlrassoarea, fiscalyear, projectname, stringsAsFactors = default.stringsAsFactors()) {

  url <-"https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_projectmapunit2_from_NASISWebReport"

  args = list(p_mlrassoarea = mlrassoarea, p_fy = fiscalyear, p_projectname = projectname)
  d.mapunit    =  parseWebReport(url, args)

  d.mapunit$musym = as.character(d.mapunit$musym)

  # set factor levels according to metadata domains
  # data is coming back uncoded from LIMS so db is set to "SDA"
  d.mapunit <- uncode(d.mapunit, db = "SDA", stringsAsFactors = stringsAsFactors)

  # return data.frame
  return(d.mapunit)

}

#' @rdname fetchNASISWebReport
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
get_progress_from_NASISWebReport <- function(mlrassoarea, fiscalyear, projecttypename) {

  url <-"https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_progress_from_NASISWebReport"

  args <- list(p_mlrassoarea = mlrassoarea, p_fy = fiscalyear, p_projecttypename = projecttypename)

  d.progress <- parseWebReport(url, args)

  # return data.frame
  return(d.progress)

}


#' @rdname fetchNASISWebReport
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
      n         = nchar(musym)
      begin_1   = substr(musym, 2, n)
      end_1     = substr(musym, 1, n - 1)
      end_4     = substr(musym, 1, n - 4)

      idx       = musym != new_musym & !is.na(new_musym)
      orig_musym = ifelse(idx & musym != begin_1 & (new_musym == begin_1 | substr(musym, 1, 1) %in% c("x", "z")), begin_1, musym)
      # Joe recommended using |\\+${1}, but appears to be legit in some cases
      orig_musym = ifelse(idx & musym != end_1   & new_musym == end_1 , end_1   , orig_musym)
      orig_musym = ifelse(idx & musym != end_4   & new_musym == end_4 , end_4   , orig_musym)
    })
  }

  d.rcor[c("n", "begin_1", "end_1", "end_4", "idx")] <- NULL

  # return data.frame
  return(d.rcor)

}




