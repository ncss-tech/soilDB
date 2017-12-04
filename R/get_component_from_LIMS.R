get_component_from_LIMS <- function(projectname) {
  
  url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_component_from_LIMS"
  
  args <- list(p_projectname = projectname)
  
  d.component <- parseWebReport(url, args)
  
  # set factor levels according to metadata domains
  d.component <- uncode(d.component, db = "LIMS")
  
  # return data.frame
  return(d.component)
  
  }


get_chorizon_from_LIMS <- function(projectname, fill = FALSE) {
  
  url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_chorizon_from_LIMS"
  
  args <- list(p_projectname = projectname)
  
  d.chorizon <- parseWebReport(url, args)
  
  ## TODO: might be nice to abstract this into a new function
  # hacks to make R CMD check --as-cran happy:
  metadata <- NULL
  # load local copy of metadata
  load(system.file("data/metadata.rda", package="soilDB")[1])
  
  # transform variables and metadata
  d.chorizon <- within(d.chorizon, {
    texture = tolower(texture)
    texture = factor(texture, levels = metadata[metadata$ColumnPhysicalName == "texcl", "ChoiceName"])
    })
  
  # fill
  if (fill == FALSE) {
    d.chorizon <- d.chorizon[!is.na(d.chorizon$chiid), ]
    }
  
  # return data.frame
  return(d.chorizon)
  
  }


get_mapunit_from_LIMS <- function(projectname) {
  
  url <-"https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_projectmapunit_from_LIMS"
  
  args <- list(p_projectname = projectname)
  
  d.mapunit <- parseWebReport(url, args)
  
  # set factor levels according to metadata domains
  d.mapunit <- uncode(d.mapunit, db = "LIMS")
  
  # return data.frame
  return(d.mapunit)
  
  }


get_project_from_LIMS <- function(mlrassoarea, fiscalyear) {
  
  url <-"https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_project_from_LIMS"
  
  args <- list(p_mlrassoarea = mlrassoarea, p_fy = fiscalyear)
  
  d.project <- parseWebReport(url, args)
  
  # set factor levels according to metadata domains
  d.project <- uncode(d.project, db = "LIMS")
  
  # return data.frame
  return(d.project)
  
  }


get_progress_from_LIMS <- function(mlrassoarea, fiscalyear, projecttypename) {
  
  url <-"https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_progress_from_LIMS"
  
  args <- list(p_mlrassoarea = mlrassoarea, p_fy = fiscalyear, p_projecttypename = projecttypename)
  
  d.progress <- parseWebReport(url, args)
  
  # set factor levels according to metadata domains
  d.progress <- uncode(d.progress, db = "LIMS")
  
  # return data.frame
  return(d.progress)
  
}


fetchLIMS_component <- function(projectname, rmHzErrors = FALSE, fill = FALSE) {
  
  # load data in pieces
  f.mapunit <- get_mapunit_from_LIMS(projectname)
  f.component <- get_component_from_LIMS(projectname)
  f.chorizon <- get_chorizon_from_LIMS(projectname, fill)
  
  # optionally test for bad horizonation... flag, and remove
  if (rmHzErrors) {
    f.chorizon.test <- plyr::ddply(f.chorizon, 'cokey', test_hz_logic, topcol='hzdept_r', bottomcol='hzdepb_r', strict=TRUE)
    
    # which are the good (valid) ones?
    good.ids <- as.character(f.chorizon.test$cokey[which(f.chorizon.test$hz_logic_pass)])
    bad.ids  <- as.character(f.chorizon.test$cokey[which(! f.chorizon.test$hz_logic_pass)])
    
    # keep the good ones
    f.chorizon <- f.chorizon[which(f.chorizon$cokey %in% good.ids), ]
    
    # keep track of those components with horizonation errors
    if(length(bad.ids) > 0)
      assign('component.hz.problems', value=bad.ids, envir=soilDB.env)
    }
  
  # upgrade to SoilProfilecollection
  depths(f.chorizon) <- coiid ~ hzdept_r + hzdepb_r
  
  
  ## TODO: this will fail in the presence of duplicates
  ## TODO: make this error more informative
  # add site data to object
  site(f.chorizon) <- f.component # left-join via cokey
  
  # print any messages on possible data quality problems:
  if (exists('component.hz.problems', envir=soilDB.env))
    message("-> QC: horizon errors detected, use `get('component.hz.problems', envir=soilDB.env)` for related cokey values")
  
  # done, return SPC
  return(list(spc = f.chorizon, mapunit = f.mapunit))
  
  }

  