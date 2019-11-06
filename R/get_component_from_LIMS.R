get_component_from_NASISWebReport <- function(projectname, stringsAsFactors = default.stringsAsFactors()) {
  
  url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_component_from_NASISWebReport"
  
  d.component <- lapply(projectname, function(x) {
    cat("getting project '", x, "' from NasisReportsWebSite \n", sep = "")
    args = list(p_projectname = x)
    d    =  parseWebReport(url, args)
  })
  d.component <- do.call("rbind", d.component)
  
  # set factor levels according to metadata domains
  d.component <- uncode(d.component, db = "LIMS", stringsAsFactors = stringsAsFactors)
  
  
  # prep
  d.component <- .cogmd_prep(d.component, db = "LIMS")
  
  
  # return data.frame
  return(d.component)
  
  }


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
  d.chorizon <- within(d.chorizon, {
    texture = tolower(texture)
    if (stringsAsFactors == TRUE) {
      texcl = factor(texcl, 
                     levels = metadata[metadata$ColumnPhysicalName == "texcl", "ChoiceValue"],
                     labels = metadata[metadata$ColumnPhysicalName == "texcl", "ChoiceName"]
                     )
    }
    })
  
  # fill
  if (fill == FALSE) {
    d.chorizon <- d.chorizon[!is.na(d.chorizon$chiid), ]
    }
  
  # return data.frame
  return(d.chorizon)
  
  }



get_legend_from_NASISWebReport <- function(areasymbol, drop.unused.levels = TRUE, stringsAsFactors = default.stringsAsFactors()) {
  url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_legend_from_NASISWebReport"
  
  d.legend <- lapply(areasymbol, function(x) {
    cat("getting legend for '", x, "' from NasisReportsWebSite \n", sep = "")
    args = list(p_areasymbol = x)
    d    =  parseWebReport(url, args)
  })
  d.legend <- do.call("rbind", d.legend)
  
  
  # set factor levels according to metadata domains
  # data is coming back uncoded from LIMS so db is set to "SDA"
  d.legend <- uncode(d.legend, 
                      db = "SDA",
                      drop.unused.levels = drop.unused.levels,
                      stringsAsFactors = stringsAsFactors
  )
  
  # date
  d.legend$cordate <- as.Date(d.legend$cordate)
  
  # return data.frame
  return(d.legend)
  
}



get_lmuaoverlap_from_NASISWebReport <- function(areasymbol, drop.unused.levels = TRUE, stringsAsFactors = default.stringsAsFactors()) {
  url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_lmuaoverlap_from_NASISWebReport"
  
  d <- lapply(areasymbol, function(x) {
    cat("getting legend for '", x, "' from NasisReportsWebSite \n", sep = "")
    args = list(p_areasymbol = x)
    d    =  parseWebReport(url, args)
  })
  d <- do.call("rbind", d)
  
  
  # set factor levels according to metadata domains
  # data is coming back uncoded from LIMS so db is set to "SDA"
  d <- uncode(d, 
              db = "SDA",
              drop.unused.levels = drop.unused.levels,
              stringsAsFactors = stringsAsFactors
  )
  
    # return data.frame
  return(d)
  
}



get_mapunit_from_NASISWebReport <- function(areasymbol, drop.unused.levels = TRUE, stringsAsFactors = default.stringsAsFactors()) {
  url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_mapunit_from_NASISWebReport"
  
  d.mapunit <- lapply(areasymbol, function(x) {
    cat("getting map units for '", x, "' from NasisReportsWebSite \n", sep = "")
    args = list(p_areasymbol = x)
    d    =  parseWebReport(url, args)
  })
  d.mapunit <- do.call("rbind", d.mapunit)
  
  d.mapunit$musym = as.character(d.mapunit$musym)
  
  # set factor levels according to metadata domains
  # data is coming back uncoded from LIMS so db is set to "SDA"
  d.mapunit <- uncode(d.mapunit, 
                      db = "SDA",
                      drop.unused.levels = drop.unused.levels,
                      stringsAsFactors = stringsAsFactors
                      )
  
  # return data.frame
  return(d.mapunit)
  
}


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


get_progress_from_NASISWebReport <- function(mlrassoarea, fiscalyear, projecttypename) {
  
  url <-"https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_progress_from_NASISWebReport"
  
  args <- list(p_mlrassoarea = mlrassoarea, p_fy = fiscalyear, p_projecttypename = projecttypename)
  
  d.progress <- parseWebReport(url, args)
  
  # return data.frame
  return(d.progress)
  
  }


get_project_correlation_from_NASISWebReport <- function(mlrassoarea, fiscalyear, projectname) {
  
  # nasty hack to trick R CMD check
  musym <- NULL
  new_musym <- NULL
  
  url <-"https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_project_correlation_from_NASISWebReport"
  
  args <- list(p_mlrassoarea = mlrassoarea, p_fy = fiscalyear, p_projectname = projectname)
  
  d.rcor <- parseWebReport(url, args)
  
  # compute musym_orig for additional lmapunits, necessary to catch changes to the original musym, due to a constraint on the lmapunit table that prevents duplicate musym for additional mapunits 
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
  d.rcor[c("n", "begin_1", "end_1", "end_4", "idx")] <- NULL
  
  # return data.frame
  return(d.rcor)
  
  }


fetchNASISWebReport <- function(projectname, rmHzErrors = FALSE, fill = FALSE,
                                 stringsAsFactors = default.stringsAsFactors()
                                 ) {
  
  # load data in pieces
  f.mapunit   <- get_projectmapunit_from_NASISWebReport(projectname, stringsAsFactors = stringsAsFactors)
  f.component <- get_component_from_NASISWebReport(projectname, stringsAsFactors = stringsAsFactors)
  f.chorizon  <- get_chorizon_from_NASISWebReport(projectname, fill, stringsAsFactors = stringsAsFactors)
  
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
  
  # set SDA/SSURGO-specific horizon identifier
  hzidname(f.chorizon) <- 'chkey'
  
  # print any messages on possible data quality problems:
  if (exists('component.hz.problems', envir=soilDB.env))
    message("-> QC: horizon errors detected, use `get('component.hz.problems', envir=soilDB.env)` for related cokey values")
  
  # done, return SPC
  return(list(spc = f.chorizon, mapunit = f.mapunit))
  
  }

  