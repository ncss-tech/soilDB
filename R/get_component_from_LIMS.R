get_component_from_LIMS <- function(projectname, stringsAsFactors = default.stringsAsFactors()) {
  
  url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_component_from_LIMS"
  
  d.component <- lapply(projectname, function(x) {
    args = list(p_projectname = x)
    d    =  parseWebReport(url, args)
  })
  d.component <- do.call("rbind", d.component)
  
  # set factor levels according to metadata domains
  d.component <- uncode(d.component, db = "LIMS", stringsAsFactors = stringsAsFactors)
  
  # rename columns
  vars <- c("gc_mntn", "gc_hill", "gc_trce", "gc_flats", "hs_hillslopeprof", "ss_shapeacross", "ss_shapedown")
  new_names <- c("mntn", "hill", "trce", "flats", "hillslopeprof", "shapeacross", "shapedown")
  idx <- which(names(d.component) %in% vars)
  names(d.component)[idx] <- new_names 
  
  # combine geompos and shapes
  d.component <- within(d.component, {
    geompos = NA
    geompos = gsub("NA,|,NA|NA", "", paste(mntn, hill, trce, flats, sep = ","))
    geompos[geompos == ""] = NA
    
    ssa = NA
    ssd = NA
    slopeshape = NA
    
    ssa = gsub("Concave", "C", shapeacross)
    ssa = gsub("Linear",  "L", ssa)
    ssa = gsub("Convex",  "V", ssa)
    
    ssd = gsub("Concave", "C", shapedown)
    ssd = gsub("Linear",  "L", ssd)
    ssd = gsub("Convex",  "V", ssd)
    
    slopeshape = gsub("NA", "", paste0(ssd, ssa, sep = ""))
    slopeshape[slopeshape == ""] = NA
    })
  d.component[c("ssa", "ssd")] <- NULL
  
  ss_vars <- c("CC", "CV", "CL", "LC", "LL", "LV", "VL", "VC", "VV")
  if (all(d.component$slopeshape[!is.na(d.component$slopeshape)] %in% ss_vars)) {
    d.component$slopeshape <- factor(d.component$slopeshape, levels = ss_vars)
    d.component$slopeshape <- droplevels(d.component$slopeshape)
  }
  
  hs_vars <- c("Toeslope", "Footslope", "Backslope", "Shoulder", "Summit")
  if (all(d.component$hillslopeprof[!is.na(d.component$hillslopeprof)] %in% hs_vars)) {
    d.component$hillslopeprof <- factor(d.component$hillslopeprof, levels = hs_vars)
    d.component$hillslopeprof <- droplevels(d.component$hillslopeprof)
  }

  hill_vars <- c("Base Slope", "Head Slope", "Side Slope", "Free Face", "Nose Slope", "Crest", "Interfluve")
  if (all(d.component$hill[!is.na(d.component$hill)] %in% hill_vars)) {
    d.component$hill <- factor(d.component$hill, levels = hill_vars)
    d.component$hill <- droplevels(d.component$hill)
  }

  flats_vars <- c("Dip", "Talf", "Rise")
  if (all(d.component$flats[!is.na(d.component$flats)] %in% flats_vars)) {
    d.component$flats <- factor(d.component$flats, levels = flats_vars)
    d.component$flats <- droplevels(d.component$flats)
  }
  
  trce_vars <- c("Tread", "Riser")
  if (all(d.component$trce[!is.na(d.component$trce)] %in% trce_vars)) {
    d.component$trce <- factor(d.component$trce, levels = trce_vars)
    d.component$trce <- droplevels(d.component$trce)
  }
  
  # parent material
  d.component <- within(d.component, {
    lacustrine = NA
    alluvium = NA
    colluvium = NA
    loess = NA
    outwash = NA
    till = NA
    residuum = NA
    
    lacustrine = grepl("lacustrine" , pmgroupname)
    alluvium   = grepl("alluvium"   , pmgroupname)
    colluvium  = grepl("colluvium"  , pmgroupname)
    loess      = grepl("outwash|glacial fluvial"    , pmgroupname)
    outwash    = grepl("outwash"    , pmgroupname)
    till       = grepl("till"       , pmgroupname)
    residuum   = grepl("residuum"   , pmgroupname)
    })
  
  # return data.frame
  return(d.component)
  
  }


get_chorizon_from_LIMS <- function(projectname, fill = FALSE, stringsAsFactors = default.stringsAsFactors()) {
  
  url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_chorizon_from_LIMS"
  
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
      texture = factor(texture, levels = metadata[metadata$ColumnPhysicalName == "texcl", "ChoiceName"])
    }
    })
  
  # fill
  if (fill == FALSE) {
    d.chorizon <- d.chorizon[!is.na(d.chorizon$chiid), ]
    }
  
  # return data.frame
  return(d.chorizon)
  
  }


get_mapunit_from_LIMS <- function(projectname, stringsAsFactors = default.stringsAsFactors()) {
  
  url <-"https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_projectmapunit_from_LIMS"
  
  
  d.mapunit <- lapply(projectname, function(x) {
    args = list(p_projectname = x)
    d    =  parseWebReport(url, args)
    })
  d.mapunit <- do.call("rbind", d.mapunit)
  
  # set factor levels according to metadata domains
  d.mapunit <- uncode(d.mapunit, db = "LIMS", stringsAsFactors = stringsAsFactors)
  
  # return data.frame
  return(d.mapunit)
  
  }


get_projectmapunit_from_LIMS <- function(mlrassoarea, fiscalyear, projectname, stringsAsFactors = default.stringsAsFactors()) {
  
  url <-"https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_projectmapunit2_from_LIMS"
  
  
  args = list(p_mlrassoarea = mlrassoarea, p_fy = fiscalyear, p_projectname = projectname)
  d.mapunit    =  parseWebReport(url, args)
  
  # set factor levels according to metadata domains
  d.mapunit <- uncode(d.mapunit, db = "LIMS", stringsAsFactors = stringsAsFactors)
  
  # return data.frame
  return(d.mapunit)
  
}


get_project_from_LIMS <- function(mlrassoarea, fiscalyear) {
  
  url <-"https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_project_from_LIMS"
  
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


get_progress_from_LIMS <- function(mlrassoarea, fiscalyear, projecttypename) {
  
  url <-"https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_progress_from_LIMS"
  
  args <- list(p_mlrassoarea = mlrassoarea, p_fy = fiscalyear, p_projecttypename = projecttypename)
  
  d.progress <- parseWebReport(url, args)
  
  # return data.frame
  return(d.progress)
  
  }


get_project_correlation_from_LIMS <- function(mlrassoarea, fiscalyear, projectname) {
  
  # nasty hack to trick R CMD check
  musym <- NULL
  new_musym <- NULL
  
  url <-"https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_project_correlation_from_LIMS"
  
  args <- list(p_mlrassoarea = mlrassoarea, p_fy = fiscalyear, p_projectname = projectname)
  
  d.rcor <- parseWebReport(url, args)
  
  # compute musym_orig for additional lmapunits, necessary to catch changes to the original musym, due to a constraint on the lmapunit table that prevents duplicate musym for additional mapunits 
  d.rcor <- within(d.rcor, {
    
    n         = nchar(musym)
    begin_1   = substr(musym, 2, n)
    end_1     = substr(musym, 1, n - 1)
    end_4     = substr(musym, 1, n - 4)
      
    idx       = musym != new_musym & !is.na(new_musym)
      
    orig_musym = ifelse(idx & musym != begin_1 & new_musym == begin_1, begin_1, musym)
    # Joe recommended using |\\+${1}, but appears to be legit in some cases
    orig_musym = ifelse(idx & musym != end_1   & new_musym == end_1 , end_1   , orig_musym)
    orig_musym = ifelse(idx & musym != end_4   & new_musym == end_4 , end_4   , orig_musym)
    })
  d.rcor[c("n", "begin_1", "end_1", "end_4", "idx")] <- NULL
  
  # return data.frame
  return(d.rcor)
  
  }


fetchLIMS_component <- function(projectname, rmHzErrors = FALSE, fill = FALSE, 
                                stringsAsFactors = default.stringsAsFactors()
                                ) {
  
  # load data in pieces
  f.mapunit   <- get_mapunit_from_LIMS(projectname, stringsAsFactors = stringsAsFactors)
  f.component <- get_component_from_LIMS(projectname, stringsAsFactors = stringsAsFactors)
  f.chorizon  <- get_chorizon_from_LIMS(projectname, fill, stringsAsFactors = stringsAsFactors)
  
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

  