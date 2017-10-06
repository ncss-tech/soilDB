get_component_from_LIMS <- function(uprojectid) {
  
  # check for required packages
  if (!requireNamespace('RCurl', quietly=TRUE))
    stop('please install the `RCurl` package', call.=FALSE)
  
  url <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=WEB-component_by_uprojectid&p_uprojectid=", uprojectid)
  
  report_html <- RCurl::getURLContent(url, ssl.verifypeer = FALSE)
  report_list <- XML::readHTMLTable(report_html, stringsAsFactors = FALSE)
  
  d.component <- do.call("rbind", report_list)
  
  # data.frame names
  row.names(d.component) <- 1:nrow(d.component)
  names(d.component) <- gsub("\n", "", names(d.component))
  names(d.component) <- tolower(names(d.component))
  
  orig_names <- names(d.component)
  
  # set factor levels according to metadata domains
  d.component <- uncode(d.component, NASIS = FALSE)
  
  # fix column classes, for some reason all the data is getting imported as characters
  d.component <- suppressWarnings(data.frame(lapply(d.component, .fix_class), stringsAsFactors = FALSE))
  
  # return data.frame
  return(d.component)
  }


get_chorizon_from_LIMS <- function(uprojectid, fill = FALSE) {
  
  # check for required packages
  if (!requireNamespace('RCurl', quietly=TRUE))
    stop('please install the `RCurl` package', call.=FALSE)
  
  url <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=WEB-chorizon_by_uprojectid&p_uprojectid=", uprojectid)
  
  report_html <- RCurl::getURLContent(url, ssl.verifypeer = FALSE)
  report_list <- XML::readHTMLTable(report_html, stringsAsFactors = FALSE)
  
  d.chorizon <- do.call("rbind", report_list)
  
  # data.frame names
  row.names(d.chorizon) <- 1:nrow(d.chorizon)
  names(d.chorizon) <- gsub("\n", "", names(d.chorizon))
  names(d.chorizon) <- tolower(names(d.chorizon))
  
  orig_names <- names(d.chorizon)
  
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
  
  # fix column classes, for some reason all the data is getting imported as characters
  fix_class = function(x) {
    if (class(x) == "character" & any(!is.na(as.numeric(x)))) {as.numeric(x)} else x
    }
  
  # fix column classes, for some reason all the data is getting imported as characters
  d.chorizon <- suppressWarnings(data.frame(lapply(d.chorizon, fix_class), stringsAsFactors = FALSE))
  
  # fill
  if (fill == FALSE) {
    d.chorizon <- d.chorizon[!is.na(d.chorizon$chiid), ]
    }
  
  # return data.frame
  return(d.chorizon)
  
  }


get_mapunit_from_LIMS <- function(uprojectid) {
  
  # check for required packages
  if (!requireNamespace('RCurl', quietly=TRUE))
    stop('please install the `RCurl` package', call.=FALSE)
  
  url <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=WEB-mapunit_by_uprojectid&p_uprojectid=", uprojectid)
  
  report_html <- RCurl::getURLContent(url, ssl.verifypeer = FALSE)
  report_list <- XML::readHTMLTable(report_html, stringsAsFactors = FALSE)
  
  d.mapunit <- do.call("rbind", report_list)
  
  # data.frame names
  row.names(d.mapunit) <- 1:nrow(d.mapunit)
  names(d.mapunit) <- gsub("\n", "", names(d.mapunit))
  names(d.mapunit) <- tolower(names(d.mapunit))
  
  orig_names <- names(d.mapunit)
  
  # set factor levels according to metadata domains
  d.mapunit <- uncode(d.mapunit, NASIS = FALSE)
  
  # fix column classes, for some reason all the data is getting imported as characters
  d.mapunit <- suppressWarnings(data.frame(lapply(d.mapunit, .fix_class), stringsAsFactors = FALSE))
  
  # return data.frame
  return(d.mapunit)
}

fetchLIMS_component <- function(uprojectid, rmHzErrors = FALSE, fill = FALSE) {
  
  # check for required packages
  if (!requireNamespace('RCurl', quietly=TRUE))
    stop('please install the `RCurl` package', call.=FALSE)
  
  
  # load data in pieces
  f.mapunit <- get_mapunit_from_LIMS(uprojectid)
  f.component <- get_component_from_LIMS(uprojectid)
  f.chorizon <- get_chorizon_from_LIMS(uprojectid, fill)
  
  # optionally test for bad horizonation... flag, and remove
  if (rmHzErrors) {
    f.chorizon.test <- plyr::ddply(f.chorizon, 'cokey', test_hz_logic, topcol='hzdept_r', bottomcol='hzdepb_r', strict=TRUE)
    
    # which are the good (valid) ones?
    good.ids <- as.character(f.chorizon.test$cokey[which(f.chorizon.test$hz_logic_pass)])
    bad.ids <- as.character(f.chorizon.test$cokey[which(! f.chorizon.test$hz_logic_pass)])
    
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

  