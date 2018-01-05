get_cosoilmoist_from_LIMS <- function(projectname, impute = TRUE, stringsAsFactors = default.stringsAsFactors()) {
  
  # check for required packages
  url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=WEB-cosoimoist_by_projectname"

  d.cosoilmoist <- lapply(projectname, function(x) {
    args = list(p_projectname = x)
    d    =  parseWebReport(url, args)
  })
  d.cosoilmoist <- do.call("rbind", d.cosoilmoist)

  # set factor levels according to metadata domains
  d.cosoilmoist <- uncode(d.cosoilmoist, db = "LIMS", stringsAsFactors = stringsAsFactors)
  
  # prep dataset: rename columns, impute empty values, stringsAsFactors
  d.cosoilmoist <- .cosoilmoist_prep(d.cosoilmoist, impute = impute, stringsAsFactors = stringsAsFactors)
  
  # return data.fram
  return(d.cosoilmoist)
  }


get_sitesoilmoist_from_LIMS <- function(usiteid) {
  
  url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_sitesoilmoist_from_LIMS"
  
  args <- list(p_usiteid = usiteid)
  
  d.sitesoilmoist <- parseWebReport(url,args)
  
  # set factor levels according to metadata domains
  d.sitesoilmoist <- uncode(d.sitesoilmoist, db="LIMS")
  
  # relabel names
  names(d.sitesoilmoist) <- gsub("^soimoist", "", names(d.sitesoilmoist))
  old_names <- "ten"
  new_names <- "tension"
  names(d.sitesoilmoist)[names(d.sitesoilmoist) %in% old_names] <- new_names
  
  return(d.sitesoilmoist)

  }
