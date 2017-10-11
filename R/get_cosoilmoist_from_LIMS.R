get_cosoilmoist_from_LIMS <- function(projectname, impute = TRUE) {
  
  # check for required packages
  url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=WEB-cosoimoist_by_projectname"
  
  args <- list(p_projectname = projectname)
  
  d.cosoilmoist <- parseWebReport(url, args)
  
  # set factor levels according to metadata domains
  d.cosoilmoist <- uncode(d.cosoilmoist, db = "LIMS")
  
  # relabel names
  names(d.cosoilmoist) <- gsub("^soimoist", "", names(d.cosoilmoist))
  old_names <- "stat"
  new_names <- "status"
  names(d.cosoilmoist)[names(d.cosoilmoist) %in% old_names] <- new_names
  
  
  # impute NA freqcl values, default = "not populated"
  if (impute == TRUE) {
    vars <- c("flodfreqcl", "pondfreqcl")
    missing <- "not_populated"
    freqcl2 <- c(missing, levels(d.cosoilmoist$flodfreqcl))
    status2 <- c(missing, levels(d.cosoilmoist$status))
    
    d.cosoilmoist <- within(d.cosoilmoist, {
      # replace NULL RV depths with 201 cm if pondfreqcl or flodqcl is not NULL
      dept_r[is.na(dept_r) & (!is.na(pondfreqcl) | !is.na(flodfreqcl))] = 201
      depb_r[is.na(depb_r) & (!is.na(pondfreqcl) | !is.na(flodfreqcl))] = 201
      
      # replace NULL L and H depths with the RV
      dept_l = ifelse(is.na(dept_l), dept_r, dept_l)
      dept_h = ifelse(is.na(dept_h), dept_r, dept_h)
      
      depb_l = ifelse(is.na(depb_l), depb_r, depb_l)
      depb_h = ifelse(is.na(depb_h), depb_r, depb_h)
      
      # replace NULL freqcl with "Not_Populated"
      status = factor(status, levels = status2)
      flodfreqcl = factor(flodfreqcl, levels = freqcl2)
      pondfreqcl = factor(pondfreqcl, levels = freqcl2)
      
      status[is.na(status)]         <- missing
      flodfreqcl[is.na(flodfreqcl)] <- missing
      pondfreqcl[is.na(pondfreqcl)] <- missing
      })
    }
  
  d.cosoilmoist <- suppressWarnings(data.frame(lapply(d.cosoilmoist, .fix_class)))
  
  # return data.fram
  return(d.cosoilmoist)
  }

