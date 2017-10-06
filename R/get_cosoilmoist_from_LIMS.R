get_cosoilmoist_from_LIMS <- function(uprojectid, impute = TRUE) {
  
  # check for required packages
  if (!requireNamespace('RCurl', quietly=TRUE))
    stop('please install the `RCurl` package', call.=FALSE)
  
  url <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=WEB-cosoimoist_by_uprojectid&p_uprojectid=", uprojectid)
  
  report_html <- RCurl::getURLContent(url, ssl.verifypeer = FALSE)
  report_list <- XML::readHTMLTable(report_html, stringsAsFactors = FALSE)
  
  d.cosoilmoist <- do.call("rbind", report_list)
  
  # column names
  row.names(d.cosoilmoist) <- 1:nrow(d.cosoilmoist)
  names(d.cosoilmoist) <- gsub("\n", "", names(d.cosoilmoist))
  names(d.cosoilmoist) <- tolower(names(d.cosoilmoist))
  
  orig_names <- names(d.cosoilmoist)
  
  # set factor levels according to metadata domains
  d.cosoilmoist <- uncode(d.cosoilmoist, NASIS = FALSE)
  
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

