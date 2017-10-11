get_cosoilmoist_from_SDA <- function(WHERE = NULL, duplicates = FALSE, impute = TRUE){
  q.cosoilmoist <- paste("SELECT", 
  if (duplicates == FALSE) {"DISTINCT"}
, "nationalmusym, muname, c.compname, c.comppct_r, month, flodfreqcl, pondfreqcl, soimoistdept_l, soimoistdept_r, soimoistdept_h, soimoistdepb_l, soimoistdepb_r, soimoistdepb_h, soimoiststat
      
  FROM legend l INNER JOIN
       mapunit mu ON mu.lkey = l.lkey INNER JOIN",
       if (duplicates == FALSE) {
          "(SELECT MIN(nationalmusym) nationalmusym2, MIN(mukey) AS mukey2 
          FROM mapunit
          GROUP BY nationalmusym) AS mu2 ON mu2.nationalmusym2 = mu.nationalmusym INNER JOIN
          (SELECT compname, comppct_r, cokey, mukey AS mukey2 FROM component) AS c ON c.mukey2 = mu2.mukey2"
          } else {"(SELECT compname, comppct_r, cokey, mukey AS mukey2 FROM component) AS c ON c.mukey2 = mu.mukey"}
       , "LEFT OUTER JOIN
       comonth cm ON cm.cokey = c.cokey LEFT OUTER JOIN
       cosoilmoist csm ON csm.comonthkey = cm.comonthkey
  
  WHERE ", WHERE,

  "ORDER BY mu.muname, comppct_r DESC, compname DESC, month, soimoistdept_r ASC
  ;")
  
  # exec query
  d.cosoilmoist <- SDA_query(q.cosoilmoist)
  
  # set factor levels according to metadata domains
  d.cosoilmoist <- uncode(d.cosoilmoist, db = "SDA")
  
  # cache original column names
  orig_names <- names(d.cosoilmoist)
  
  # relabel names
  names(d.cosoilmoist) <- gsub("^soimoist", "", names(d.cosoilmoist))
  old_names <- "stat"
  new_names <- "status"
  names(d.cosoilmoist)[names(d.cosoilmoist) %in% old_names] <- new_names


  # impute NA freqcl values, default = "not populated"
  if (impute == TRUE) {
    vars <- c("flodfreqcl", "pondfreqcl")
    missing <- "Not_Populated"
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
  
  
  # done
  return(d.cosoilmoist)
  }
