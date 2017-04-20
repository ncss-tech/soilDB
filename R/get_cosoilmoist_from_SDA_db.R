get_cosoimoist_from_SDA_db <- function(mukey, impute = TRUE){
  q.cosoilmoist <- paste0("SELECT mu.nationalmusym, mu.mukey, mu.muname, c.compname, c.comppct_r, month, flodfreqcl, pondfreqcl, soimoistdept_l, soimoistdept_r, soimoistdept_h, soimoistdepb_l, soimoistdepb_r, soimoistdepb_h, soimoiststat
      
  FROM mapunit mu INNER JOIN
       component c ON c.mukey = mu.mukey LEFT OUTER JOIN
       comonth cm ON cm.cokey = c.cokey LEFT OUTER JOIN
       cosoilmoist csm ON csm.comonthkey = cm.comonthkey
  
  WHERE mu.mukey IN ('", paste0(mukey, collapse = "', '"), "')

  ORDER BY muname, comppct_r DESC, compname DESC, month, soimoistdept_r
  ;")
  
  
  # exec query
  d.cosoilmoist <- SDA_query(q.cosoilmoist)
  
  
  # cache original column names
  orig_names <- names(d.cosoilmoist)
  
  
  # relabel names
  names(d.cosoilmoist) <- gsub("^soimoist", "", names(d.cosoilmoist))
  old_names <- "stat"
  new_names <- "status"
  names(d.cosoilmoist)[names(d.cosoilmoist) %in% old_names] <- new_names
  
  
  # Convert to factors
  freqcl <- c("None", "Very rare", "Rare", "Occasional", "Frequent", "Very frequent")
  
  d.cosoilmoist <- within(d.cosoilmoist, {
    month      = factor(month, levels = month.name)
    flodfreqcl = factor(flodfreqcl, levels = freqcl)
    pondfrecl  = factor(pondfreqcl, levels = freqcl)
    })
  
  
  # reorder data.frame
  d.cosoilmoist  <- with(d.cosoilmoist, 
                         d.cosoilmoist[
                           order(muname, - comppct_r, - xtfrm(compname), month, dept_r),
                           ])

  
  # impute NA freqcl values, default = "not populated"
  
  if (impute == TRUE) {
    vars <- c("flodfreqcl", "pondfreqcl")
    missing <- c("Not_Populated")
    freqcl2 <- c(missing, freqcl)
    
    d.cosoilmoist <- within(d.cosoilmoist, {
      flodfreqcl = factor(flodfreqcl, levels = freqcl2)
      pondfreqcl = factor(pondfreqcl, levels = freqcl2)
      flodfreqcl[is.na(flodfreqcl)] <- missing
      pondfreqcl[is.na(pondfreqcl)] <- missing
      dept_l = ifelse(!is.na(dept_l), dept_l, dept_r)
      dept_h = ifelse(!is.na(dept_h), dept_h, dept_r)
      depb_l = ifelse(!is.na(depb_l), depb_l, depb_r)
      depb_h = ifelse(!is.na(depb_h), depb_h, depb_r)
      })
    }
  
  
  # done
  return(d.cosoilmoist)
  }
