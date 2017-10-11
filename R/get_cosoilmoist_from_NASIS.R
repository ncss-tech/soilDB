get_cosoilmoist_from_NASIS <- function(impute = TRUE) {
  # must have RODBC installed
  if (!requireNamespace('RODBC')) stop('please install the `RODBC` package', call.=FALSE)
  
  q.cosoilmoist <- "SELECT dmuiidref AS dmuiid, coiid, compname, comppct_r, month, flodfreqcl, pondfreqcl, cosoilmoistiid, soimoistdept_l, soimoistdept_r, soimoistdept_h, soimoistdepb_l, soimoistdepb_r, soimoistdepb_h, soimoiststat
  
  FROM component_View_1 co LEFT OUTER JOIN
       comonth com ON com.coiidref = co.coiid LEFT OUTER JOIN
       cosoilmoist cosm ON cosm.comonthiidref = com.comonthiid
  
  ORDER BY dmuiid, comppct_r DESC, compname, month, soimoistdept_r
  ;"
  
  
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  
  # exec query
  d.cosoilmoist <- RODBC::sqlQuery(channel, q.cosoilmoist, stringsAsFactors = FALSE)
  
  
  # recode metadata domains
  d.cosoilmoist <- uncode(d.cosoilmoist)
  
  
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