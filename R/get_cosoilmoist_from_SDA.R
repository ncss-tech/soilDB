get_cosoilmoist_from_SDA <- function(WHERE = NULL, duplicates = FALSE, impute = TRUE, 
                                     stringsAsFactors = default.stringsAsFactors()
                                     ) {
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
  d.cosoilmoist <- uncode(d.cosoilmoist, db = "SDA", stringsAsFactors = stringsAsFactors)
  
  # prep dataset: rename columns, impute empty values, stringsAsFactors
  d.cosoilmoist <- .cosoilmoist_prep(d.cosoilmoist, impute = impute, stringsAsFactors = stringsAsFactors)
  
  # done
  return(d.cosoilmoist)
  }
