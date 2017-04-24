get_component_from_SDA_db <- function(nationalmusym = NULL, areasymbol = NULL, impute = TRUE){
  # SDA is missing soiltempa_r AS mast_r
  q.component <- paste("SELECT DISTINCT", 
  if (!is.null(areasymbol)) "areasymbol, musym,", 
  "mu.nationalmusym, compname, comppct_r, compkind, majcompflag, localphase, slope_r, tfact, wei, weg, drainagecl, elev_r, aspectrep, map_r, airtempa_r AS maat_r, reannualprecip_r, ffd_r, nirrcapcl, nirrcapscl, irrcapcl, irrcapscl, frostact, hydgrp, corcon, corsteel, taxclname, taxorder, taxsuborder, taxgrtgroup, taxsubgrp, taxpartsize, taxpartsizemod, taxceactcl, taxreaction, taxtempcl, taxmoistscl, taxtempregime, soiltaxedition, nationalmusym + '_' + CAST(comppct_r AS VARCHAR) + '_' + compname AS derived_cokey
      
  FROM mapunit mu INNER JOIN
       component c ON c.mukey = mu.mukey", 
  if (!is.null(areasymbol)) {
    paste0("INNER JOIN legend l ON l.lkey = mu.lkey
          
          WHERE l.areasymbol IN ('", paste0(areasymbol, collapse = "', '"), "')")
    }, # areasymbol = NULL
  if (!is.null(nationalmusym)){
    paste0("WHERE mu.nationalmusym IN ('", paste0(nationalmusym, collapse = "', '"), "')")
    }, # nationalmusym = NULL
  
  "ORDER BY",  if (!is.null(areasymbol)) "areasymbol, musym,", "nationalmusym, comppct_r DESC, compname
  ;")
  
  
  # exec query
  d.component <- SDA_query(q.component)

  
  # recode metadata domains
  d.component <- .metadata_replace(d.component, NASIS = FALSE)
  
  
  # cache original column names
  orig_names <- names(d.component)

  
  # done
  return(d.component)
  }



get_mapunit_from_SDA_db <- function(nationalmusym = NULL, areasymbol = NULL) {

  q.mapunit <- paste("SELECT areasymbol, areaname, ssastatus, cordate, nationalmusym, mukey, musym, muname, mukind, mustatus, muacres, farmlndcl
  
  FROM  mapunit mu INNER JOIN
        legend l ON l.lkey = mu.lkey",

  if (!is.null(areasymbol)){
    paste0("WHERE l.areasymbol IN ('", paste0(areasymbol, collapse = "', '"), "')", sep = " ")},
  if (!is.null(nationalmusym)){
    paste0("WHERE mu.nationalmusym IN ('", paste0(nationalmusym, collapse = "', '"), "')")},
                      
  "ORDER BY areasymbol, musym;")
  
  
  # exec query
  d.mapunit <- SDA_query(q.mapunit)

  
  # recode metadata domains
  d.mapunit <- .metadata_replace(d.mapunit, NASIS = FALSE)
  
  
  # cache original column names
  orig_names <- names(d.mapunit)
  
  
  # done
  return(d.mapunit)
}



get_chorizon_from_SDA_db <- function(nationalmusym = NULL, areasymbol = NULL) {
  # seriously!, how is their no fragvoltot_r column?
  q.chorizon <- paste("SELECT DISTINCT nationalmusym + '_' + CAST(comppct_r AS VARCHAR) + '_' + compname AS derived_cokey, hzname, hzdept_r, hzdepb_r, sandtotal_l, sandtotal_r, sandtotal_h, silttotal_l, silttotal_r, silttotal_h, claytotal_l, claytotal_r, claytotal_h, texture, om_l, om_r, om_h, dbovendry_r, ksat_r, awc_l, awc_r, awc_h, lep_r, sar_r, ec_r, cec7_r, sumbases_r, ph1to1h2o_l, ph1to1h2o_r, ph1to1h2o_h
  
  FROM mapunit mu INNER JOIN
       component c ON c.mukey = mu.mukey INNER JOIN
       chorizon ch ON ch.cokey = c.cokey LEFT OUTER JOIN
       chtexturegrp chtg ON chtg.chkey = ch.chkey AND rvindicator = 'YES'",

  if (!is.null(areasymbol)) {
    paste0("INNER JOIN legend l ON l.lkey = mu.lkey
           WHERE l.areasymbol IN ('", paste0(areasymbol, collapse = "', '"), "')")
    }, # areasymbol = NULL
  if (!is.null(nationalmusym)){
    paste0("WHERE mu.nationalmusym IN ('", paste0(nationalmusym, collapse = "', '"), "')")
    },
  
  "ORDER BY derived_cokey DESC, hzdept_r ASC;")
  
  # exec query
  d.chorizon <- SDA_query(q.chorizon)
  
  # recode metadata domains
  d.chorizon <- .metadata_replace(d.chorizon, NASIS = FALSE)
  
  # done
  return(d.chorizon)
}


fetchSDA_components <- function(nationalmusym = NULL, areasymbol = NULL, rmHzErrors = TRUE) {

  # load data in pieces
  f.component <- get_component_from_SDA_db(nationalmusym, areasymbol)
  f.mapunit <- get_mapunit_from_SDA_db(nationalmusym, areasymbol)
  f.chorizon <- get_chorizon_from_SDA_db(nationalmusym, areasymbol)
  
  # optionally test for bad horizonation... flag, and remove
  if (rmHzErrors) {
    f.chorizon.test <- plyr::ddply(f.chorizon, 'derived_cokey', test_hz_logic, topcol='hzdept_r', bottomcol='hzdepb_r', strict=TRUE)
    
    # which are the good (valid) ones?
    good.ids <- as.character(f.chorizon.test$derived_cokey[which(f.chorizon.test$hz_logic_pass)])
    bad.ids <- as.character(f.chorizon.test$derived_cokey[which(! f.chorizon.test$hz_logic_pass)])
    
    # keep the good ones
    f.chorizon <- f.chorizon[which(f.chorizon$derived_cokey %in% good.ids), ]
    
    # keep track of those components with horizonation errors
    if(length(bad.ids) > 0)
      assign('component.hz.problems', value=bad.ids, envir=soilDB.env)
  }
  
  
  # upgrade to SoilProfilecollection
  depths(f.chorizon) <- derived_cokey ~ hzdept_r + hzdepb_r
  
  
  ## TODO: this will fail in the presence of duplicates
  ## TODO: make this error more informative
  # add site data to object
  site(f.chorizon) <- f.component # left-join via derived_cokey


  # print any messages on possible data quality problems:
  if (exists('component.hz.problems', envir=soilDB.env))
    message("-> QC: horizon errors detected, use `get('component.hz.problems', envir=soilDB.env)` for related nationalmusym values")
  
  # done, return SPC
  return(list(spc = f.chorizon, f.mapunit))
  
}
