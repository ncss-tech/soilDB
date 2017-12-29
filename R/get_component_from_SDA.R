get_component_from_SDA <- function(WHERE = NULL, duplicates = FALSE, stringsAsFactors = default.stringsAsFactors()){
  # SDA is missing soiltempa_r AS mast_r
  # Joining in the fetch on derived_cokey doesn't work but should. There are duplicate components with the same combination of elements.
  # paste0("mu.nationalmusym + '_' + CAST(comppct_r AS VARCHAR) + '_' + compname + '-' + ISNULL(localphase, 'no_phase') AS derived_cokey")
  c.vars <- "compname, comppct_r, compkind, majcompflag, localphase, slope_r, tfact, wei, weg, drainagecl, elev_r, aspectrep, map_r, airtempa_r, reannualprecip_r, ffd_r, nirrcapcl, nirrcapscl, irrcapcl, irrcapscl, frostact, hydgrp, corcon, corsteel, taxclname, taxorder, taxsuborder, taxgrtgroup, taxsubgrp, taxpartsize, taxpartsizemod, taxceactcl, taxreaction, taxtempcl, taxmoistscl, taxtempregime, soiltaxedition, cokey"
  q.component <- paste("
  SELECT", 
  if (duplicates == FALSE) {"DISTINCT"} else {"mukey,"}
  , "mu.nationalmusym,", c.vars,
      
  "FROM legend l INNER JOIN
       mapunit mu ON mu.lkey = l.lkey INNER JOIN",
  if (duplicates == FALSE) {
    paste("(SELECT MIN(nationalmusym) nationalmusym2, MIN(mukey) AS mukey2 
    FROM mapunit
    GROUP BY nationalmusym) AS mu2 ON mu2.nationalmusym2 = mu.nationalmusym INNER JOIN
    (SELECT", c.vars, ", mukey AS mukey2 FROM component) AS c ON c.mukey2 = mu2.mukey2")
  } else {paste("(SELECT", c.vars, ", mukey AS mukey2 FROM component) AS c ON c.mukey2 = mu.mukey")}
          
  , "WHERE", WHERE,
  
  "ORDER BY nationalmusym, comppct_r DESC, compname
  ;")
  
  
  # exec query
  d.component <- SDA_query(q.component)

  
  # recode metadata domains
  d.component <- uncode(d.component, db = "SDA", stringsAsFactors = stringsAsFactors)
  
  
  # cache original column names
  orig_names <- names(d.component)
  
  # reorder columns
  # d.component <- with(d.component, { d.component[order(nationalmusym, - comppct_r, compname), ]})
  
  # done
  return(d.component)
  }



get_mapunit_from_SDA <- function(WHERE = NULL, stringsAsFactors = default.stringsAsFactors()) {

  q.mapunit <- paste("SELECT areasymbol, areaname, ssastatus, cordate, nationalmusym, mukey, musym, muname, mukind, mustatus, muacres, farmlndcl
  
  FROM  mapunit mu INNER JOIN
        legend l ON l.lkey = mu.lkey
                     
  WHERE", WHERE,
  
  "ORDER BY areasymbol, muname;")
  
  
  # exec query
  d.mapunit <- SDA_query(q.mapunit)

  
  # recode metadata domains
  d.mapunit <- uncode(d.mapunit, db = "SDA", stringsAsFactors = stringsAsFactors)
  
  
  # cache original column names
  orig_names <- names(d.mapunit)
  
  
  # done
  return(d.mapunit)
}



get_chorizon_from_SDA <- function(WHERE = NULL, duplicates = FALSE, stringsAsFactors = default.stringsAsFactors()) {
  # seriously!, how is their no fragvoltot_r column?
  q.chorizon <- paste("
  SELECT", 
  if (duplicates == FALSE) {"DISTINCT"}
  , "hzname, hzdept_r, hzdepb_r, texture, sandtotal_l, sandtotal_r, sandtotal_h, silttotal_l, silttotal_r, silttotal_h, claytotal_l, claytotal_r, claytotal_h, om_l, om_r, om_h, dbovendry_r, ksat_r, awc_l, awc_r, awc_h, lep_r, sar_r, ec_r, cec7_r, sumbases_r, ph1to1h2o_l, ph1to1h2o_r, ph1to1h2o_h, c.cokey

  FROM legend l INNER JOIN
       mapunit mu ON mu.lkey = l.lkey INNER JOIN",
  if (duplicates == FALSE) {paste("
  (SELECT MIN(nationalmusym) nationalmusym2, MIN(mukey) AS mukey2 
  FROM mapunit
  GROUP BY nationalmusym) AS 
  mu2 ON mu2.nationalmusym2 = mu.nationalmusym INNER JOIN
  (SELECT compname, comppct_r, cokey, mukey AS mukey2 FROM component) AS c ON c.mukey2 = mu2.mukey2")
  } else {paste("
    (SELECT compname, comppct_r, cokey, mukey AS mukey2 FROM component) AS c ON c.mukey2 = mu.mukey")}, "INNER JOIN
  chorizon ch ON ch.cokey = c.cokey LEFT OUTER JOIN
  chtexturegrp chtg ON chtg.chkey = ch.chkey AND rvindicator = 'YES'
  
  WHERE", WHERE,
                      
  "ORDER BY c.cokey, hzdept_r ASC;")
  
  # exec query
  d.chorizon <- SDA_query(q.chorizon)
  
  ## TODO: might be nice to abstract this into a new function
  # hacks to make R CMD check --as-cran happy:
  metadata <- NULL
  # load local copy of metadata
  load(system.file("data/metadata.rda", package="soilDB")[1])

  # transform variables and metadata
  d.chorizon <- within(d.chorizon, {
    nationalmusym = NULL
    texture = tolower(texture)
    if (stringsAsFactors == TRUE) {
      texture = factor(texture, levels = metadata[metadata$ColumnPhysicalName == "texcl", "ChoiceName"])
    }
  })
  
  # Note: only chtexturegrp$texdesc from SDA matches metadata[metadata$ColumnPhysicalName == "texcl", "ChoiceName"] in metadata
  # # recode metadata domains
  # d.chorizon <- uncode(d.chorizon, NASIS = FALSE)
  
  # done
  return(d.chorizon)
}


fetchSDA_component <- function(WHERE = NULL, duplicates = FALSE, rmHzErrors = FALSE, 
                               stringsAsFactors = default.stringsAsFactors()
                               ) {

  # load data in pieces
  f.component <- get_component_from_SDA(WHERE, duplicates, stringsAsFactors = stringsAsFactors)
  f.mapunit   <- get_mapunit_from_SDA(WHERE, stringsAsFactors = stringsAsFactors)
  f.chorizon  <- get_chorizon_from_SDA(WHERE, duplicates)
  
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
  depths(f.chorizon) <- cokey ~ hzdept_r + hzdepb_r
  
  
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
