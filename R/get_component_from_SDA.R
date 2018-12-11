## what is causing duplication?
## STATSGO?
# https://github.com/ncss-tech/soilDB/issues/38
# https://github.com/ncss-tech/soilDB/issues/36

get_component_from_SDA <- function(WHERE = NULL, duplicates = FALSE, childs = TRUE, stringsAsFactors = default.stringsAsFactors()){
  
  # SDA is missing soiltempa_r AS mast_r
  # Joining in the fetch on derived_cokey doesn't work but should. There are duplicate components with the same combination of elements.
  # paste0("mu.nationalmusym + '_' + CAST(comppct_r AS VARCHAR) + '_' + compname + '-' + ISNULL(localphase, 'no_phase') AS derived_cokey")
  
  c.vars <- "compname, comppct_r, compkind, majcompflag, localphase, slope_r, drainagecl, elev_r, aspectrep, map_r, airtempa_r, reannualprecip_r, ffd_r, earthcovkind1, earthcovkind2, erocl, tfact, wei, weg, nirrcapcl, nirrcapscl, irrcapcl, irrcapscl, frostact, hydgrp, corcon, corsteel, taxclname, taxorder, taxsuborder, taxgrtgroup, taxsubgrp, taxpartsize, taxpartsizemod, taxceactcl, taxreaction, taxtempcl, taxmoistscl, taxtempregime, soiltaxedition, cokey"
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

  
  # parent material
  q.pm <- paste0(
    "SELECT co.cokey, pmg.copmgrpkey, pmgroupname, pmorder, pmkind, pmorigin
    
    FROM
    component co                                        LEFT OUTER JOIN
    copmgrp   pmg ON pmg.cokey         = co.cokey AND
                       pmg.rvindicator = 'Yes'          LEFT OUTER JOIN
    copm      pm  ON pm.copmgrpkey     = pmg.copmgrpkey

    WHERE co.cokey IN (", paste0(d.component$cokey, collapse = ", "), ")
    
    ORDER BY co.cokey, pmg.copmgrpkey, pmorder"
    )
  
  
  # append child tables
  if (childs == TRUE) {
    
    # exec query
    d.pm <- SDA_query(q.pm)
    
    # prep
    d.pm <- .copm_prep(d.pm, db = "SDA")
    
    # merge
    d.component <- merge(d.component, d.pm, by = "cokey", all.x = TRUE)
    }
  
  
  
  # landform
  q.lf <- paste0(
    "SELECT co.cokey, ls.geomfname landscape, lf.geomfname landform, lf.geomfeatid, lf.existsonfeat,
            geomposmntn mntn, geomposhill hill, geompostrce trce, geomposflats flats,
            shapeacross, shapedown,
            hillslopeprof
    
    FROM
    component co

    LEFT OUTER JOIN
        cogeomordesc ls ON ls.cokey       = co.cokey   AND
                           ls.rvindicator = 'Yes'      AND
                           ls.geomftname  = 'Landscape'
    LEFT OUTER JOIN
        cogeomordesc lf ON lf.cokey       = co.cokey   AND
                           lf.rvindicator = 'Yes'      AND
                           lf.geomftname  = 'Landform'
    LEFT OUTER JOIN 
        cosurfmorphgc  lf_3d ON lf_3d.cogeomdkey = lf.cogeomdkey
    LEFT OUTER JOIN
        cosurfmorphss  lf_ss ON lf_ss.cogeomdkey = lf.cogeomdkey
    LEFT OUTER JOIN
        cosurfmorphhpp lf_2d ON lf_2d.cogeomdkey = lf.cogeomdkey

    WHERE co.cokey IN (", paste0(d.component$cokey, collapse = ", "), ")
    
    ORDER BY cokey, ls.geomftname, ls.geomfeatid, ls.existsonfeat, lf.geomftname, lf.geomfeatid, lf.existsonfeat"
    )
  
  
  # append child tables
  if (childs == TRUE) {
    
    # exec query
    d.cogmd <- SDA_query(q.lf)
    
    # prep
    d.cogmd <- .cogmd_prep(d.cogmd, db = "SDA")
    
    # merge
    d.component <- merge(d.component, d.cogmd, by = "cokey", all.x = TRUE)
    }
  
  
  # done
  return(d.component)
  }



get_cointerp_from_SDA <- function(WHERE = NULL, duplicates = FALSE, stringsAsFactors = default.stringsAsFactors()) {
  
  d.component <- get_component_from_SDA(WHERE = WHERE, duplicates = duplicates, childs = FALSE, 
                                        stringsAsFactors = stringsAsFactors
  )
  
  q.cointerp <- paste0("

  SELECT 
  co.cokey, mrulename, max_seqnum, interpll, interplr, interphr, interphh, interpllc, interplrc, interphrc, interphhc
  
  FROM 
  component co                          LEFT OUTER JOIN
  cointerp  coi ON coi.cokey = co.cokey

  INNER JOIN
      (SELECT co2.cokey, coi2.mrulekey, MAX(seqnum) max_seqnum
       FROM 
       component co2                       LEFT OUTER JOIN
       cointerp  coi2 ON coi2.cokey = co2.cokey
       WHERE co2.cokey IN ('", paste0(d.component$cokey, collapse = "', '"), "')
       GROUP BY co2.cokey, coi2.mrulekey
      ) coi22 ON coi22.cokey = co.cokey AND coi22.mrulekey = coi.mrulekey
  
  WHERE co.cokey IN ('", paste0(d.component$cokey, collapse = "', '"), "') AND
        mrulename = rulename
  
  ORDER BY co.cokey ASC;"
  )
  
  d.cointerp <- SDA_query(q.cointerp)
  
  # recode metadata domains
  d.cointerp <- uncode(d.cointerp, db = "SDA", stringsAsFactors = stringsAsFactors)
  
  return(d.cointerp)
  }



get_mapunit_from_SDA <- function(WHERE = NULL, stringsAsFactors = default.stringsAsFactors()) {

  q.mapunit <- paste("
  SELECT 
  mlraoffice, areasymbol, areaname, areaacres, ssastatus, cordate, projectscale, invesintens,
  mukey, nationalmusym, musym, muname, mukind, mustatus, muacres, farmlndcl,
  pct_hydric, pct_component, n_component, n_majcompflag, l.lkey

  FROM
  legend  l                      INNER JOIN
  mapunit mu ON mu.lkey = l.lkey LEFT OUTER JOIN
  
  --components
  (SELECT  co.mukey co_mukey, 
   SUM(comppct_r * CASE WHEN hydricrating = 'Yes' THEN 1 ELSE 0 END) pct_hydric,
   SUM(comppct_r)                                                    pct_component,
   COUNT(*)                                                          n_component,
   SUM(CASE WHEN majcompflag  = 'Yes' THEN 1 ELSE 0 END)             n_majcompflag
   
   FROM     component co
   GROUP BY co.mukey
  ) co ON co.co_mukey = mu.mukey
                     
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
  , "hzname, hzdept_r, hzdepb_r, texture, texcl, fragvol_r, sandtotal_l, sandtotal_r, sandtotal_h, silttotal_l, silttotal_r, silttotal_h, claytotal_l, claytotal_r, claytotal_h, om_l, om_r, om_h, dbthirdbar_l, dbthirdbar_r, dbthirdbar_h, ksat_l, ksat_r, ksat_h, awc_l, awc_r, awc_h, lep_r, sar_r, ec_r, cec7_r, sumbases_r, ph1to1h2o_l, ph1to1h2o_r, ph1to1h2o_h, caco3_l, caco3_r, caco3_h, c.cokey

  FROM legend l INNER JOIN
       mapunit mu ON mu.lkey = l.lkey INNER JOIN",
  if (duplicates == FALSE) { paste("
  (SELECT MIN(nationalmusym) nationalmusym2, MIN(mukey) AS mukey2 
   FROM mapunit
   GROUP BY nationalmusym) AS mu2 ON mu2.nationalmusym2 = mu.nationalmusym
  ")
  } else { paste("
   (SELECT nationalmusym, mukey
    FROM mapunit) AS mu2 ON mu2.mukey = mu.mukey
   ")},
  "INNER JOIN
   component    c    ON c.mukey      = mu.mukey   INNER JOIN
   chorizon     ch   ON ch.cokey     = c.cokey    LEFT OUTER JOIN
   chtexturegrp chtg ON chtg.chkey   = ch.chkey   
                     AND rvindicator = 'YES'      LEFT OUTER JOIN
   chtexture    cht  ON cht.chtgkey  = chtg.chkey

   LEFT OUTER JOIN
       (SELECT SUM(fragvol_r) fragvol_r, ch2.chkey
        FROM chorizon ch2
        INNER JOIN chfrags chf ON chf.chkey = ch2.chkey
        WHERE fraghard IN ('indurated', 'strongly cemented', 'strongly cemented') OR 
              fraghard IS NULL
        GROUP BY ch2.chkey) chfrags2  ON chfrags2.chkey = ch.chkey
  
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
      texcl = factor(tolower(texcl), levels = metadata[metadata$ColumnPhysicalName == "texcl", "ChoiceName"])
    }
  })
  
  # Note: only chtexturegrp$texdesc from SDA matches metadata[metadata$ColumnPhysicalName == "texcl", "ChoiceName"] in metadata
  # # recode metadata domains
  # d.chorizon <- uncode(d.chorizon, NASIS = FALSE)
  
  # done
  return(d.chorizon)
}


fetchSDA_component <- function(WHERE = NULL, duplicates = FALSE, childs = TRUE, rmHzErrors = FALSE, 
                               stringsAsFactors = default.stringsAsFactors()
                               ) {

  # load data in pieces
  f.component <- get_component_from_SDA(WHERE, duplicates, childs, stringsAsFactors = stringsAsFactors)
  # f.mapunit   <- get_mapunit_from_SDA(WHERE, stringsAsFactors = stringsAsFactors)
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
  return(f.chorizon)
  
}
