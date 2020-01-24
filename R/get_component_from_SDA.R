## what is causing duplication?
## STATSGO?
# https://github.com/ncss-tech/soilDB/issues/38
# https://github.com/ncss-tech/soilDB/issues/36

get_component_from_SDA <- function(WHERE = NULL, duplicates = FALSE, childs = TRUE, 
                                   droplevels = TRUE,
                                   stringsAsFactors = default.stringsAsFactors()
                                   ) {
  if(!duplicates & grepl(WHERE, pattern = "mukey")[1])
    warning("duplicates is set to FALSE and 'mukey' is in WHERE clause. Note: 'mukey' omitted from result.", call.=FALSE)
  
  # SDA is missing soiltempa_r AS mast_r
  # Joining in the fetch on derived_cokey doesn't work but should. There are duplicate components with the same combination of elements.
  # paste0("mu.nationalmusym + '_' + CAST(comppct_r AS VARCHAR) + '_' + compname + '-' + ISNULL(localphase, 'no_phase') AS derived_cokey")
  
  c.vars <- "cokey, compname, comppct_r, compkind, majcompflag, localphase, slope_r, drainagecl, hydricrating, elev_r, aspectrep, map_r, airtempa_r, reannualprecip_r, ffd_r, earthcovkind1, earthcovkind2, erocl, tfact, wei, weg, nirrcapcl, nirrcapscl, irrcapcl, irrcapscl, frostact, hydgrp, corcon, corsteel, taxclname, taxorder, taxsuborder, taxgrtgroup, taxsubgrp, taxpartsize, taxpartsizemod, taxceactcl, taxreaction, taxtempcl, taxmoistscl, taxtempregime, soiltaxedition"
  es.vars <- "ecoclassname, ecoclasstypename, ecoclassref, ecoclassid"

  q.component <- paste("SELECT", if (duplicates == FALSE) { "DISTINCT" } else { "mu.mukey AS mukey," }, "mu.nationalmusym,", c.vars, ",", es.vars,
      
  "FROM legend l INNER JOIN
       mapunit mu ON mu.lkey = l.lkey INNER JOIN",
  
  if (duplicates == FALSE) {
    paste("
    (SELECT MIN(nationalmusym) nationalmusym2, MIN(mukey) AS mukey2 FROM mapunit GROUP BY nationalmusym) AS mu2 ON mu2.nationalmusym2 = mu.nationalmusym 
      INNER JOIN (SELECT", c.vars, ", mukey AS mukey2 FROM component) AS c ON c.mukey2 = mu2.mukey2 
      LEFT OUTER JOIN (SELECT cokey AS cokey2,", es.vars, "FROM coecoclass WHERE ecoclasstypename IN ('NRCS Rangeland Site', 'NRCS Forestland Site')) AS ces ON c.cokey = ces.cokey2")
  } else {
    paste("
    (SELECT", c.vars, ", mukey AS mukey2 FROM component) AS c ON c.mukey2 = mu.mukey 
      LEFT OUTER JOIN (SELECT cokey AS cokey2,", es.vars, "FROM coecoclass WHERE ecoclasstypename IN ('NRCS Rangeland Site', 'NRCS Forestland Site')) AS ces ON c.cokey = ces.cokey2")
  }
          
  , "WHERE", WHERE,
  
  "ORDER BY nationalmusym, comppct_r DESC, compname;")
  
  
  # exec query
  d.component <- SDA_query(q.component)
  
  
  # recode metadata domains
  d.component <- uncode(d.component, db = "SDA", 
                        droplevels = droplevels,
                        stringsAsFactors = stringsAsFactors
                        )

  # if mukeys are "flattened" to nmusym, make sure the mukey column _exists_ but is empty (NA)
  # presence of NA used to make it clear to user whether they need to set the duplicates flag TRUE, 
  # depending on their use case (i.e. need all unique MUKEYS, set duplicates=TRUE; need unique data? duplicates=FALSE)
  if(duplicates == FALSE) {
    d.component$mukey <- NA
  }
  
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



get_cointerp_from_SDA <- function(WHERE = NULL, mrulename = NULL, duplicates = FALSE, 
                                  droplevels = TRUE,
                                  stringsAsFactors = default.stringsAsFactors()
                                  ) {
  
  d.component <- get_component_from_SDA(WHERE = WHERE, duplicates = duplicates, 
                                        childs = FALSE,
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
       WHERE co2.cokey IN ('", paste0(d.component$cokey, collapse = "', '"), "')",
                       if (!is.null(mrulename)) paste0(" AND mrulename = '", mrulename), " 
       GROUP BY co2.cokey, coi2.mrulekey
      ) coi22 ON coi22.cokey = co.cokey AND coi22.mrulekey = coi.mrulekey
  
  WHERE co.cokey IN ('", paste0(d.component$cokey, collapse = "', '"), "') AND
        mrulename = rulename
  
  ORDER BY co.cokey ASC"
  )
  
  d.cointerp <- SDA_query(q.cointerp)
  
  # recode metadata domains
  d.cointerp <- uncode(d.cointerp, db = "SDA", 
                       droplevels = droplevels,
                       stringsAsFactors = stringsAsFactors
                       )
  
  return(d.cointerp)
  }


get_legend_from_SDA <- function(WHERE = NULL, droplevels = TRUE, stringsAsFactors = default.stringsAsFactors()) {
  q.legend  <- paste("
                     SELECT
                     mlraoffice, areasymbol, areaname, areatypename, CAST(areaacres AS INTEGER) AS areaacres, ssastatus, 
                     CAST(projectscale AS INTEGER) projectscale, cordate, 
                     CAST(l.lkey AS INTEGER) lkey, COUNT(mu.mukey) n_mukey
                     
                     FROM       legend  l  
                     INNER JOIN mapunit mu ON mu.lkey = l.lkey
                     
                     WHERE", WHERE,
                     
                     "GROUP BY mlraoffice, areasymbol, areaname, areatypename, areaacres, ssastatus, projectscale, cordate, l.lkey
                     
                     ORDER BY areasymbol
                     ;")
  
  # exec query
  d.legend <- SDA_query(q.legend)
  
  # recode metadata domains
  d.legend <- uncode(d.legend,
                     db = "SDA", 
                     droplevels = droplevels,
                     stringsAsFactors   = stringsAsFactors
  )
  
  
  # done
  return(d.legend)
}



get_lmuaoverlap_from_SDA <- function(WHERE = NULL, droplevels = TRUE, stringsAsFactors = default.stringsAsFactors()) {

  q <- paste("SELECT
             legend.areasymbol, legend.areaname, legend.areaacres, 
             lao.areatypename lao_areatypename, lao.areasymbol lao_areasymbol, lao.areaname lao_areaname, lao.areaovacres lao_areaovacres,
             mu.mukey, musym, nationalmusym, muname, mustatus, muacres,
             muao.areaovacres muao_areaovacres
             
             FROM 
             legend                                   INNER JOIN 
             mapunit     mu ON mu.lkey  = legend.lkey    
             
             INNER JOIN             
                 laoverlap  lao ON lao.lkey       = legend.lkey
             
             INNER JOIN
                 muaoverlap muao ON muao.mukey       = mu.mukey
                                AND muao.lareaovkey  = lao.lareaovkey
             
             WHERE", WHERE, 
             
             "ORDER BY legend.areasymbol, mu.musym, legend.areatypename
             ;"
  )
  
  # exec query
  d <- SDA_query(q)
  
  d$musym = as.character(d$musym)
  
  # recode metadata domains
  d <- uncode(d,
              db = "NASIS", 
              droplevels = droplevels,
              stringsAsFactors = stringsAsFactors
  )
  
  # done
  return(d)
}



get_mapunit_from_SDA <- function(WHERE = NULL, 
                                 droplevels = TRUE,
                                 stringsAsFactors = default.stringsAsFactors()
                                 ) {

  q.mapunit <- paste("
  SELECT 
  areasymbol, l.lkey, mukey, nationalmusym, musym, muname,
  mukind, mustatus, invesintens, muacres, farmlndcl,
  pct_component, pct_hydric, n_component, n_majcompflag

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
  
  d.mapunit$musym = as.character(d.mapunit$musym)
  
  # recode metadata domains
  d.mapunit <- uncode(d.mapunit, 
                      db = "SDA", 
                      droplevels = droplevels,
                      stringsAsFactors   = stringsAsFactors
                      )
  
  
  # cache original column names
  orig_names <- names(d.mapunit)
  
  
  # done
  return(d.mapunit)
}



get_chorizon_from_SDA <- function(WHERE = NULL, duplicates = FALSE, 
                                  childs = TRUE, 
                                  nullFragsAreZero = TRUE, 
                                  droplevels = TRUE,
                                  stringsAsFactors = default.stringsAsFactors()
                                  ) {

  q.chorizon <- paste("
  SELECT", 
  if (duplicates == FALSE) {"DISTINCT"}
  , "hzname, hzdept_r, hzdepb_r, texture, texcl, fragvol_l, fragvol_r, fragvol_h, sandtotal_l, sandtotal_r, sandtotal_h, silttotal_l, silttotal_r, silttotal_h, claytotal_l, claytotal_r, claytotal_h, om_l, om_r, om_h, dbthirdbar_l, dbthirdbar_r, dbthirdbar_h, ksat_l, ksat_r, ksat_h, awc_l, awc_r, awc_h, lep_r, sar_r, ec_r, cec7_r, sumbases_r, ph1to1h2o_l, ph1to1h2o_r, ph1to1h2o_h, caco3_l, caco3_r, caco3_h, kwfact, kffact, c.cokey, ch.chkey

  FROM legend l INNER JOIN
       mapunit mu ON mu.lkey = l.lkey INNER JOIN",
  if (duplicates == FALSE) { paste("
  (SELECT MIN(nationalmusym) nationalmusym2, MIN(mukey) AS mukey2 
   FROM mapunit
   GROUP BY nationalmusym) AS mu2 ON mu2.mukey2 = mu.mukey
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
       (SELECT SUM(fragvol_l) fragvol_l, SUM(fragvol_r) fragvol_r, SUM(fragvol_h) fragvol_h, ch2.chkey
        FROM chorizon ch2
        INNER JOIN chfrags chf ON chf.chkey = ch2.chkey

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
    if (droplevels == droplevels & is.factor(texcl)) {
      texcl = droplevels(texcl)
      }
    })
  
  # Note: only chtexturegrp$texdesc from SDA matches metadata[metadata$ColumnPhysicalName == "texcl", "ChoiceName"] in metadata
  # # recode metadata domains
  # d.chorizon <- uncode(d.chorizon, NASIS = FALSE)
  
  if (childs == TRUE) {
    
    WHERE = paste0("WHERE co.cokey IN (", paste0(unique(d.chorizon$cokey), collapse = ","), ")")
    
    q.chfrags <- paste("
                          
                          -- find fragsize_r
                          CREATE TABLE #RF1 (cokey INT, chkey INT, chfragskey INT, fragvol_r REAL,
                          shape CHAR(7), para INT, nonpara INT, fragsize_r2 INT);
                          
                          INSERT INTO  #RF1 (cokey, chkey, chfragskey, fragvol_r, shape, para, nonpara, fragsize_r2)
                          SELECT             co.cokey, ch.chkey, chfragskey, fragvol_r,
                          -- shape
                          CASE WHEN fragshp = 'Nonflat' OR fragshp IS NULL THEN 'nonflat' ELSE 'flat' END shape,
                          -- hardness
                          CASE WHEN fraghard IN ('Extremely weakly cemented', 'Very weakly cemented', 'Weakly cemented', 'Weakly cemented*', 'Moderately cemented', 'Moderately cemented*', 'soft')                     THEN 1 ELSE NULL END para,
                          CASE WHEN fraghard IN ('Strongly cemented', 'Strongly cemented*', 'Very strongly cemented', 'Extremely strong', 'Indurated', 'hard')   OR fraghard IS NULL THEN 1 ELSE NULL END nonpara,
                          -- fragsize_r
                          CASE WHEN fragsize_r IS NOT NULL THEN fragsize_r
                          WHEN fragsize_r IS NULL     AND fragsize_h IS NOT NULL AND fragsize_l IS NOT NULL 
                          THEN (fragsize_h + fragsize_l) / 2
                          WHEN fragsize_h IS NOT NULL THEN fragsize_h
                          WHEN fragsize_l IS NOT NULL THEN fragsize_l
                          ELSE NULL END
                          fragsize_r2
                          
                          FROM
                          component co                        LEFT OUTER JOIN
                          chorizon  ch ON ch.cokey = co.cokey LEFT OUTER JOIN
                          chfrags   cf ON cf.chkey = ch.chkey",
                          
                          WHERE = WHERE
                          
                          ,"
                          ORDER BY co.cokey, ch.chkey, cf.chfragskey;
                          
                          
                          -- compute logicals
                          CREATE TABLE #RF2 (
                          cokey INT, chkey INT, chfragskey INT, fragvol_r REAL, para INT, nonpara INT, 
                          fine_gravel INT, gravel INT, cobbles INT, stones INT, boulders INT, channers INT, flagstones INT,
                          unspecified INT
                          );
                          INSERT INTO  #RF2 (
                          cokey, chkey, chfragskey, fragvol_r, para, nonpara, 
                          fine_gravel, gravel, cobbles, stones, boulders, channers, flagstones,
                          unspecified
                          )
                          SELECT 
                          cokey, chkey, chfragskey, fragvol_r, para, nonpara,
                          -- fragments
                          CASE WHEN   fragsize_r2 >= 2  AND fragsize_r2 <= 5   AND shape = 'nonflat' THEN 1 ELSE NULL END fine_gravel,
                          CASE WHEN   fragsize_r2 >= 2  AND fragsize_r2 <= 76  AND shape = 'nonflat' THEN 1 ELSE NULL END gravel,
                          CASE WHEN   fragsize_r2 > 76  AND fragsize_r2 <= 250 AND shape = 'nonflat' THEN 1 ELSE NULL END cobbles,
                          CASE WHEN ((fragsize_r2 > 250 AND fragsize_r2 <= 600 AND shape = 'nonflat') OR
                          (fragsize_r2 >= 380 AND fragsize_r2 < 600 AND shape = 'flat'))
                          THEN 1 ELSE NULL END stones,
                          CASE WHEN   fragsize_r2 > 600 THEN 1 ELSE NULL END boulders,
                          CASE WHEN   fragsize_r2 >= 2  AND fragsize_r2 <= 150 AND shape = 'flat' THEN 1 ELSE NULL END channers,
                          CASE WHEN   fragsize_r2 > 150 AND fragsize_r2 <= 380 AND shape = 'flat' THEN 1 ELSE NULL END flagstones,
                          CASE WHEN   fragsize_r2 IS NULL                                         THEN 1 ELSE NULL END unspecified 
                          
                          FROM
                          #RF1
                          
                          ORDER BY cokey, chkey, chfragskey;
                          
                          
                          -- summarize rock fragments
                          SELECT
                          chkey,
                          -- nonpara rock fragments
                          SUM(fragvol_r * fine_gravel * nonpara)  fine_gravel,
                          SUM(fragvol_r * gravel      * nonpara)  gravel,
                          SUM(fragvol_r * cobbles     * nonpara)  cobbles,
                          SUM(fragvol_r * stones      * nonpara)  stones,
                          SUM(fragvol_r * boulders    * nonpara)  boulders,
                          SUM(fragvol_r * channers    * nonpara)  channers,
                          SUM(fragvol_r * flagstones  * nonpara)  flagstones,
                          -- para rock fragments
                          SUM(fragvol_r * fine_gravel * para)     parafine_gravel,
                          SUM(fragvol_r * gravel      * para)     paragravel,
                          SUM(fragvol_r * cobbles     * para)     paracobbles,
                          SUM(fragvol_r * stones      * para)     parastones,
                          SUM(fragvol_r * boulders    * para)     paraboulders,
                          SUM(fragvol_r * channers    * para)     parachanners,
                          SUM(fragvol_r * flagstones  * para)     paraflagstones,
                          -- unspecified
                          SUM(fragvol_r * unspecified)            unspecified,
                          -- total_frags_pct_para
                          SUM(fragvol_r               * nonpara)  total_frags_pct_nopf,
                          -- total_frags_pct
                          SUM(fragvol_r)                          total_frags_pct
                          
                          FROM #RF2
                          
                          GROUP BY cokey, chkey
                          
                          ORDER BY cokey, chkey;
                          
                          
                          -- cleanup
                          DROP TABLE #RF1;
                          DROP TABLE #RF2;
                          ")
    
    d.chfrags  <- SDA_query(q.chfrags)
    
    # r.rf.data.v2 nullFragsAreZero = TRUE
    idx <- !names(d.chfrags) %in% "chkey"
    if (nullFragsAreZero == TRUE) {
      d.chfrags[idx] <- lapply(d.chfrags[idx], function(x) ifelse(is.na(x), 0, x))
    }
    
    d.chorizon <- merge(d.chorizon, d.chfrags, all.x = TRUE, by = "chkey")
    
  }
  
  # done
  return(d.chorizon)
}


.get_diagnostics_from_SDA <- function(target_cokeys) {
  # query SDA to get corresponding codiagfeatures
  q <- paste0('SELECT * FROM codiagfeatures WHERE cokey IN ', format_SQL_in_statement(target_cokeys), ";")
  return(SDA_query(q))
}

fetchSDA <- function(WHERE = NULL, duplicates = FALSE, childs = TRUE, 
                     nullFragsAreZero = TRUE, 
                     rmHzErrors = FALSE,
                     droplevels = TRUE,
                     stringsAsFactors = default.stringsAsFactors()
                     ) {

  # load data in pieces
  f.component <- get_component_from_SDA(WHERE, 
                                        duplicates = duplicates, 
                                        childs = childs, 
                                        droplevels = droplevels,
                                        stringsAsFactors = stringsAsFactors
                                        )
  # f.mapunit   <- get_mapunit_from_SDA(WHERE, stringsAsFactors = stringsAsFactors)
  
  # AGB update: only query component horizon for cokeys in the component result (subject to user-specified WHERE clause)
  f.chorizon  <- get_chorizon_from_SDA(paste0('c.cokey IN', format_SQL_in_statement(unique(f.component$cokey))), 
                                       duplicates = duplicates, 
                                       droplevels = droplevels
                                       )
  
  # diagnostic features  
  f.diag <- .get_diagnostics_from_SDA(f.component$cokey)
  
  # optionally test for bad horizonation... flag, and remove
  if (rmHzErrors) {
    f.chorizon.test <- plyr::ddply(f.chorizon, 'cokey', function(d) {
      res <- aqp::hzDepthTests(top=d[['hzdept_r']], bottom=d[['hzdepb_r']])
      return(all(!res))
    })
    
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
  
  # set SDA/SSURGO-specific horizon identifier
  hzidname(f.chorizon) <- 'chkey'
  
  # set optional hz designation and texture slots
  hzdesgnname(f.chorizon) <- "hzname"
  hztexclname(f.chorizon) <- "texture"
  
  # add diagnostics
  if(is.data.frame(f.diag)) {
    diagnostic_hz(f.chorizon) <- f.diag
  }
  
  # print any messages on possible data quality problems:
  if (exists('component.hz.problems', envir=soilDB.env))
    message("-> QC: horizon errors detected, use `get('component.hz.problems', envir=soilDB.env)` for related cokey values")
  
  # done, return SPC
  return(f.chorizon)
  
}
