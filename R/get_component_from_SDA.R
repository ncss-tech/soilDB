## what is causing duplication?
## STATSGO?
# https://github.com/ncss-tech/soilDB/issues/38
# https://github.com/ncss-tech/soilDB/issues/36

get_component_from_SDA <- function(WHERE = NULL, duplicates = FALSE, childs = TRUE,
                                   droplevels = TRUE, nullFragsAreZero = TRUE,
                                   stringsAsFactors = default.stringsAsFactors()
                                   ) {
  if(!duplicates & grepl(WHERE, pattern = "mukey")[1])
    warning("duplicates is set to FALSE and 'mukey' is in WHERE clause. Note: 'mukey' omitted from result.", call.=FALSE)

  # SDA is missing soiltempa_r AS mast_r
  # Joining in the fetch on derived_cokey doesn't work but should. There are duplicate components with the same combination of elements.
  # paste0("mu.nationalmusym + '_' + CAST(comppct_r AS VARCHAR) + '_' + compname + '-' + ISNULL(localphase, 'no_phase') AS derived_cokey")

  es.vars <- "ecoclasstypename, ecoclassref, ecoclassid, ecoclassname"
  co.vars <- "co.cokey, compname, comppct_r, compkind, majcompflag, localphase, drainagecl, hydricrating, erocl, earthcovkind1, earthcovkind2, elev_r, slope_r, aspectrep, map_r, airtempa_r, reannualprecip_r, ffd_r, hydgrp,  nirrcapcl, nirrcapscl, irrcapcl, irrcapscl, tfact, wei, weg, corcon, corsteel, frostact, taxclname, taxorder, taxsuborder, taxgrtgroup, taxsubgrp, taxpartsize, taxpartsizemod, taxceactcl, taxreaction, taxtempcl, taxmoistscl, taxtempregime, soiltaxedition"
  vars    <- paste0(unlist(strsplit(co.vars, "earthcovkind2,")), collapse = paste0("earthcovkind2, ", es.vars, ","))

  q.component <- paste(

    # excluding mukey conditionally and DISTINCT are necessary, otherwise querying on states like CA% exceed SDAs record limit
    "SELECT",
    if (duplicates == FALSE) "DISTINCT" else "mu.mukey AS mukey,",
    "mu.nationalmusym,", vars,

    "FROM
     legend  l                      INNER JOIN
     mapunit mu ON mu.lkey = l.lkey INNER JOIN",

    if (duplicates == FALSE) {
    "(SELECT nationalmusym AS nationalmusym2, MIN(mukey) AS mukey2
      FROM mapunit
      GROUP BY nationalmusym
     ) AS mu2 ON mu2.nationalmusym2 = mu.nationalmusym INNER JOIN"
      } else "",

    "(SELECT", vars, ", mukey AS mukey2
      FROM
      component  co                        LEFT OUTER JOIN
      coecoclass ce ON ce.cokey = co.cokey AND
                       ecoclasstypename IN ('NRCS Rangeland Site', 'NRCS Forestland Site')
      ) AS co ON co.mukey2 =", if (duplicates == FALSE) "mu2.mukey2" else "mu.mukey",

   "WHERE", WHERE,

  "ORDER BY nationalmusym, comppct_r DESC, compname;")


  # exec query
  d.component <- SDA_query(q.component)

  # empty result set should short circuit (no error, just message)
  if(length(d.component) == 0)
    return(d.component)

  # recode metadata domains
  d.component <- uncode(d.component, db = "SDA",
                        droplevels = droplevels,
                        stringsAsFactors = stringsAsFactors
                        )

  # if mukeys are "flattened" to nmusym, make sure the mukey column _exists_ but is empty (NA)
  # presence of NA used to make it clear to user whether they need to set the duplicates flag TRUE,
  # depending on their use case (i.e. need all unique MUKEYS, set duplicates=TRUE; need unique data? duplicates=FALSE)
  if(duplicates == FALSE) {
    d.component <- cbind(mukey = NA, d.component)
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
    d.pm    <- SDA_query(q.pm)
    d.cogmd <- SDA_query(q.lf)
    d.cosrf <- .get_cosurffrags_from_SDA(unique(d.component$cokey), nullFragsAreZero = nullFragsAreZero)

    # prep
    d.pm    <- .copm_prep(d.pm, db = "SDA")
    d.cogmd <- .cogmd_prep(d.cogmd, db = "SDA")

    # merge
    d.component <- merge(d.component, d.pm,    by = "cokey", all.x = TRUE)
    d.component <- merge(d.component, d.cogmd, by = "cokey", all.x = TRUE)
    d.component <- merge(d.component, d.cosrf, by = "cokey", all.x = TRUE)

    # r.rf.data.v2 nullFragsAreZero = TRUE
    idx <- grepl("surface_", names(d.component))
    if (nullFragsAreZero == nullFragsAreZero) {
      d.component[idx] <- lapply(d.component[idx], function(x) ifelse(is.na(x), 0, x))
      }
    }


  # fix for multiple ecosites linked to 1 component
  idx <- table(d.component$cokey) > 1

  if (any(idx)) {

    cokeys <- as.integer(names(idx[idx == TRUE]))
    idx    <- d.component$cokey %in% cokeys

    assign('component.ecosite.problems', value = cokeys, envir = soilDB.env)
    message("-> QC: multiple ecosites linked to 1 component\n\tUse `get('component.ecosite.problems', envir = soilDB.env)` for component keys (cokey)")

    nodups <- {
      d.component[idx, ] ->.;
      split(., .$cokey)  ->.;
      lapply(., function(x) {
        temp                  = x[1, ]
        temp$ecoclassname     = paste0(x$ecoclassname, collapse = ", ")
        temp$ecoclasstypename = paste0(x$ecoclasstypename, collapse = ", ")
        temp$ecoclassref      = paste0(x$ecoclassref,  collapse = ", ")
        temp$ecoclassid       = paste0(x$ecoclassid,   collapse = ", ")
        return(temp)
      }) ->.;
      do.call("rbind", .) ->.;
    }
    d.component <- rbind(d.component[! idx, ], nodups)
    d.component <- with(d.component,
                        d.component[order(nationalmusym, - comppct_r, compname), ]
                        )
    }


  # done
  return(d.component)
}



.get_cosurffrags_from_SDA <- function(cokey, nullFragsAreZero = nullFragsAreZero) {

  q <- paste("

  -- find sfragsize_r
  CREATE TABLE #RF1 (cokey INT, cosurffragskey INT, sfragcov_r REAL,
                     shape CHAR(7), para INT, nonpara INT, fragsize_r2 INT);

  INSERT INTO  #RF1 (cokey, cosurffragskey, sfragcov_r, shape, para, nonpara, fragsize_r2)

  SELECT
  cokey, cosurffragskey, sfragcov_r,
  -- shape
  CASE WHEN sfragshp = 'Nonflat' OR sfragshp IS NULL THEN 'nonflat' ELSE 'flat' END shape,
  -- hardness
  CASE WHEN sfraghard IN ('Extremely weakly cemented', 'Very weakly cemented', 'Weakly cemented', 'Weakly cemented*', 'Moderately cemented', 'Moderately cemented*', 'soft')             THEN 1 ELSE NULL END para,
  CASE WHEN sfraghard IN ('Strongly cemented', 'Strongly cemented*', 'Very strongly cemented', 'Extremely strong', 'Indurated', 'hard')
       OR sfraghard IS NULL
       THEN 1 ELSE NULL END nonpara,
  -- sfragsize_r
  CASE WHEN sfragsize_r IS NOT NULL THEN sfragsize_r
       WHEN sfragsize_r IS NULL     AND sfragsize_h IS NOT NULL AND sfragsize_l IS NOT NULL
       THEN (sfragsize_h + sfragsize_l) / 2
       WHEN sfragsize_h IS NOT NULL THEN sfragsize_h
       WHEN sfragsize_l IS NOT NULL THEN sfragsize_l
       ELSE NULL END
       sfragsize_r2

  FROM cosurffrags

  WHERE cokey IN (", paste0(cokey, collapse = ", "), ")

  ORDER BY cokey, cosurffragskey;


  -- compute logicals
  CREATE TABLE #RF2 (cokey INT, cosurffragskey INT, sfragcov_r REAL, para INT, nonpara INT,                      fine_gravel INT, gravel INT, cobbles INT, stones INT, boulders INT,                        channers INT, flagstones INT, unspecified INT
                     );
  INSERT INTO  #RF2 (cokey, cosurffragskey, sfragcov_r, para, nonpara,
                     fine_gravel, gravel, cobbles, stones, boulders, channers, flagstones,
                     unspecified
                     )
  SELECT
  cokey, cosurffragskey, sfragcov_r, para, nonpara,
  -- fragments
  CASE WHEN   fragsize_r2 >= 2  AND fragsize_r2 < 5   AND shape = 'nonflat'
       THEN 1 ELSE NULL
       END fine_gravel,
  CASE WHEN   fragsize_r2 >= 2  AND fragsize_r2 < 75  AND shape = 'nonflat'
       THEN 1 ELSE NULL
       END gravel,
  CASE WHEN   fragsize_r2 >= 75 AND fragsize_r2 < 250 AND shape = 'nonflat'
       THEN 1 ELSE NULL
       END cobbles,
  CASE WHEN ((fragsize_r2 >= 250 AND fragsize_r2 < 600 AND shape = 'nonflat') OR
             (fragsize_r2 >= 380 AND fragsize_r2 < 600 AND shape = 'flat'))
       THEN 1 ELSE NULL END stones,
  CASE WHEN   fragsize_r2 >= 600
       THEN 1 ELSE NULL
       END boulders,
  CASE WHEN   fragsize_r2 >= 2   AND fragsize_r2 < 150 AND shape = 'flat'
       THEN 1 ELSE NULL
       END channers,
  CASE WHEN   fragsize_r2 >= 150 AND fragsize_r2 < 380 AND shape = 'flat'
       THEN 1 ELSE NULL
       END flagstones,
  CASE WHEN   fragsize_r2 IS NULL
       THEN 1 ELSE NULL
       END unspecified

  FROM #RF1

  ORDER BY cokey, cosurffragskey;


  -- summarize rock fragments
  SELECT
  cokey,
  -- nonpara rock fragments
  SUM(sfragcov_r * fine_gravel * nonpara)  surface_fine_gravel,
  SUM(sfragcov_r * gravel      * nonpara)  surface_gravel,
  SUM(sfragcov_r * cobbles     * nonpara)  surface_cobbles,
  SUM(sfragcov_r * stones      * nonpara)  surface_stones,
  SUM(sfragcov_r * boulders    * nonpara)  surface_boulders,
  SUM(sfragcov_r * channers    * nonpara)  surface_channers,
  SUM(sfragcov_r * flagstones  * nonpara)  surface_flagstones,
  -- para rock fragments
  SUM(sfragcov_r * fine_gravel * para)     surface_parafine_gravel,
  SUM(sfragcov_r * gravel      * para)     surface_paragravel,
  SUM(sfragcov_r * cobbles     * para)     surface_paracobbles,
  SUM(sfragcov_r * stones      * para)     surface_parastones,
  SUM(sfragcov_r * boulders    * para)     surface_paraboulders,
  SUM(sfragcov_r * channers    * para)     surface_parachanners,
  SUM(sfragcov_r * flagstones  * para)     surface_paraflagstones,
  -- unspecified
  SUM(sfragcov_r * unspecified)            surface_unspecified,
  -- total_frags_pct_para
  SUM(sfragcov_r               * nonpara)  surface_total_frags_pct_nopf,
  -- total_frags_pct
  SUM(sfragcov_r)                          surface_total_frags_pct

  FROM #RF2

  GROUP BY cokey

  ORDER BY cokey;

  -- cleanup
  DROP TABLE #RF1;
  DROP TABLE #RF2;
  ")

  d <- SDA_query(q)

  if (is.null(d)) {
    d <- data.frame(
      cokey = cokey[1],
      surface_fine_gravel = as.integer(NA),
      surface_gravel      = as.integer(NA),
      surface_cobbles     = as.integer(NA),
      surface_stones      = as.integer(NA),
      surface_boulders    = as.integer(NA),
      surface_channers    = as.integer(NA),
      surface_flagstones  = as.integer(NA),
      surface_parafine_gravel = as.integer(NA),
      surface_paragravel      = as.integer(NA),
      surface_paracobbles     = as.integer(NA),
      surface_parastones      = as.integer(NA),
      surface_paraboulders    = as.integer(NA),
      surface_parachanners    = as.integer(NA),
      surface_paraflagstones  = as.integer(NA),
      surface_unspecified     = as.integer(NA),
      surface_total_frags_pct_nopf = as.integer(NA),
      surface_total_frags_pct      = as.integer(NA))
  }

  # set NA values as integer NA
  idx <- sapply(d, function(x) all(is.na(x)))
  d[idx] <- lapply(d[idx], function(x) x = as.integer(NA))

  return(d)

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
                       if (!is.null(mrulename)) paste0(" AND mrulename = '", mrulename, "'"), "
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
   component    c    ON c.mukey      = mu.mukey   LEFT JOIN
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
  if (!is.null(d.chorizon) && nrow(d.chorizon) > 0){
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
                            CASE WHEN   fragsize_r2 >= 2   AND fragsize_r2 < 5   AND shape = 'nonflat' THEN 1 ELSE NULL END fine_gravel,
                            CASE WHEN   fragsize_r2 >= 2   AND fragsize_r2 < 75  AND shape = 'nonflat' THEN 1 ELSE NULL END gravel,
                            CASE WHEN   fragsize_r2 >= 75  AND fragsize_r2 < 250 AND shape = 'nonflat' THEN 1 ELSE NULL END cobbles,
                            CASE WHEN ((fragsize_r2 >= 250 AND fragsize_r2 < 600 AND shape = 'nonflat') OR
                            (fragsize_r2 >= 380 AND fragsize_r2 <= 600 AND shape = 'flat'))
                            THEN 1 ELSE NULL END stones,
                            CASE WHEN   fragsize_r2 >= 600 THEN 1 ELSE NULL END boulders,
                            CASE WHEN   fragsize_r2 >= 2   AND fragsize_r2 < 150 AND shape = 'flat' THEN 1 ELSE NULL END channers,
                            CASE WHEN   fragsize_r2 >= 150 AND fragsize_r2 < 380 AND shape = 'flat' THEN 1 ELSE NULL END flagstones,
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
  # } else {
  #   return(structure(list(chkey = integer(0), hzname = character(0), hzdept_r = integer(0), 
  #                         hzdepb_r = integer(0), texture = character(0), texcl = character(0), 
  #                         fragvol_l = integer(0), fragvol_r = integer(0), fragvol_h = integer(0), 
  #                         sandtotal_l = numeric(0), sandtotal_r = numeric(0), sandtotal_h = numeric(0), 
  #                         silttotal_l = numeric(0), silttotal_r = numeric(0), silttotal_h = numeric(0), 
  #                         claytotal_l = numeric(0), claytotal_r = numeric(0), claytotal_h = numeric(0), 
  #                         om_l = numeric(0), om_r = numeric(0), om_h = numeric(0), 
  #                         dbthirdbar_l = numeric(0), dbthirdbar_r = numeric(0), dbthirdbar_h = numeric(0), 
  #                         ksat_l = numeric(0), ksat_r = numeric(0), ksat_h = numeric(0), 
  #                         awc_l = numeric(0), awc_r = numeric(0), awc_h = numeric(0), 
  #                         lep_r = numeric(0), sar_r = numeric(0), ec_r = numeric(0), 
  #                         cec7_r = numeric(0), sumbases_r = numeric(0), ph1to1h2o_l = numeric(0), 
  #                         ph1to1h2o_r = numeric(0), ph1to1h2o_h = numeric(0), caco3_l = integer(0), 
  #                         caco3_r = integer(0), caco3_h = integer(0), kwfact = character(0), 
  #                         kffact = character(0), cokey = integer(0), fine_gravel = numeric(0), 
  #                         gravel = numeric(0), cobbles = numeric(0), stones = numeric(0), 
  #                         boulders = numeric(0), channers = numeric(0), flagstones = numeric(0), 
  #                         parafine_gravel = numeric(0), paragravel = numeric(0), paracobbles = numeric(0), 
  #                         parastones = numeric(0), paraboulders = numeric(0), parachanners = numeric(0), 
  #                         paraflagstones = numeric(0), unspecified = numeric(0), total_frags_pct_nopf = numeric(0), 
  #                         total_frags_pct = numeric(0)), row.names = integer(0), class = "data.frame"))
  }

  # done
  return(d.chorizon)
}


.get_diagnostics_from_SDA <- function(target_cokeys) {
  # query SDA to get corresponding codiagfeatures
  q <- paste0('SELECT * FROM codiagfeatures WHERE cokey IN ', format_SQL_in_statement(target_cokeys), ";")
  return(SDA_query(q))
}


.get_restrictions_from_SDA <- function(target_cokeys) {
  # query SDA to get corresponding corestrictions
  q <- paste0('SELECT * FROM corestrictions WHERE cokey IN ', format_SQL_in_statement(target_cokeys), ";")
  return(SDA_query(q))
}




#' @title Get SSURGO/STATSGO2 Mapunit Data from Soil Data Access
#'
#' @description Functions to download and flatten commonly used tables and from Soil Data
#' Access, and create soil profile collection objects (SPC).
#'
#' @details These functions return data from Soil Data Access with the use of a simple
#' text string that formatted as an SQL WHERE clause (e.g. \code{WHERE =
#' "areasymbol = 'IN001'"}. All functions are SQL queries that wrap around
#' \code{SDAquery()} and format the data for analysis.
#'
#' Beware SDA includes the data for both SSURGO and STATSGO2. The
#' \code{areasymbol} for STATSGO2 is \code{US}. For just SSURGO, include
#' \code{WHERE = "areareasymbol != 'US'"}.
#'
#' If the duplicates argument is set to TRUE, duplicate components are
#' returned. This is not necessary with data returned from NASIS, which has one
#' unique national map unit. SDA has duplicate map national map units, one for
#' each legend it exists in.
#'
#' The value of \code{nullFragsAreZero} will have a significant impact on the
#' rock fragment fractions returned by \code{fetchSDA}. Set
#' \code{nullFragsAreZero = FALSE} in those cases where there are many
#' data-gaps and NULL rock fragment values should be interpreted as NULLs. Set
#' \code{nullFragsAreZero = TRUE} in those cases where NULL rock fragment
#' values should be interpreted as 0.
#' 
#' Additional examples can be found in the [Soil Data Access (SDA) Tutorial](http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.html)
#' 
#' @aliases fetchSDA get_legend_from_SDA get_lmuaoverlap_from_SDA
#' get_mapunit_from_SDA get_component_from_SDA get_chorizon_from_SDA
#' get_cosoilmoist_from_SDA get_cointerp_from_SDA
#' @param WHERE text string formatted as an SQL WHERE clause (default: FALSE)
#' @param duplicates logical; if TRUE a record is returned for each unique
#' mukey (may be many per nationalmusym)
#' @param childs logical; if FALSE parent material and geomorphic child tables
#' are not flattened and appended
#' @param nullFragsAreZero should fragment volumes of NULL be interpreted as 0?
#' (default: TRUE), see details
#' @param rmHzErrors should pedons with horizonation errors be removed from the
#' results? (default: FALSE)
#' @param droplevels logical: indicating whether to drop unused levels in
#' classifying factors. This is useful when a class has large number of unused
#' classes, which can waste space in tables and figures.
#' @param stringsAsFactors logical: should character vectors be converted to
#' factors? This argument is passed to the uncode() function. It does not
#' convert those vectors that have set outside of uncode() (i.e. hard coded).
#' The 'factory-fresh' default is TRUE, but this can be changed by setting
#' options(stringsAsFactors = FALSE)
#' @return A data.frame or SoilProfileCollection object.
#' @author Stephen Roecker
#' @seealso \link{SDA_query}
#' @keywords manip
#'
#' @export fetchSDA
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
                                        nullFragsAreZero = TRUE,
                                        stringsAsFactors = stringsAsFactors
                                        )
  # f.mapunit   <- get_mapunit_from_SDA(WHERE, stringsAsFactors = stringsAsFactors)

  # AGB update: only query component horizon for cokeys in the component result (subject to user-specified WHERE clause)
  f.chorizon  <- get_chorizon_from_SDA(paste0('c.cokey IN', format_SQL_in_statement(unique(f.component$cokey))),
                                       duplicates = duplicates,
                                       droplevels = droplevels
                                       )

  # diagnostic features and restrictions
  f.diag <- .get_diagnostics_from_SDA(f.component$cokey)
  f.restr <- .get_restrictions_from_SDA(f.component$cokey)

  # optionally test for bad horizonation... flag, and remove
  if (rmHzErrors) {
    f.chorizon.test <- aqp::checkHzDepthLogic(f.chorizon, hzdepths = c('hzdept_r', 'hzdepb_r'),
                                              idname = 'cokey', fast = TRUE)

    # which are the good (valid) ones?
    good.ids <- as.character(f.chorizon.test$cokey[which(f.chorizon.test$valid)])
    bad.ids  <- as.character(f.chorizon.test$cokey[which(! f.chorizon.test$valid)])

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
  if ('chkey' %in% aqp::horizonNames(f.chorizon) && all(!is.na('chkey'))) {
      hzidname(f.chorizon) <- 'chkey'
  }

  # set optional hz designation and texture slots
  hzdesgnname(f.chorizon) <- "hzname"
  hztexclname(f.chorizon) <- "texture"

  # add diagnostics
  if(is.data.frame(f.diag)) {
    diagnostic_hz(f.chorizon) <- f.diag
  }

  # add restrictions
  if(is.data.frame(f.restr)) {
    restrictions(f.chorizon) <- f.restr
  }

  # print any messages on possible data quality problems:
  if (exists('component.hz.problems', envir=soilDB.env))
    message("-> QC: horizon errors detected, use `get('component.hz.problems', envir=soilDB.env)` for component keys (cokey)")

  # done, return SPC
  return(f.chorizon)

}

