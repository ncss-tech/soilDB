
get_component_from_GDB <- function(dsn = "gNATSGO_CONUS.gdb", WHERE = NULL, childs = FALSE, droplevels = TRUE, stringsAsFactors = TRUE) {
  
  message("getting components from ", substr(WHERE, 1, 20), "...")
  qry <- paste(
    "SELECT mukey, cokey, compname, comppct_r, compkind, majcompflag, localphase, drainagecl, hydricrating, erocl, earthcovkind1, earthcovkind2, elev_r, slope_r, aspectrep, map_r, airtempa_r, reannualprecip_r, ffd_r, hydgrp,  nirrcapcl, nirrcapscl, irrcapcl, irrcapscl, tfact, wei, weg, corcon, corsteel, frostact, taxclname, taxorder, taxsuborder, taxgrtgroup, taxsubgrp, taxpartsize, taxpartsizemod, taxceactcl, taxreaction, taxtempcl, taxmoistscl, taxtempregime, soiltaxedition
    
    FROM component
    
    WHERE", WHERE
    )
  co <- read_sf(dsn = dsn, layer = "component", query = qry, as_tibble = FALSE)
    
    
  # childs
  if (childs == TRUE) {
    
    message("getting child tables")
    
    # exec sub query
    pm    <- .get_copmgrp_from_GDB(dsn = dsn, co)
    cogmd <- .get_cogeomordesc_from_GDB(dsn = dsn, co)
    
    # prep
    pm    <- .copm_prep(pm, db = "SDA")
    cogmd <- .cogmd_prep(cogmd, db = "SDA")
      
    # merge
    co <- merge(co, pm,    by = "cokey", all.x = TRUE, sort = FALSE)
    co <- merge(co, cogmd, by = "cokey", all.x = TRUE, sort = FALSE)
  }
  
  # recode metadata domains
  co <- uncode(co,
               db = "SDA",
               droplevels = droplevels,
               stringsAsFactors = stringsAsFactors
  )
  
  # done
  return(co)
  
}



get_legend_from_GDB <- function(dsn = "gNATSGO_CONUS.gdb", WHERE = NULL, droplevels = TRUE, stringsAsFactors = TRUE, stats = FALSE) {
  
  
  if (!is.null(WHERE)) {
    qry <- paste0("SELECT * FROM legend WHERE ", WHERE)
    le <- read_sf(dsn = dsn, layer = "legend", query = qry)
  } else le <- read_sf(dsn = dsn, layer = "legend")
  
  
  if (stats == TRUE) {
    mu  <- read_sf(dsn = dsn, layer = "mapunit", query = paste0("SELECT lkey, mukey FROM mapunit WHERE lkey IN ('", paste0(le$lkey, collapse = "', '"), "')"))
    n_mukey <- aggregate(mukey ~ lkey, data = mu, length)
    names(n_mukey)[2] <- "n_mukey"
    
    le <- merge(le, n_mukey, by = "lkey", all.x = TRUE, sort = FALSE)
  }
  
  
  # recode metadata domains
  le <- uncode(le,
               db = "SDA",
               droplevels = droplevels,
               stringsAsFactors = stringsAsFactors
  )
  
  vars <- c("mlraoffice", "areasymbol", "areaname", "areatypename", "areaacres", "ssastatus", "projectscale", "cordate", "lkey", "n_mukey")
  
  if (stats == TRUE) {
    le <- le[vars]
  } else le <- le[vars[-10]]
  
  # done
  return(le)
  
}



get_mapunit_from_GDB <- function(dsn = "gNATSGO_CONUS.gdb", WHERE = NULL, droplevels = TRUE, stringsAsFactors = TRUE, stats = FALSE) {
  
  message("getting mapunits")
  
  qry <- paste0("SELECT * FROM mapunit WHERE ", WHERE)
  mu  <- read_sf(dsn = dsn, layer = "mapunit", query = qry)
  
  qry <- paste0("SELECT * FROM legend WHERE lkey IN ('", paste0(unique(mu$lkey), collapse = "', '"), "')")
  le <- read_sf(dsn = dsn, layer = "legend", query = qry)
  
  mu <- merge(mu, le, by = "lkey", all.x = TRUE, sort = FALSE)
  mu <- mu[order(mu$areasymbol), ]
  
  
  if (stats == TRUE) {
    
    co <- by(mu, mu$lkey, function(x) {
      
      qry <- paste(
        "SELECT mukey, cokey, comppct_r, majcompflag, hydricrating 
        
         FROM component 
          
         WHERE mukey IN ('", paste0(x$mukey, collapse = "', '"), "')"
        )
      message("getting components from ", unique(x$areasymbol))
      read_sf(dsn = dsn, layer = "component", query = qry)
      })
    co <- do.call("rbind", co)
    
    co <- {
      co$df <- data.frame(mukey         = as.character(0),
                          pct_component = as.integer(0),
                          pct_hydric    = as.integer(0),
                          n_component   = as.integer(0),
                          n_majcompflag = as.integer(0),
                          stringsAsFactors = FALSE
                          )
      split(co, co$mukey, drop = TRUE) ->.;
      lapply(., function(x) {
        df               = x$df[1, ]
        df$mukey         = x$mukey[1]
        df$pct_component = sum(x$comppct_r, na.rm = TRUE)
        df$pct_hydric    = sum(x$hydricrating == "Yes")
        df$n_component   = length(x$cokey)
        df$n_majcompflag = sum(x$majcompflag == "Yes")
        return(df)
      }) ->.;
      do.call("rbind", .) ->.;
    }
    mu <- merge(mu, co, by = "mukey", all.x = TRUE, sort = FALSE)
  }
  
  

  # recode metadata domains
  mu <- uncode(mu,
               db = "SDA",
               droplevels = droplevels,
               stringsAsFactors = stringsAsFactors
               )
  
  vars <- c("areasymbol", "lkey", "mukey", "musym", "muname", "mukind", "mustatus", "invesintens", "muacres", "farmlndcl", "pct_component", "pct_hydric", "n_component", "n_majcompflag")
  
  if (stats == TRUE) {
    mu <- mu[vars]
  } else mu <- mu[vars[1:10]]
  
  
  # done
  return(mu)

}


.get_copmgrp_from_GDB <- function(dsn = dsn, co) {
  
 # message("getting ", unique(substr(x$areasymbol, 1, 2)))
  qry <- paste0(
    "SELECT cokey, copmgrpkey, pmgroupname
    
     FROM copmgrp 
    
     WHERE rvindicator = 'Yes' AND
          cokey IN ('", paste0(co$cokey, collapse = "', '"), "')"
    )
  pmg <- read_sf(dsn = dsn, layer = "copmgrp", query = qry, as_tibble = FALSE)
    
  qry <- paste0(
    "SELECT copmgrpkey, pmorder, pmkind, pmorigin
      
     FROM copm
      
     WHERE copmgrpkey IN ('", paste0(pmg$copmgrpkey, collapse = "', '"), "')"
    )
  pm <- read_sf(dsn = dsn, layer = "copm", query = qry, as_tibble = FALSE)
  
  pm <- merge(pmg, pm, by = "copmgrpkey", all.x = TRUE, sort = FALSE)
  
  pm <- pm[with(pm, order(cokey, copmgrpkey, pmorder)), ]
  
  return(pm)
}


.get_cogeomordesc_from_GDB <- function(dsn = dsn, co) {
  
  qry <- paste0(
      "SELECT cokey, geomftname, geomfname, geomfeatid, existsonfeat, cogeomdkey
      
      FROM cogeomordesc
      
      WHERE rvindicator = 'Yes' AND 
            cokey IN ('", paste0(co$cokey, collapse = "', '"), "')"
    )
    cogmd    <- read_sf(dsn, layer = "cogeomordesc", query = qry, as_tibble = FALSE)
    cogmd_ls <- cogmd[cogmd$geomftname == "Landscape", ]
    cogmd_lf <- cogmd[cogmd$geomftname == "Landform",  ]
    cogmd_lf$geomftname <- NULL
    
    names(cogmd_ls)[names(cogmd_ls) == "geomfname"] <- "landscape"
    names(cogmd_lf)[names(cogmd_lf) == "geomfname"] <- "landform"
    
    
    # geomorphic components
    qry <- paste0(
      "SELECT geomposmntn, geomposhill, geompostrce, geomposflats, cogeomdkey
      
      FROM cosurfmorphgc
      
      WHERE cogeomdkey IN ('", paste0(cogmd_lf$cogeomdkey, collapse = "', '"), "')"
    )
    lf_3d <- read_sf(dsn, layer = "cosurfmorphgc", query = qry, as_tibble = FALSE)
    names(lf_3d)[1:4] <- gsub("geompos", "", names(lf_3d)[1:4])
    
    
    # slope shape
    qry <- paste0(
      "SELECT shapeacross, shapedown, cogeomdkey
      
      FROM cosurfmorphss
      
      WHERE cogeomdkey IN ('", paste0(cogmd_lf$cogeomdkey, collapse = "', '"), "')"
    )
    lf_ss <- read_sf(dsn, layer = "cosurfmorphgc", query = qry, as_tibble = FALSE)
    
    
    # hillslope position
    qry <- paste0(
      "SELECT hillslopeprof, cogeomdkey
      
      FROM cosurfmorphhpp
      
      WHERE cogeomdkey IN ('", paste0(cogmd_lf$cogeomdkey, collapse = "', '"), "')"
    )
    lf_2d <- read_sf(dsn, layer = "cosurfmorphhpp", query = qry, as_tibble = FALSE)
    
    
    # merge results
    cogmd <- merge(cogmd_ls[c("cokey", "landscape")], cogmd_lf, by = "cokey", all.x = TRUE, sort = FALSE)
    cogmd <- merge(cogmd, lf_3d, by = "cogeomdkey", all.x = TRUE, sort = FALSE)
    cogmd <- merge(cogmd, lf_ss, by = "cogeomdkey", all.x = TRUE, sort = FALSE)
    cogmd <- merge(cogmd, lf_2d, by = "cogeomdkey", all.x = TRUE, sort = FALSE)
    cogmd$cogeomdkey <- NULL

  return(cogmd)
}


.get_chorizon_from_GDB <- function(dsn = "gNATSGO_CONUS.gdb", co, droplevels = TRUE, stringsAsFactors = TRUE) {
  
  # chorizon
  qry <- paste0(
  "SELECT cokey, chkey, hzname, hzdept_r, hzdepb_r, sandtotal_l, sandtotal_r, sandtotal_h, silttotal_l, silttotal_r, silttotal_h, claytotal_l, claytotal_r, claytotal_h, om_l, om_r, om_h, dbthirdbar_l, dbthirdbar_r, dbthirdbar_h, ksat_l, ksat_r, ksat_h, awc_l, awc_r, awc_h, lep_r, sar_r, ec_r, cec7_r, sumbases_r, ph1to1h2o_l, ph1to1h2o_r, ph1to1h2o_h, caco3_l, caco3_r, caco3_h, kwfact, kffact
  
  FROM chorizon
  
  WHERE cokey IN ('", paste0(co$cokey, collapse = "', '"), "')" 
  )
  ch  <- read_sf(dsn = dsn, layer = "chorizon", query = qry, as_tibble = FALSE)
  
  
  # iterate over the threshold
  if (nrow(ch) > 0) { 
    
    idx <- c(0, rep(4000, 10) * 1:10)
    ch$idx <- as.character(cut(1:nrow(ch), breaks = idx))
    
    temp <- by(ch, ch$idx, function(x) {
      # chtexturegrp
      qry  <- paste0("SELECT * FROM chtexturegrp WHERE rvindicator = 'Yes' AND chkey IN ('", paste0(x$chkey, collapse = "', '"), "')")
      chtg <- read_sf(dsn = dsn, layer = "chtexturegrp", query = qry, as_tibble = FALSE)
      
      # chtexture
      qry  <- paste0("SELECT * FROM chtexture WHERE chtgkey IN ('", paste0(chtg$chtgkey, collapse = "', '"), "')")
      cht <- read_sf(dsn = dsn, layer = "chtexture", query = qry, as_tibble = FALSE)
      
      # aggregate
      chtg <- aggregate(texture ~ chkey + chtgkey, data = chtg, paste0, collapse = ", ")
      cht  <- aggregate(texcl   ~ chtgkey, data = cht, paste0, collapse = ", ")
      
      # merge
      ch <- merge(x, chtg, by = "chkey",   all.x = TRUE, sort = FALSE)
      ch <- merge(ch, cht,  by = "chtgkey", all.x = TRUE, sort = FALSE)
    
    
      vars <- c("cokey", "chkey", "hzname", "hzdept_r", "hzdepb_r", "texture", "texcl", "sandtotal_l", "sandtotal_r", "sandtotal_h", "silttotal_l", "silttotal_r", "silttotal_h", "claytotal_l", "claytotal_r", "claytotal_h", "om_l", "om_r", "om_h", "dbthirdbar_l", "dbthirdbar_r", "dbthirdbar_h", "ksat_l", "ksat_r", "ksat_h", "awc_l", "awc_r", "awc_h", "lep_r", "sar_r", "ec_r", "cec7_r", "sumbases_r", "ph1to1h2o_l", "ph1to1h2o_r", "ph1to1h2o_h", "caco3_l", "caco3_r", "caco3_h", "kwfact", "kffact")
      ch <- ch[vars]
    
      return(ch)
      })
    ch <- do.call("rbind", temp)
    ch$idx <- NULL
    } else {
      idx <- which(names(ch) == "hzdepb_r")
      ch  <- cbind(ch[1:idx], texture = as.character(NULL), texcl = as.character(NULL), ch[(idx + 1):ncol(ch)])
    }
  
  ch <- uncode(ch,
               db = "SDA",
               droplevels = droplevels,
               stringsAsFactors = stringsAsFactors
  )

  return(ch)
}


fetchGDB <- function(dsn = "D:/geodata/soils/gNATSGO_CONUS.gdb",
                     WHERE = NULL, 
                     childs = TRUE,
                     droplevels = TRUE,
                     stringsAsFactors = TRUE
) {
  
  # target legend table
  le_vars <- c("mlraoffice|areasymbol|areaname|areatypename|areaacres|ssastatus|projectscale|cordate|lkey")
  idx <- grepl(le_vars, WHERE)
  if (idx) {
    le <- get_legend_from_GDB(dsn = dsn, WHERE = WHERE, stats = FALSE)
    
    qry <- paste0("lkey IN ('", paste(le$lkey, collapse = "', '"), "')")
    mu <- get_mapunit_from_GDB(dsn = dsn, WHERE = qry)
    mu <- mu[order(mu$areasymbol), ]
    
    temp <- by(mu, mu$areasymbol, function(x) {
      message("getting components and horizons from areasymbol = '", unique(x$areasymbol), "'")
      qry <- paste0("mukey IN ('", paste0(x$mukey, collapse = "', '"), "')") 
      co  <- suppressMessages(get_component_from_GDB(dsn = dsn, WHERE = qry, childs = childs, droplevels = droplevels, stringsAsFactors = stringsAsFactors))
      
      h   <- .get_chorizon_from_GDB(dsn = dsn, co)
      
      return(list(co =  co, h = h))
      })
  }
  
  co <- do.call("rbind", lapply(temp, function(x) x$co))
  h  <- do.call("rbind", lapply(temp, function(x) x$h))
  
  f <- merge(co["cokey"], h, by = "cokey", all.x = TRUE, sort = FALSE)
  f <- f[order(f$cokey), ]
  depths(f) <- cokey ~ hzdept_r + hzdepb_r
  site(f) <- co
  
  return(f)
}


