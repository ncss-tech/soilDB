
get_component_from_GDB <- function(dsn = "gNATSGO_CONUS.gdb", WHERE = NULL, childs = FALSE, droplevels = TRUE, stringsAsFactors = TRUE) {

  # check
  co_vars <- "comppct_l|comppct_r|comppct_h|compname|compkind|majcompflag|otherph|localphase|slope_l|slope_r|slope_h|slopelenusle_l|slopelenusle_r|slopelenusle_h|runoff|tfact|wei|weg|erocl|earthcovkind1|earthcovkind2|hydricon|hydricrating|drainagecl|elev_l|elev_r|elev_h|aspectccwise|aspectrep|aspectcwise|geomdesc|albedodry_l|albedodry_r|albedodry_h|airtempa_l|airtempa_r|airtempa_h|map_l|map_r|map_h|reannualprecip_l|reannualprecip_r|reannualprecip_h|ffd_l|ffd_r|ffd_h|nirrcapcl|nirrcapscl|nirrcapunit|irrcapcl|irrcapscl|irrcapunit|cropprodindex|constreeshrubgrp|wndbrksuitgrp|rsprod_l|rsprod_r|rsprod_h|foragesuitgrpid|wlgrain|wlgrass|wlherbaceous|wlshrub|wlconiferous|wlhardwood|wlwetplant|wlshallowwat|wlrangeland|wlopenland|wlwoodland|wlwetland|soilslippot|frostact|initsub_l|initsub_r|initsub_h|totalsub_l|totalsub_r|totalsub_h|hydgrp|corcon|corsteel|taxclname|taxorder|taxsuborder|taxgrtgroup|taxsubgrp|taxpartsize|taxpartsizemod|taxceactcl|taxreaction|taxtempcl|taxmoistscl|taxtempregime|soiltaxedition|castorieindex|flecolcomnum|flhe|flphe|flsoilleachpot|flsoirunoffpot|fltemik2use|fltriumph2use|indraingrp|innitrateleachi|misoimgmtgrp|vasoimgtgrp|cokey|mukey"
  co_idx <- grepl(co_vars, WHERE, ignore.case = TRUE)

  if (! co_idx) {
    stop("the WHERE argument is not targeting the component table")
  }


  # query
  message("getting components from ", substr(WHERE, 1, 20), "...")
  qry <- paste(
    "SELECT mukey, cokey, compname, comppct_r, compkind, majcompflag, localphase, drainagecl, hydricrating, erocl, earthcovkind1, earthcovkind2, elev_r, slope_r, aspectrep, map_r, airtempa_r, reannualprecip_r, ffd_r, hydgrp,  nirrcapcl, nirrcapscl, irrcapcl, irrcapscl, tfact, wei, weg, corcon, corsteel, frostact, taxclname, taxorder, taxsuborder, taxgrtgroup, taxsubgrp, taxpartsize, taxpartsizemod, taxceactcl, taxreaction, taxtempcl, taxmoistscl, taxtempregime, soiltaxedition

    FROM component

    WHERE", WHERE
  )
  co <- sf::read_sf(dsn = dsn, layer = "component", query = qry, as_tibble = FALSE)
  
  
  # recode metadata domains
  co <- uncode(co,
               db = "SDA",
               droplevels = droplevels,
               stringsAsFactors = stringsAsFactors
  )
  

  # childs
  if (childs == TRUE) {

    message("getting child tables")

    # exec sub query
    pm    <- .get_copmgrp_from_GDB(dsn = dsn, co)

    idx <- c(0, rep(1500, 10) * 1:10)
    co$idx <- as.character(cut(1:nrow(co), breaks = idx))

    cogmd <- by(co, co$idx, function(x) {
      temp <- .get_cogeomordesc_from_GDB(dsn = dsn, x)
    })
    cogmd <- do.call("rbind", cogmd)

    # prep
    pm    <- .copm_prep(pm, db = "SDA")
    cogmd <- .cogmd_prep(cogmd, db = "SDA")

    # merge
    co$idx <- NULL
    co <- merge(co, pm,    by = "cokey", all.x = TRUE, sort = FALSE)
    co <- merge(co, cogmd, by = "cokey", all.x = TRUE, sort = FALSE)
  }

  
  # done
  return(co)
}



get_legend_from_GDB <- function(dsn = "gNATSGO_CONUS.gdb", WHERE = NULL, droplevels = TRUE, stringsAsFactors = TRUE, stats = FALSE) {
  
  if (!is.null(WHERE)) {
    # check
    le_vars <- "mlraoffice|areasymbol|areaname|areatypename|areaacres|ssastatus|projectscale|cordate|lkey"
    le_idx <- grepl(le_vars, WHERE, ignore.case = TRUE)

    if (! le_idx) {
      stop("the WHERE argument is not targeting the legend table")
    }
    
    # query
    qry <- paste0("SELECT * FROM legend WHERE ", WHERE)
    le <- sf::read_sf(dsn = dsn, layer = "legend", query = qry)
  } else le <- sf::read_sf(dsn = dsn, layer = "legend")


  if (stats == TRUE) {
    mu  <- sf::read_sf(dsn = dsn, layer = "mapunit", query = paste0("SELECT lkey, mukey FROM mapunit WHERE lkey IN ('", paste0(le$lkey, collapse = "', '"), "')"))
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

  # tests
  if (!is.null(WHERE)) {
    
    le_vars <- "mlraoffice|areasymbol|areaname|areatypename|areaacres|ssastatus|projectscale|cordate"
    le_idx <- grepl(le_vars, WHERE, ignore.case = TRUE)
    
    mu_vars <- "lkey|mukey|musym|muname|mukind|mustatus|invesintens|muacres|farmlndcl"
    mu_idx <- grepl(mu_vars, WHERE, ignore.case = TRUE)
    
    if (le_idx & mu_idx) {
      stop("the WHERE argument can not target both the legend and mapunit table at the same time")
    }
    
    if (! le_idx & ! mu_idx) {
      stop("the WHERE argument is not targeting either the legend or mapunit table")
    }
    
    # query
    
    message("getting mapunits from WHERE ", WHERE)
    
    if (mu_idx) {
      qry <- paste0("SELECT * FROM mapunit WHERE ", WHERE)
      mu  <- sf::read_sf(dsn = dsn, layer = "mapunit", query = qry)
      
      qry <- paste0("SELECT * FROM legend WHERE lkey IN ('", paste0(unique(mu$lkey), collapse = "', '"), "')")
      le <- sf::read_sf(dsn = dsn, layer = "legend", query = qry)
    }
    
    if (le_idx) {
      qry <- paste0("SELECT * FROM legend WHERE ", WHERE)
      le <- sf::read_sf(dsn = dsn, layer = "legend", query = qry)
      
      qry <- paste0("SELECT * FROM mapunit WHERE lkey IN ('", paste0(unique(le$lkey), collapse = "', '"), "')")
      mu  <- sf::read_sf(dsn = dsn, layer = "mapunit", query = qry)
    }
  }


  # query
  if (is.null(WHERE)) {
    message("getting mapunits from WHERE areasymbol LIKE '%'")
    mu  <- sf::read_sf(dsn = dsn, layer = "mapunit")
    
    qry <- paste0("SELECT * FROM legend WHERE lkey IN ('", paste0(unique(mu$lkey), collapse = "', '"), "')")
    le <- sf::read_sf(dsn = dsn, layer = "legend", query = qry)
  }
  

  mu <- merge(mu, le, by = "lkey", all.x = TRUE, sort = FALSE)
  mu <- mu[order(mu$areasymbol), ]


  if (stats == TRUE) {
    
    if (nrow(mu) > 3000) {
      idx <- c(0, rep(3000, 10) * 1:10)
      mu$idx <- as.character(cut(1:nrow(mu), breaks = idx))
    } else mu$idx <- mu$areasymbol
    
    co <- by(mu, mu$idx, function(x) {

      qry <- paste(
        "SELECT mukey, cokey, comppct_r, majcompflag, hydricrating

         FROM component

         WHERE mukey IN ('", paste0(x$mukey, collapse = "', '"), "')"
      )
      message("getting components from ", substr(paste0(unique(x$areasymbol), collapse = ", "), 1, 100), "...")
      sf::read_sf(dsn = dsn, layer = "component", query = qry)
    })
    co <- do.call("rbind", co)

    if (nrow(co) > 0) {
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
    } else {
      mu = cbind(mu, pct_component = NA_integer_, pct_hydric = NA_integer_, n_component = NA_integer_, n_majcompflag = NA_integer_)
    }
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
  pmg <- sf::read_sf(dsn = dsn, layer = "copmgrp", query = qry, as_tibble = FALSE)

  qry <- paste0(
    "SELECT copmgrpkey, pmorder, pmkind, pmorigin

     FROM copm

     WHERE copmgrpkey IN ('", paste0(pmg$copmgrpkey, collapse = "', '"), "')"
  )
  pm <- sf::read_sf(dsn = dsn, layer = "copm", query = qry, as_tibble = FALSE)

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
  cogmd    <- sf::read_sf(dsn, layer = "cogeomordesc", query = qry, as_tibble = FALSE)
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
  lf_3d <- sf::read_sf(dsn, layer = "cosurfmorphgc", query = qry, as_tibble = FALSE)
  names(lf_3d)[1:4] <- gsub("geompos", "", names(lf_3d)[1:4])


  # slope shape
  qry <- paste0(
    "SELECT shapeacross, shapedown, cogeomdkey

      FROM cosurfmorphss

      WHERE cogeomdkey IN ('", paste0(cogmd_lf$cogeomdkey, collapse = "', '"), "')"
  )
  lf_ss <- sf::read_sf(dsn, layer = "cosurfmorphgc", query = qry, as_tibble = FALSE)


  # hillslope position
  qry <- paste0(
    "SELECT hillslopeprof, cogeomdkey

      FROM cosurfmorphhpp

      WHERE cogeomdkey IN ('", paste0(cogmd_lf$cogeomdkey, collapse = "', '"), "')"
  )
  lf_2d <- sf::read_sf(dsn, layer = "cosurfmorphhpp", query = qry, as_tibble = FALSE)


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
  idx <- c(0, rep(3000, 10) * 1:10)
  co$idx <- as.character(cut(1:nrow(co), breaks = idx))

  ch <- by(co, co$idx, function(x) {
    qry <- paste0(
      "SELECT cokey, chkey, hzname, hzdept_r, hzdepb_r, sandtotal_l, sandtotal_r, sandtotal_h, silttotal_l, silttotal_r, silttotal_h, claytotal_l, claytotal_r, claytotal_h, om_l, om_r, om_h, dbthirdbar_l, dbthirdbar_r, dbthirdbar_h, ksat_l, ksat_r, ksat_h, awc_l, awc_r, awc_h, lep_r, sar_r, ec_r, cec7_r, sumbases_r, ph1to1h2o_l, ph1to1h2o_r, ph1to1h2o_h, caco3_l, caco3_r, caco3_h, kwfact, kffact

    FROM chorizon

    WHERE cokey IN ('", paste0(x$cokey, collapse = "', '"), "')"
    )
    ch  <- sf::read_sf(dsn = dsn, layer = "chorizon", query = qry, as_tibble = FALSE)
  })
  ch <- do.call("rbind", ch)


  # iterate over the threshold
  if (nrow(ch) > 0) {

    idx <- c(0, rep(3000, 50) * 1:50)
    ch$idx <- as.character(cut(1:nrow(ch), breaks = idx, labels = paste0("i", 1:50)))

    temp <- by(ch, ch$idx, function(x) {

      # chtexturegrp
      qry  <- paste0("SELECT * FROM chtexturegrp WHERE rvindicator = 'Yes' AND chkey IN ('", paste0(x$chkey, collapse = "', '"), "')")
      chtg <- sf::read_sf(dsn = dsn, layer = "chtexturegrp", query = qry, as_tibble = FALSE)

      # chtexture
      qry  <- paste0("SELECT * FROM chtexture WHERE chtgkey IN ('", paste0(chtg$chtgkey, collapse = "', '"), "')")
      cht <- sf::read_sf(dsn = dsn, layer = "chtexture", query = qry, as_tibble = FALSE)

      # aggregate
      if (nrow(chtg) > 0) {
        chtg <- aggregate(texture ~ chkey + chtgkey, data = chtg, paste0, collapse = ", ")
      } else chtg <- chtg[c("chkey", "chtgkey", "texture")]
      if (nrow(cht) > 0) {
        cht  <- aggregate(texcl   ~ chtgkey,         data = cht,  paste0, collapse = ", ")
      } else cht <- cht[c("chtgkey", "texcl")]

      # merge
      ch <- merge(x, chtg, by = "chkey",    all.x = TRUE, sort = FALSE)
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


fetchGDB <- function(dsn = "gNATSGO_CONUS.gdb",
                     WHERE = NULL,
                     childs = TRUE,
                     droplevels = TRUE,
                     stringsAsFactors = TRUE
) {

  # checks
  le_vars <- "mlraoffice|areasymbol|areaname|areatypename|areaacres|ssastatus|projectscale|cordate|lkey"
  mu_vars <- "mukey|musym|muname|mukind|mustatus|invesintens|muacres|farmlndcl"
  co_vars <- "comppct_l|comppct_r|comppct_h|compname|compkind|majcompflag|otherph|localphase|slope_l|slope_r|slope_h|slopelenusle_l|slopelenusle_r|slopelenusle_h|runoff|tfact|wei|weg|erocl|earthcovkind1|earthcovkind2|hydricon|hydricrating|drainagecl|elev_l|elev_r|elev_h|aspectccwise|aspectrep|aspectcwise|geomdesc|albedodry_l|albedodry_r|albedodry_h|airtempa_l|airtempa_r|airtempa_h|map_l|map_r|map_h|reannualprecip_l|reannualprecip_r|reannualprecip_h|ffd_l|ffd_r|ffd_h|nirrcapcl|nirrcapscl|nirrcapunit|irrcapcl|irrcapscl|irrcapunit|cropprodindex|constreeshrubgrp|wndbrksuitgrp|rsprod_l|rsprod_r|rsprod_h|foragesuitgrpid|wlgrain|wlgrass|wlherbaceous|wlshrub|wlconiferous|wlhardwood|wlwetplant|wlshallowwat|wlrangeland|wlopenland|wlwoodland|wlwetland|soilslippot|frostact|initsub_l|initsub_r|initsub_h|totalsub_l|totalsub_r|totalsub_h|hydgrp|corcon|corsteel|taxclname|taxorder|taxsuborder|taxgrtgroup|taxsubgrp|taxpartsize|taxpartsizemod|taxceactcl|taxreaction|taxtempcl|taxmoistscl|taxtempregime|soiltaxedition|castorieindex|flecolcomnum|flhe|flphe|flsoilleachpot|flsoirunoffpot|fltemik2use|fltriumph2use|indraingrp|innitrateleachi|misoimgmtgrp|vasoimgtgrp|cokey"

  le_idx <- grepl(le_vars, WHERE, ignore.case = TRUE)
  mu_idx <- grepl(mu_vars, WHERE, ignore.case = TRUE)
  co_idx <- grepl(co_vars, WHERE, ignore.case = TRUE)

  if (le_idx & mu_idx & co_idx | le_idx & mu_idx | le_idx & co_idx | mu_idx & co_idx) {
    stop("WHERE can only target 1 table at a time")
  }


  # target legend table
  if (le_idx) {
    le <- get_legend_from_GDB(dsn = dsn, WHERE = WHERE, stats = FALSE)

    qry <- paste0("lkey IN ('", paste(le$lkey, collapse = "', '"), "')")
    mu <- suppressMessages(get_mapunit_from_GDB(dsn = dsn, WHERE = qry))
    mu <- mu[order(mu$areasymbol), ]
  }

  # target mapunit table
  if (mu_idx) {
    mu <- suppressMessages(get_mapunit_from_GDB(dsn = dsn, WHERE = WHERE))
    mu <- mu[order(mu$areasymbol), ]
  }


  # get components
  if (le_idx | mu_idx) {

    if (le_idx) mu$idx <- mu$areasymbol
    if (mu_idx) mu$idx <- "1"

    temp <- by(mu, mu$idx, function(x) {

      if (le_idx) {
        message("getting components and horizons from areasymbol = '", unique(x$idx), "'")
      } else message("getting components and horizons from ", WHERE)

      # components
      idx <- c(0, rep(375, 15) * 1:15)
      x$idx <- as.character(cut(1:nrow(x), breaks = idx))
      co <- by(x, x$idx, function(x2) {
        qry <- paste0("mukey IN ('", paste0(x2$mukey, collapse = "', '"), "')")
        tryCatch({
          co  <- suppressMessages(get_component_from_GDB(
            dsn        = dsn,
            WHERE      = qry,
            childs     = childs,
            droplevels = droplevels,
            stringsAsFactors = stringsAsFactors
          ))
        },
        error = function(err) {
          print(paste("Error occured: ", err))
          return(NULL)
        }
        )
      })
      co <- do.call("rbind", co)
      co$idx <- NULL

      # horizons
      tryCatch({
        h   <- .get_chorizon_from_GDB(dsn = dsn, co)
      },
      error = function(err) {
        print(paste("Error occured: ", err))
        return(NULL)
      }
      )

      return(list(co = co, h = h))
    })

    co <- do.call("rbind", lapply(temp, function(x) x$co))
    h  <- do.call("rbind", lapply(temp, function(x) x$h))

  } else {

    message("getting components and horizons from ", WHERE)

    # target component table
    co <- suppressMessages(get_component_from_GDB(
      dsn        = dsn,
      WHERE      = WHERE,
      childs     = childs,
      droplevels = droplevels,
      stringsAsFactors = stringsAsFactors
      ))

    # horizons
    h <- .get_chorizon_from_GDB(dsn = dsn, co)

  }


  f <- merge(co["cokey"], h, by = "cokey", all.x = TRUE, sort = FALSE)
  f <- f[order(f$cokey), ]
  depths(f) <- cokey ~ hzdept_r + hzdepb_r
  site(f) <- co

  return(f)
}


