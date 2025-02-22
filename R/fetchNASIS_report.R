
.fetchNASIS_report <- function(url = NULL,
                               rmHzErrors       = FALSE,
                               nullFragsAreZero = TRUE,
                               soilColorState   = "moist",
                               stringsAsFactors = NULL
) {
  
  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }
  
  if (!requireNamespace("aqp")) {
    stop("package 'aqp' is required", call. = FALSE)
  }
  
  tf <- "C:/ProgramData/USDA/NASIS/Temp/fetchNASIS.txt"
  if (!is.null(url))
    tf <- url
  
  # sanity check
  .checks$soilColorState(soilColorState)
  
  # check if temp file exists
  if (!file.exists(tf) & is.null(url)) {
    stop("the temp file ", tf, "\n doesn't exist, please run the fetchNASIS report from NASIS")
  }
  
  temp <- readLines(tf)
  
  be <- data.frame(
    table = c("site", "pediagfeatures", "phorizon", "phcolor"),
    begin = grep("@begin", temp),
    end = grep("@end", temp)
  )
  
  # check to see if there is any data
  be$diff <- be$end - be$begin
  
  if(all(be$diff == 1))
    stop("empty result set -- check parameters used to run `fetchNASIS` export report.", call.=FALSE)
  
  split(be, be$table) ->.;
  lapply(., function(x) {
    if (x$diff > 1) {
      x2 <- temp[seq(x$begin + 1, x$end - 1)]
      # remove "\" from lines and blank lines e.g. ||
      x2 <- gsub('"|\\|*$', "", x2)
      x2 <- read.csv(textConnection(x2), sep = "|", quote = "", stringsAsFactors = FALSE)
      # aggregate NASIS returns empty rows
      # NASIS text reports return empty columns
      # remove
      # x2 <- x2[, -ncol(x2)]
      idx <- names(x2) %in% c("pmkind", "pmorigin")
      x2[!idx] <- soilDB::uncode(x2[!idx])
      idx <- sapply(x2, is.character)
      x2[idx] <- lapply(x2[idx], function(x) ifelse(x == "", NA, x))
      return(x2)
    } else return(NULL)
  }) -> .;
  names(.) <- c("pediagfeatures", "phcolor", "phorizon", "site")
  
  
  # simplify colors
  if (!is.null(.$phcolor)) {
    .$phcolor <- .color(.$phcolor, soilColorState = soilColorState)
  }
  
  # fix problems
  .$site = .fix_site_problems(.$site, nullFragsAreZero = nullFragsAreZero)
  
  
  if (!is.null(.$phorizon)) {
    .$phorizon <- .fix_problems(hz_data    = .$phorizon,
                                site_data  = .$site,
                                pedon_id   = "peiid",
                                hzdept     = "hzdept",
                                hzdepb     = "hzdepb",
                                rmHzErrors = rmHzErrors,
                                nullFragsAreZero = nullFragsAreZero,
                                soilColorState = soilColorState
    )
    
    
    # upgrade to SoilProfilecollection
    h <- .$phorizon
    aqp::depths(h) <- peiid ~ hzdept + hzdepb
    
    # set optional hz designation and texture slots
    aqp::hzdesgnname(h) <- "hzname"
    aqp::hztexclname(h) <- "texture"
    
    aqp::site(h) <- .$site
    
    if (!is.null(.$phcolor)) {
      aqp::horizons(h) <- .$phcolor
    }
    
  } else {
    h <- aqp::SoilProfileCollection(
      idcol = "peiid",
      site = .$site,
      horizons = data.frame(
        peiid = .$site$peiid,
        hzdept = NA_integer_,
        hzdepb = NA_integer_,
        hzID = seq(nrow(.$site))
      ),
      depthcols = c("hzdept", "hzdepb"), 
      hzidcol = "hzID"
    )
  }
  
  # tidy .$pediagfeatures
  pediagfeatures <- .$pediagfeatures
  pediagfeatures[-1] <- lapply(pediagfeatures[-1], function(x) {
    ifelse(!is.na(x), TRUE, FALSE)
  })
  
  # pediagfeatures[!is.na(.$pediagfeatures)] <- TRUE
  aqp::site(h) <- pediagfeatures
  
  if (!is.null(.$pediagfeatures)) {
    vars <- names(.$pediagfeatures)[-1]
    pediagfeatures <- stats::reshape(.$pediagfeatures,
                                     direction = "long",
                                     timevar = "featkind", times = vars,
                                     v.names = "featdep", varying = vars
    )
    pediagfeatures <- pediagfeatures[! is.na(pediagfeatures$featdep), ]
    featdep <- {
      x <- strsplit(pediagfeatures$featdep, "-")
      featdept <- unlist(lapply(x, function(x) as.integer(x[1])))
      suppressWarnings(featdepb <- unlist(lapply(x, function(x) as.integer(x[2]))))
      data.frame(featdept, featdepb)
    }
    pediagfeatures <- cbind(pediagfeatures[c("peiid", "featkind")], featdep)
    pediagfeatures$featkind <- gsub("\\.", " ", pediagfeatures$featkind)
    
    aqp::diagnostic_hz(h) <- pediagfeatures
  }
  
  # set metadata
  m <- aqp::metadata(h)
  m$origin <- "NASIS pedons (export)"
  m$created <- Sys.time()
  aqp::metadata(h) <- m
  
  # set NASIS-specific horizon identifier
  tryCatch(aqp::hzidname(h) <- 'phiid', error = function(e) {
    if(grepl(e$message, pattern="not unique$")) {
      if(!rmHzErrors) {
        # if rmHzErrors = FALSE, keep unique integer assigned ID to all records automatically
        message("-> QC: duplicate horizons are present with rmHzErrors=FALSE! defaulting to `hzID` as unique horizon ID.")
      } else {
        stop(e)
      }
    }
  })
  
  
  # done
  return(h)
  
}

# temp <- .fetchNASISTemp()

.get_site_from_NASISReport <- function(url = NULL, nullFragsAreZero = TRUE, stringsAsFactors = NULL
) {
  
  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }
  
  tf = "C:/ProgramData/USDA/NASIS/Temp/get_site_from_NASIS.txt"
  if (!is.null(url)) tf = url
  
  # check if temp file exists
  if (!file.exists(tf) & is.null(url)) {
    stop("the temp file ", tf, "\n doesn't exist, please run the fetchNASIS report from NASIS")
  }
  
  
  # check to see if data is coming from fetchNASIS or get_site
  temp <- readLines(tf)
  begin = grep("@begin get_site_from_NASIS", temp)
  
  if (length(begin) > 0) {
    
    end = grep("@end get_site_from_NASIS", temp)
    # check to see if there is any data
    diff.idx <- end - begin
    
    if(all(diff.idx == 1))
      stop("empty result set -- check parameters used to run `fetchNASIS` export report.", call.=FALSE)
    
    x2 <- temp[seq(begin + 1, end - 1)]
    temp <- read.csv(textConnection(x2), sep = "|", quote = "", stringsAsFactors = FALSE)
  } else   temp <- read.csv(textConnection(temp), sep = "|", quote = "", stringsAsFactors = FALSE)
  
  # aggregate NASIS returns empty rows
  # NASIS text reports return empty columns
  # remove
  temp = temp[!is.na(temp$siteiid), - ncol(temp)]
  idx  = names(temp) %in% c("pmkind", "pmorigin")
  temp[!idx] = uncode(temp[!idx])
  idx  = sapply(temp, is.character)
  temp[idx] = lapply(temp[idx], function(x) ifelse(x == "", NA, x))
  # temp = within(temp, {
  #   obsdate   = as.Date(as.character(obsdate))
  #   classdate = as.Date(as.character(classdate))
  # })
  
  temp = .fix_site_problems(temp, nullFragsAreZero = nullFragsAreZero)
  
  # impute missing x_std & y_std if utm are present
  # idx <- with(temp, ! complete.cases(x_std, y_std) & complete.cases(utmzone, utmeasting, utmnorthing, horizdatnm))
  return(temp)
  
}



.get_pediagfeatures_from_NASISTemp <- function(stringsAsFactors = NULL) {
  
  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }
  
  tf <- "C:/ProgramData/USDA/NASIS/Temp/get_pediagfeatures_from_NASIS.txt"
  
  # check if temp file exists
  if (!file.exists(tf)) {
    stop("the temp file ", tf, "\n doesn't exist, please run the get_pediagfeatures_from_NASIS report from NASIS")
  }
  
  temp = read.csv(
    textConnection(
      readLines(tf)
    ),
    sep = "|",
    quote = ""
  )
  # aggregate NASIS returns empty rows
  # NASIS text reports return empty columns
  # remove
  temp = temp[!is.na(temp$peiid), - ncol(temp)]
  temp = uncode(temp)
  # temp = within(temp, {
  #   obsdate   = as.Date(as.character(obsdate))
  #   classdate = as.Date(as.character(classdate))
  # })
  
}


## move these to utils.R after more testing
## fix some common problems

.checks <- list(
  
  soilColorState = function(soilColorState) {
    if(! soilColorState %in% c('dry', 'moist'))
      stop('soilColorState must be either `dry` or `moist`',
           call. = FALSE
      )
  }
)


# color
.color <- function(df, soilColorState = "moist") {
  
  # convert back to characters / numeric
  df$colormoistst = as.character(df$colormoistst)
  df$colorhue     = as.character(df$colorhue)
  
  # careful!
  # uncode creates factors, so we have to convert to character first
  df$colorvalue  = as.numeric(as.character(df$colorvalue))
  df$colorchroma = as.numeric(as.character(df$colorchroma))
  
  # sanity check, only attempt to simplify colors if there are > 1 rows
  if (nrow(df) > 1) {
    # mix colors as-needed, mixing done in CIE LAB space
    df <- simplifyColorData(df)
  } else {
    # TODO: this could lead to problems due to assumed column presence
    # do nothing
    df
  }
  
  ## copy pre-computed colors into a convenience field for plotting
  # moist colors
  if (soilColorState == "moist")
    df$soil_color <- df$moist_soil_color
  # dry colors
  if (soilColorState == "dry")
    df$soil_color <- df$dry_soil_color
  
  return(df)
}



.fix_problems <- function(hz_data,
                          site_data,
                          pedon_id,
                          hzdept,
                          hzdepb,
                          rmHzErrors = FALSE,
                          nullFragsAreZero = TRUE,
                          soilColorState = "moist"
) {
  
  # 1 - replace missing lower boundaries
  missing.lower.depth.idx <- which(!is.na(hz_data[[hzdept]]) & is.na(hz_data[[hzdepb]]))
  
  # keep track of affected pedon IDs (if none, this will have zero length)
  assign("missing.bottom.depths",
         value = unique(hz_data[[pedon_id]][missing.lower.depth.idx]),
         envir = get_soilDB_env()
  )
  
  if (length(missing.lower.depth.idx) > 0) {
    message(paste('replacing missing lower horizon depths with top depth + 1cm ... [', length(missing.lower.depth.idx), ' horizons]', sep=''))
    
    # make edit
    hz_data[[hzdepb]][missing.lower.depth.idx] <- hz_data[[hzdept]][missing.lower.depth.idx] + 1
  }
  
  
  # 2 - top == bottom ? bottom <- bottom + 1
  top.eq.bottom.idx <- which(hz_data[[hzdept]] == hz_data[[hzdepb]])
  
  # keep track of affected pedon IDs (if none, this will have zero length)
  assign('top.bottom.equal',
         value = unique(hz_data[[pedon_id]][top.eq.bottom.idx]),
         envir = get_soilDB_env()
  )
  # make the edit
  if (length(top.eq.bottom.idx) > 0) {
    message(paste('top/bottom depths equal, adding 1cm to bottom depth ... [', length(top.eq.bottom.idx), ' horizons]', sep = '')
    )
    hz_data[[hzdepb]][top.eq.bottom.idx] <- hz_data[[hzdepb]][top.eq.bottom.idx] + 1
  }
  
  h.test <- aqp::checkHzDepthLogic(hz_data, c('hzdept', 'hzdepb'), idname = pedon_id, fast = TRUE)
  
  # which are the good (valid) ones?
  good.ids      <- as.character(h.test[[pedon_id]][which(h.test$valid)])
  bad.ids       <- as.character(h.test[[pedon_id]][which(!h.test$valid)])
  bad.horizons  <- hz_data[which(!h.test$valid), c(1:4,6,7)]
  bad.pedon.ids <- site_data[[pedon_id]][which(site_data[[pedon_id]] %in% bad.ids)]
  
  # optionally filter pedons WITH NO horizonation inconsistencies
  if (rmHzErrors) {
    hz_data <- hz_data[which(hz_data[[pedon_id]] %in% good.ids), ]
  }
  
  # keep track of those pedons with horizonation errors
  assign('bad.pedon.ids',
         value = bad.pedon.ids,
         envir = get_soilDB_env()
  )
  assign("bad.horizons",
         value = data.frame(bad.horizons),
         envir = get_soilDB_env()
  )
  
  #4 - optionally convert NA fragvol to 0
  if (nullFragsAreZero) {
    
    idx <- grep("fragvol|frags_|gravel|cobbles|stones|boulders|channers|unspecified", names(hz_data))
    vars <- names(hz_data)[idx]
    hz_data[idx] <-lapply(hz_data[idx], function(x) {
      ifelse(is.na(x), 0, x)
    })
  }
  
  return(hz_data)
  
}



.fix_site_problems <- function(site_data, nullFragsAreZero = nullFragsAreZero) {
  
  if (nullFragsAreZero == TRUE) {
    idx <- grep("fragvol|frags_|gravel|cobbles|stones|boulders|channers|unspecified", names(site_data))
    vars <- names(site_data)[idx]
    site_data[idx] <-lapply(site_data[idx], function(x) {
      ifelse(is.na(x), 0, x)
    })
  } else site_data
  
  return(site_data)
}


.get_copedon_from_NASISReport <- function(nasissitename = NULL, grpname = NULL, areasymbol = NULL) {
  
  # fp <- "C:/ProgramData/USDA/NASIS/Temp/get_copedon_from_NASIS.html"
  # 
  # # test
  # tf_idx <- file.exists(tf)
  p_idx  <- sapply(list(nasissitename, grpname, areasymbol), is.null)
  
  # if (tf_idx & any(! p_idx)) {
  #   stop("the temp file ", tf, "\n doesn't exist and some of the argument parameters are NULL, please run the get_copedon_from_NASIS report in NASIS or enter the necessary argument parameters")
  # }
  if (any(p_idx)) {
    stop("some of the argument parameters are NULL")
  }
  
  
  # # local report
  # if (tf_idx & all(p_idx)) {
  #   df <- as.data.frame(rvest::html_table(xml2::read_html(tf), header = TRUE))
  # } 
  
  # web report
  if (!all(p_idx)) {
    url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_copedon_from_NASISWebReport"
    args <- list(p_nasissitename = nasissitename, p_grpname = grpname, p_areasymbol = areasymbol)
    
    df <- parseWebReport(url, args)
  }
  
  return(df)
}
