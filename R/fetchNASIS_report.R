
.fetchNASIS_report <- function(url = NULL,
                               rmHzErrors       = TRUE,
                               nullFragsAreZero = TRUE,
                               soilColorState   = "moist",
                               stringsAsFactors = NULL
                               ) {

  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
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

  be <- data.frame(table = c("site", "pediagfeatures", "phorizon", "phcolor"),
                   begin = grep("@begin", temp),
                   end = grep("@end", temp))

  # check to see if there is any data
  diff.idx <- be$end - be$begin

  if(all(diff.idx == 1))
    stop("empty result set -- check parameters used to run `fetchNASIS` export report.", call.=FALSE)

  split(be, be$table) ->.;
  lapply(., function(x) {
      x2 <- temp[seq(x$begin + 1, x$end - 1)]
      x2 <- read.csv(textConnection(x2), sep = "|", quote = "", stringsAsFactors = FALSE)
      # aggregate NASIS returns empty rows
      # NASIS text reports return empty columns
      # remove
      x2 <- x2[!is.na(x2$peiid), - ncol(x2)]
      idx <- names(x2) %in% c("pmkind", "pmorigin")
      x2[!idx] <- uncode(x2[!idx], db = "LIMS")
      idx <- sapply(x2, is.character)
      x2[idx] <- lapply(x2[idx], function(x) ifelse(x == "", NA, x))
      return(x2)
  }) ->.;
  names(.) <- c("pediagfeatures", "phcolor", "phorizon", "site")


  # simplify colors
  .$phcolor <- .color(.$phcolor, soilColorState = soilColorState)


  # fix problems
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
  h <- merge(.$phorizon, .$phcolor, by = "phiid", all.x = TRUE, sort = FALSE)
  depths(h) <- peiid ~ hzdept + hzdepb


  # left-join via peiid
  # < 0.1 second for ~ 4k pedons
  .$site = .fix_site_problems(.$site, nullFragsAreZero = nullFragsAreZero)
  site(h) <- .$site


  # tidy .$pediagfeatures
  pediagfeatures <- .$pediagfeatures
  pediagfeatures[-1] <- lapply(pediagfeatures[-1], function(x) {
    ifelse(!is.na(x), TRUE, FALSE)
  })
  # pediagfeatures[!is.na(.$pediagfeatures)] <- TRUE
  site(h) <- pediagfeatures

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

  diagnostic_hz(h) <- pediagfeatures


  # set metadata
  m <- metadata(h)
  m$origin <- "NASIS pedons (export)"
  metadata(h) <- m

  # set NASIS-specific horizon identifier
  tryCatch(hzidname(h) <- 'phiid', error = function(e) {
    if(grepl(e$message, pattern="not unique$")) {
      if(!rmHzErrors) {
        # if rmHzErrors = FALSE, keep unique integer assigned ID to all records automatically
        message("-> QC: duplicate horizons are present with rmHzErrors=FALSE! defaulting to `hzID` as unique horizon ID.")
      } else {
        stop(e)
      }
    }
  })

  # set optional hz designation and texture slots
  hzdesgnname(h) <- "hzname"
  hztexclname(h) <- "texture"

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


  temp = read.csv(
    textConnection(
      readLines(tf)
      ),
    sep = "|",
    quote = "",
    stringsAsFactors = FALSE
    )
  # aggregate NASIS returns empty rows
  # NASIS text reports return empty columns
  # remove
  temp = temp[!is.na(temp$peiid), - ncol(temp)]
  idx  = names(temp) %in% c("pmkind", "pmorigin")
  temp[!idx] = uncode(temp[!idx], db = "LIMS")
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
  temp = uncode(temp, db = "LIMS")
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
                          rmHzErrors = TRUE,
                          nullFragsAreZero = TRUE,
                          soilColorState = "moist"
) {

  # 1 - replace missing lower boundaries
  missing.lower.depth.idx <- which(!is.na(hz_data[[hzdept]]) & is.na(hz_data[[hzdepb]]))

  # keep track of affected pedon IDs (if none, this will have zero length)
  assign("missing.bottom.depths",
         value = unique(hz_data[[pedon_id]][missing.lower.depth.idx]),
         envir = soilDB.env
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
         envir = soilDB.env
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
         envir = soilDB.env
  )
  assign("bad.horizons",
         value = data.frame(bad.horizons),
         envir = soilDB.env
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
