
.fetchNASIS_temp <- function(rmHzErrors       = TRUE,
                             nullFragsAreZero = TRUE, 
                             soilColorState   = "moist", 
                             stringsAsFactors = default.stringsAsFactors()
                             ) {
  
  # sanity check
  .checks$soilColorState(soilColorState)
  
  temp = readLines("C:/ProgramData/USDA/NASIS/Temp/fetchNASIS.txt")
  
  be   = data.frame(table = c("site", "phorizon", "phcolor"), 
                    begin = grep("@begin", temp), 
                    end = grep("@end", temp)
                    )
  split(be, be$table) ->.;
  lapply(., function(x) {
    x2 = temp[seq(x$begin + 1, x$end - 1)]
    x2 = read.csv(textConnection(x2), sep = "|", quote = "", stringsAsFactors = stringsAsFactors)
    x2 = x2[, - ncol(x2)]
    x2 = uncode(x2)
    }) ->.;
  names(.) <- c("phcolor", "phorizon", "site")
  
  
  # simplify colors
  .$phcolor <- .color(.$phcolor, soilColorState = soilColorState)
  
  
  # fix problems
  .$phorizon <- .fix_problems(site_data  = .$site,
                              hz_data    = .$phorizon,
                              pedon_id   = "peiid",
                              hzdept     = "hzdept",
                              hzdepb     = "hzdepb",
                              rmHzErrors = rmHzErrors,
                              nullFragsAreZero = nullFragsAreZero,
                              soilColorState = soilColorState
                              )
  
  # upgrade to SoilProfilecollection
  h <- .$phorizon
  depths(h) <- peiid ~ hzdept + hzdepb
  
  
  # left-join via peiid
  # < 0.1 second for ~ 4k pedons
  site(h) <- .$site
  
  # set metadata
  m <- metadata(h)
  m$origin <- "NASIS pedons"
  metadata(h) <- m
  
  # set NASIS-specific horizon identifier
  hzidname(h) <- "phiid"
  
  # done
  return(h)
  
  }

# temp <- .fetchNASISTemp()

.get_site_from_NASISTemp <- function(stringsAsFactors = default.stringsAsFactors()
) {
  
  temp = readLines("C:/ProgramData/USDA/NASIS/Temp/get_site2_from_NASIS.txt")
  
  temp = read.csv(
    textConnection(
      readLines("C:/ProgramData/USDA/NASIS/Temp/get_site2_from_NASIS.txt")
      ), 
    sep = "|", 
    quote = "", 
    stringsAsFactors = stringsAsFactors
    )
  temp = temp[, - ncol(temp)]
  temp = uncode(temp)
  
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
  
  
  # 3 - test for horizonation inconsistencies... flag, and optionally remove
  # ~ 1.3 seconds / ~ 4k pedons
  h.test <- ddply(hz_data, pedon_id, test_hz_logic, 
                  topcol    = hzdept, 
                  bottomcol = hzdepb, 
                  strict    = TRUE
  )
  
  # which are the good (valid) ones?
  good.ids      <- as.character(h.test[[pedon_id]][which(h.test$hz_logic_pass)])
  bad.ids       <- as.character(h.test[[pedon_id]][which(!h.test$hz_logic_pass)])
  bad.horizons  <- hz_data[which(!h.test$hz_logic_pass), c(1:4,6,7)]
  bad.pedon.ids <- site_data[[pedon_id]][which(site_data[[pedon_id]] %in% bad.ids)]
  
  # optionally filter pedons WITH NO horizonation inconsistencies
  if (rmHzErrors)
    hz_data <- hz_data[which(hz_data[[pedon_id]] %in% good.ids), ]
  
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


