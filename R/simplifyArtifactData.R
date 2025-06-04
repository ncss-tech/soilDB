# code for dealing with human artifacts; see simplifyFragmentData.R

.artifactSieve <- function(x, vol.var = "huartvol", ...) {
  x$huartco <- tolower(x$huartco)
  x$huartshp <- tolower(x$huartshp)

  ## assumptions
  # missing huartco = cohesive
  x$huartco[which(is.na(x$huartco))] <- 'cohesive'
  
  # missing huartshp = irregular
  x$huartshp[which(is.na(x$huartshp))] <- 'irregular'

  ## the RV size is likely the safest estimate
  x$huartsize_r <- ifelse(
    is.na(x$huartsize_r),
    (x$huartsize_l + x$huartsize_h) / 2,
    x$huartsize_r
  )

  ## split flat/nonflat
  idx <- grep('^flat', x$huartshp, ignore.case = TRUE, invert = TRUE)
  arts <- x[idx, ]

  idx <- grep('^flat', x$huartshp, ignore.case = TRUE)
  farts <- x[idx, ]

  ## sieve using RV sizes
  # non-flat fragments
  arts$class <- .sieve(arts$huartsize_r, new.names = c('art_fgr', 'art_gr', 'art_cb', 'art_st', 'art_by'), ...)

  # flat artifacts
  farts$class <- .sieve(farts$huartsize_r, flat = TRUE, new.names = c('art_ch','art_fl', 'art_st', 'art_by'), ...)

  res <- rbind(arts, farts)
  idx <- which(is.na(res$class) & !is.na(res[[vol.var]]))
  if (length(idx) > 0) {
    res$class[idx] <- 'art_unspecified'
  }

  # done
  return(res)
}

#' @param art a \code{data.frame} object, typically returned from NASIS, see details
#' @rdname simplifyFragmentData
#' @export simplifyArtifactData
simplifyArtifactData <- function(art, id.var, vol.var = "huartvol", nullFragsAreZero = nullFragsAreZero, ...) {

  # artifact size classes, using fragment breaks, are used in this function
  art.classes <- c('art_fgr', 'art_gr', 'art_cb', 'art_st', 'art_by', 'art_ch', 'art_fl', 'art_unspecified')
  result.columns <- c(id.var, art.classes, "total_art_pct", "huartvol_cohesive", "huartvol_penetrable", "huartvol_innocuous", "huartvol_persistent")
  
  # warn the user and remove the NA records
  # if all huartvol are NA then result is a data frame with all ID values NA
  if (nrow(art[which(!is.na(art[[vol.var]])),]) == 0) {
    message('NOTE: all records are missing artifact volume')
    dat <- as.data.frame(t(rep(NA, length(result.columns))))[seq_len(length(art[[id.var]])),]
    dat[[which(result.columns == id.var)]] <- art[[id.var]]
    colnames(dat) <- result.columns
    rownames(dat) <- NULL
    return(dat)
  } else if (anyNA(art[[vol.var]])) {
    art <- art[which(!is.na(art[[vol.var]])), ]
    # message('NOTE: some records are missing artifact volume')
  }

  # extract classes
  # note: these will put any fragments without fragsize into an 'unspecified' class
  artifact.classes <- .artifactSieve(art, vol.var = vol.var, ...)

  # sum volume by id and class
  art.sums <- aggregate(artifact.classes[[vol.var]],
                        by = list(artifact.classes[[id.var]], artifact.classes[['class']]),
                        FUN = sum, na.rm = TRUE)
  names(art.sums) <- c(id.var, 'class', 'volume')

  # set levels of classes
  art.sums$class <- factor(art.sums$class, levels = art.classes)
  
  # convert to wide format
  fm <- as.formula(paste0(id.var, ' ~ class'))
  art.wide <- as.data.frame(data.table::dcast(data.table::data.table(art.sums), fm, value.var = 'volume', drop = FALSE))

  # must determine the index to the ID column in the wide format
  id.col.idx <- which(names(art.wide) == id.var)

  ## optionally convert NULL frags -> 0
  if (nullFragsAreZero & ncol(art.wide) > 1) {
    art.wide <- as.data.frame(
      cbind(art.wide[, id.col.idx, drop = FALSE], 
            lapply(art.wide[, -id.col.idx], function(i) ifelse(is.na(i), 0, i))
      ), stringsAsFactors = FALSE)
  }
  
  # are there any fractions or the total >= 100%
  gt.100 <- lapply(art.wide[, -id.col.idx, drop = FALSE], FUN = function(i) i >= 100)
  gt.100.matches <- sapply(gt.100, any, na.rm = TRUE)
  if (any(gt.100.matches)) {
    # search within each fraction
    class.idx <- which(gt.100.matches)
    idx <- unique(unlist(lapply(gt.100[class.idx], which)))
    flagged.ids <- art.wide[[id.var]][idx]
    
    gt100nm <- paste0("artifact.volume.gt100.", id.var)
    assign(gt100nm, value = flagged.ids, envir = get_soilDB_env())
    
    message(sprintf("NOTE: some %s have artifact volume >= 100%%", id.var))
  }

  # compute total artifacts
  if (ncol(art.wide) > 1) {
    # calculate another column for total RF, ignoring parafractions
    # index of columns to ignore, para*
    #idx.pf <- grep(names(art.wide), pattern="para")
    # also remove ID column
    idx <- c(id.col.idx)#, idx.pf)
    # this could result in an error if all fragments are para*
    art.wide$total_art_pct <- rowSums(art.wide[, -idx], na.rm = TRUE)
  }
  
  # corrections:
  # 1. fine gravel is a subset of gravel, therefore: gravel = gravel + fine_gravel
  art.wide$art_gr <- rowSums(cbind(art.wide$art_gr, art.wide$art_fgr), na.rm = TRUE)

  # now, do some summaries of cohesion, shape, roundess, penetrability, safety and persistence

  art.wide$huartvol_cohesive <- as.numeric(lapply(split(art, art[[id.var]]), function(art.sub) {
    sum(art.sub[[vol.var]][art.sub$huartco == "cohesive"], na.rm = TRUE)
  }))
  
  art.wide$huartvol_penetrable <- as.numeric(lapply(split(art, art[[id.var]]), function(art.sub) {
    sum(art.sub[[vol.var]][art.sub$huartpen == "penetrable"], na.rm = TRUE)
  }))
  
  art.wide$huartvol_noxious <- as.numeric(lapply(split(art, art[[id.var]]), function(art.sub) {
    sum(art.sub[[vol.var]][art.sub$huartsafety == "noxious artifacts"], na.rm = TRUE)
  }))
  
  art.wide$huartvol_persistent <- as.numeric(lapply(split(art, art[[id.var]]), function(art.sub) {
    sum(art.sub[[vol.var]][art.sub$huartper == "persistent"], na.rm = TRUE)
  }))

  # TODO: somehow summarize shape and roundness? we don't do that for regular frags

  # done
  return(art.wide)
}

