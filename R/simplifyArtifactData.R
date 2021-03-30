## TODO: convert commentary from source material (fragments) and add manual page, thanks

# code for dealing with human artifacts

.artifactSieve <- function(x) {
  # convert to lower case: NASIS metadata usese upper for labels, lower for values
  x$huartco <- tolower(x$huartco)
  x$huartshp <- tolower(x$huartshp)

  ## assumptions
  # missing hardness = rock fragment
  x$huartco[which(is.na(x$huartco))] <- 'cohesive'
  # missing shape = Nonflat
  x$huartshp[which(is.na(x$huartshp))] <- 'irregular'

  ## the RV size is likely the safest estimate,
  ## given the various upper bounds for GR (74mm, 75mm, 76mm)
  # calculate if missing
  x$huartsize_r <- ifelse(
    is.na(x$huartsize_r),
    (x$huartsize_l + x$huartsize_h) / 2,
    x$huartsize_r
  )


  ## split flat/nonflat
  idx <- grep('^flat', x$huartshp, ignore.case = TRUE, invert=TRUE)
  arts <- x[idx, ]

  idx <- grep('^flat', x$huartshp, ignore.case = TRUE)
  farts <- x[idx, ]

  ## sieve using RV sizes
  # non-flat fragments
  arts$class <- .sieve(arts$huartsize_r, new.names = c('art_fgr', 'art_gr', 'art_cb',
                                        'art_st', 'art_by'))

  # flat artifacts
  farts$class <- .sieve(farts$huartsize_r, flat = TRUE, new.names = c('art_ch','art_fl', 'art_st', 'art_by'))

  # combine pieces, note may contain  RF classes == NA
  res <- rbind(arts, farts)

  # what does an NA fragment class mean?
  #
  # typically, fragment size missing
  # or, worst-case, .sieve() rules are missing criteria
  #
  # keep track of these for QC in an 'unspecified' column
  # but only when there is a fragment volume specified
  idx <- which(is.na(res$class) & !is.na(res$huartvol))
  if( length(idx) > 0 ) {
    res$class[idx] <- 'art_unspecified'
  }

  # done
  return(res)
}

simplifyArtifactData <- function(art, id.var, nullFragsAreZero = nullFragsAreZero) {

  # nasty hack to trick R CMD check
  huartvol <- NULL

  # artifact size classes, using fragment breaks, are used in this function
  # note that we are adding a catch-all for those strange phfrags records missing fragment size
  art.classes <- c('art_fgr', 'art_gr', 'art_cb', 'art_st', 'art_by', 'art_ch', 'art_fl', 'art_unspecified')

  result.columns <- c(id.var, art.classes, "total_art_pct",  "huartvol_cohesive","huartvol_penetrable", "huartvol_innocuous", "huartvol_persistent")

  # first of all, we can't do anything if the fragment volume is NA
  # warn the user and remove the offending records
  if(any(is.na(art$huartvol))) {
    art <- art[which(!is.na(art$huartvol)), ]
    warning('some records are missing artifact volume, these have been removed', call. = FALSE)
  }

  # if all fragvol are NA then rf is an empty data.frame and we are done
  if(nrow(art[which(!is.na(art$huartvol)), ]) == 0) {
    warning('all records are missing artifact volume (NULL). buffering result with NA. will be converted to zero if nullFragsAreZero = TRUE.', call. = FALSE)
    dat <- as.data.frame(t(rep(NA, length(result.columns))))
    for(i in 1:length(art[[id.var]])) {
      dat[i,] <- dat[1,]
      dat[i,which(result.columns == id.var)] <- art[[id.var]][i]
    }
    colnames(dat) <- result.columns
    return(dat)
  }

  # extract classes
  # note: these will put any fragments without fragsize into an 'unspecified' class
  artifact.classes <- .artifactSieve(art)

  # sum volume by id and class
  # class cannot contain NA
  art.sums <- aggregate(artifact.classes$huartvol, by=list(artifact.classes[[id.var]], artifact.classes[['class']]), FUN=sum, na.rm=TRUE)
  # fix defualt names from aggregate()
  names(art.sums) <- c(id.var, 'class', 'volume')


  ## NOTE: we set factor levels here because the reshaping (long->wide) needs to account for all possible classes
  ## NOTE: this must include all classes that related functions return
  # set levels of classes
  art.sums$class <- factor(art.sums$class, levels=art.classes)

  # convert to wide format
  fm <- as.formula(paste0(id.var, ' ~ class'))
  art.wide <- reshape2::dcast(art.sums, fm, value.var = 'volume', drop = FALSE)

  # must determine the index to the ID column in the wide format
  id.col.idx <- which(names(art.wide) == id.var)

  ## optionally convert NULL frags -> 0
  if(nullFragsAreZero & ncol(art.wide) > 1) {
    art.wide <- as.data.frame(
      cbind(art.wide[, id.col.idx, drop=FALSE],
            lapply(art.wide[, -id.col.idx], function(i) ifelse(is.na(i), 0, i))
      ), stringsAsFactors=FALSE)
  }

  # final sanity check: are there any fractions or the total >= 100%
  # note: sapply() was previously used here
  #       1 row in rf.wide --> result is a vector
  #       >1 row in rf.wide --> result is a matrix
  # solution: keep as a list
  gt.100 <- lapply(art.wide[, -id.col.idx, drop=FALSE], FUN=function(i) i >= 100)

  # check each size fraction and report id.var if there are any
  gt.100.matches <- sapply(gt.100, any, na.rm=TRUE)
  if(any(gt.100.matches)) {
    # search within each fraction
    class.idx <- which(gt.100.matches)
    idx <- unique(unlist(lapply(gt.100[class.idx], which)))
    flagged.ids <- art.wide[[id.var]][idx]

    warning(sprintf("artifact volume >= 100%%\n%s:\n%s", id.var, paste(flagged.ids, collapse = "\n")), call. = FALSE)
  }

  ## TODO: 0 is returned when all NA and nullFragsAreZero=FALSE
  ## https://github.com/ncss-tech/soilDB/issues/57
  # compute total fragments
  # trap no frag condition
  # includes unspecified class
  if(ncol(art.wide) > 1) {
    # calculate another column for total RF, ignoring parafractions
    # index of columns to ignore, para*
    #idx.pf <- grep(names(art.wide), pattern="para")
    # also remove ID column
    idx <- c(id.col.idx)#, idx.pf)
    # this could result in an error if all fragments are para*
    art.wide$total_art_pct <- rowSums(art.wide[, -idx], na.rm=TRUE)
  }

  ## TODO: 0 is returned when all NA and nullFragsAreZero=FALSE
  ## https://github.com/ncss-tech/soilDB/issues/57
  # corrections:
  # 1. fine gravel is a subset of gravel, therefore: gravel = gravel + fine_gravel
  art.wide$art_gr <- rowSums(cbind(art.wide$art_gr, art.wide$art_fgr), na.rm = TRUE)

  # now, do some summaries of cohesion, shape, roundess, penetrability, safety and persistence

  art.wide$huartvol_cohesive <- as.numeric(lapply(split(art, art[[id.var]]), function(art.sub) {
    sum(art.sub$huartvol[art.sub$huartco == "cohesive"], na.rm = TRUE)
  }))
  art.wide$huartvol_penetrable <- as.numeric(lapply(split(art, art[[id.var]]), function(art.sub) {
    sum(art.sub$huartvol[art.sub$huartpen == "penetrable"], na.rm = TRUE)
  }))
  art.wide$huartvol_noxious <- as.numeric(lapply(split(art, art[[id.var]]), function(art.sub) {
    sum(art.sub$huartvol[art.sub$huartsafety == "noxious artifacts"], na.rm = TRUE)
  }))
  art.wide$huartvol_persistent <- as.numeric(lapply(split(art, art[[id.var]]), function(art.sub) {
    sum(art.sub$huartvol[art.sub$huartper == "persistent"], na.rm = TRUE)
  }))

  # TODO: somehow summarize shape and roundness? we don't do that for regular frags

  # done
  return(art.wide)
}

