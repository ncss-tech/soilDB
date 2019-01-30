
# internally-used function to test size classes
# diameter is in mm
# NA diameter results in NA class
.sieve <- function(diameter, flat=FALSE, para=FALSE) {
  
  # flat fragments
  if(flat == TRUE)
    sieves <- c(channers=150, flagstones=380, stones=600, boulders=10000000000)
  
  # non-flat fragments
  if(flat == FALSE)
    sieves <- c(fine_gravel=5, gravel=76, cobbles=250, stones=600, boulders=10000000000)
  
  # test for NA, and filter-out
  res <- vector(mode='character', length = length(diameter))
  res[which(is.na(diameter))] <- NA
  no.na.idx <- which(!is.na(diameter))
  
  # only assign classes to non-NA diameters
  if(length(no.na.idx) > 0) {
    # pass diameters "through" sieves
    classes <- t(sapply(diameter[no.na.idx], function(i) i <= sieves))
    
    # determine largest passing sieve name
    res[no.na.idx] <- names(sieves)[apply(classes, 1, which.max)]
    
    # change names if we are working with parafrags
    if(para == TRUE)
      res[no.na.idx] <- paste0('para', res[no.na.idx])
  }
  
  return(res)  
}


## TODO: this is NASIS-specific for now, generalize this to any data
# x: uncoded contents of the phfrags table
.rockFragmentSieve <- function(x) {
  
  # convert to lower case: NASIS metadata usese upper for labels, lower for values
  x$fraghard <- tolower(x$fraghard)
  x$fragshp <- tolower(x$fragshp)
  
  ## assumptions
  # missing hardness = rock fragment
  x$fraghard[which(is.na(x$fraghard))] <- 'strongly cemented'
  # missing shape = Nonflat
  x$fragshp[which(is.na(x$fragshp))] <- 'nonflat'
  
  ## split frags / parafrags
  # frags: >= strongly cemented
  # this should generalize across old / modern codes
  idx <- grep('strong|indurated', x$fraghard, ignore.case = TRUE)
  frags <- x[idx, ]
  
  idx <- grep('strong|indurated', x$fraghard, ignore.case = TRUE, invert = TRUE)
  parafrags <- x[idx, ]
  
  
  ## split flat / non-flat
  # frags
  idx <- which(frags$fragshp == 'nonflat')
  frags.nonflat <- frags[idx, ]
  
  idx <- which(frags$fragshp == 'flat')
  frags.flat <- frags[idx, ]
  
  # parafrags
  idx <- which(parafrags$fragshp == 'nonflat')
  parafrags.nonflat <- parafrags[idx, ]
  
  idx <- which(parafrags$fragshp == 'flat')
  parafrags.flat <- parafrags[idx, ]
  
  ## sieve
  # non-flat fragments
  d <- ifelse(is.na(frags.nonflat$fragsize_h), frags.nonflat$fragsize_r, frags.nonflat$fragsize_h)
  frags.nonflat$class <- .sieve(d, flat = FALSE)
  
  # non-flat parafragments
  d <- ifelse(is.na(parafrags.nonflat$fragsize_h), parafrags.nonflat$fragsize_r, parafrags.nonflat$fragsize_h)
  parafrags.nonflat$class <- .sieve(d, flat = FALSE, para = TRUE)
  
  # flat fragments
  d <- ifelse(is.na(frags.flat$fragsize_h), frags.flat$fragsize_r, frags.flat$fragsize_h)
  frags.flat$class <- .sieve(d, flat = TRUE)
  
  # flat parafragments
  d <- ifelse(is.na(parafrags.flat$fragsize_h), parafrags.flat$fragsize_r, parafrags.flat$fragsize_h)
  parafrags.flat$class <- .sieve(d, flat = TRUE, para = TRUE)
  
  # combine pieces, note may contain  RF classes == NA
  res <- rbind(frags.nonflat, frags.flat, parafrags.nonflat, parafrags.flat)
  
  
  # what does an NA fragment class mean?
  # 
  # typically, fragment size missing
  # or, worst-case, .sieve() rules are missing criteria 
  # 
  # keep track of these for QC in an 'unspecified' column
  # but only when there is a fragment volume specified
  idx <- which(is.na(res$class) & !is.na(res$fragvol))
  if( length(idx) > 0 ) {
    res$class[idx] <- 'unspecified'
  }
  
  # done
  return(res)
}


# rf: un-coded contents of the phfrags table
# id.var: id column name
# nullFragsAreZero: convert NA to 0?
simplifyFragmentData <- function(rf, id.var, nullFragsAreZero=TRUE) {
  
  # nasty hack to trick R CMD check
  fragvol <- NULL
  
  # fragment classes used in this function
  # note that we are adding a catch-all for those strange phfrags records missing fragment size
  frag.classes <- c('fine_gravel', 'gravel', 'cobbles', 'stones', 'boulders', 'channers', 'flagstones', 'parafine_gravel', 'paragravel', 'paracobbles', 'parastones', 'paraboulders', 'parachanners', 'paraflagstones', 'unspecified')
  
  # first of all, we can't do anything if the fragment volume is NA
  # warn the user and remove the offending records
  if(any(is.na(rf$fragvol))) {
    warning('some records are missing rock fragment volume, these have been removed', call. = FALSE)
  }
  rf <- rf[which(!is.na(rf$fragvol)), ]
  
  # if all fragvol are NA then rf is an empty data.frame and we are done
  if(nrow(rf) == 0) {
    warning('all records are missing rock fragment volume, returing NULL', call. = FALSE)
    return(NULL)
  }
  
  
  # extract classes
  # note: these will put any fragments without fragsize into an 'unspecified' class
  rf.classes <- .rockFragmentSieve(rf)
  
  ## this is incredibly slow (~ 5 seconds for 30k records)
  ## NOTE: this is performed on the data, as-is: not over all possible classes as enforced by factor levels
  # sum volume by id and class
  # rf.sums <- ddply(rf.classes, c(id.var, 'class'), plyr::summarise, volume=sum(fragvol, na.rm=TRUE))
  
  ## NOTE: this is performed on the data, as-is: not over all possible classes as enforced by factor levels
  # sum volume by id and class
  # much faster than ddply
  # class cannot contain NA
  rf.sums <- aggregate(rf.classes$fragvol, by=list(rf.classes[[id.var]], rf.classes[['class']]), FUN=sum, na.rm=TRUE)
  # fix defualt names from aggregate()
  names(rf.sums) <- c(id.var, 'class', 'volume')
  
  
  ## NOTE: we set factor levels here because the reshaping (long->wide) needs to account for all possible classes
  ## NOTE: this must include all classes that related functions return
  # set levels of classes
  rf.sums$class <- factor(rf.sums$class, levels=frag.classes)
  
  # convert to wide format
  fm <- as.formula(paste0(id.var, ' ~ class'))
  rf.wide <- reshape2::dcast(rf.sums, fm, value.var = 'volume', drop = FALSE)
  
  # must determine the index to the ID column in the wide format
  id.col.idx <- which(names(rf.wide) == id.var)
  
  
  ## optionally convert NULL frags -> 0
  if(nullFragsAreZero & ncol(rf.wide) > 1) {
    rf.wide <- as.data.frame(
      cbind(rf.wide[, id.col.idx, drop=FALSE], 
            lapply(rf.wide[, -id.col.idx], function(i) ifelse(is.na(i), 0, i))
      ), stringsAsFactors=FALSE)
  }
  
  # final sanity check: are there any fractions or the total >= 100%
  # note: sapply() was previously used here
  #       1 row in rf.wide --> result is a vector
  #       >1 row in rf.wide --> result is a matrix
  # solution: keep as a list
  gt.100 <- lapply(rf.wide[, -id.col.idx, drop=FALSE], FUN=function(i) i >= 100)
  
  # check each size fraction and report id.var if there are any
  gt.100.matches <- sapply(gt.100, any, na.rm=TRUE)
  if(any(gt.100.matches)) {
    # search within each fraction
    class.idx <- which(gt.100.matches)
    idx <- unique(unlist(lapply(gt.100[class.idx], which)))
    flagged.ids <- rf.wide[[id.var]][idx]
    
    warning(sprintf("fragment volume >= 100%%\n%s:\n%s", id.var, paste(flagged.ids, collapse = "\n")), call. = FALSE)
  }
  
  ## TODO: 0 is returned when all NA and nullFragsAreZero=FALSE
  ## https://github.com/ncss-tech/soilDB/issues/57
  # compute total fragments
  # trap no frag condition
  # includes unspecified class
  if(ncol(rf.wide) > 1) {
    # calculate another column for total RF, ignoring parafractions
    # index of columns to ignore, para*
    idx.pf <- grep(names(rf.wide), pattern="para")
    # also remove ID column
    idx <- c(id.col.idx, idx.pf)
    # this could result in an error if all fragments are para*
    rf.wide$total_frags_pct_nopf <- rowSums(rf.wide[, -idx], na.rm=TRUE)
    
    # calculate total fragments (including para)
    # excluding ID and last columns
    idx <- c(id.col.idx, length(names(rf.wide)))
    rf.wide$total_frags_pct <- rowSums(rf.wide[, -idx], na.rm=TRUE)
  }
  
  ## TODO: 0 is returned when all NA and nullFragsAreZero=FALSE
  ## https://github.com/ncss-tech/soilDB/issues/57
  # corrections:
  # 1. fine gravel is a subset of gravel, therefore: gravel = gravel + fine_gravel
  rf.wide$gravel <- rowSums(cbind(rf.wide$gravel, rf.wide$fine_gravel), na.rm = TRUE)
  rf.wide$paragravel <- rowSums(cbind(rf.wide$paragravel, rf.wide$parafine_gravel), na.rm=TRUE)
  
  # done
  return(rf.wide)
}

