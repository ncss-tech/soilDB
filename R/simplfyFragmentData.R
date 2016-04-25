
# internally-used function to test size classes
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
      res <- paste0('para', res)
  }
  
  return(res)  
}


# TODO: generalize this to any data
# NOTE: this is NASIS-specific for now
# x: raw, un-coded contents of the phfrags table
.rockFragmentSieve <- function(x) {
  
  ## split frags / parafrags
  idx <- which(x$fraghard %in% c('Strongly cemented', 'Very strongly cemented', 'Indurated') | is.na(x$fraghard))
  frags <- x[idx, ]
  
  idx <- which(! x$fraghard %in% c('Strongly cemented', 'Very strongly cemented', 'Indurated') & ! is.na(x$fraghard) )
  parafrags <- x[idx, ]
  
  
  ## split flat / non-flat
  # frags
  idx <- which(frags$fragshp == 'Nonflat' | is.na(frags$fragshp))
  frags.nonflat <- frags[idx, ]
  
  idx <- which(frags$fragshp == 'Flat' & ! is.na(frags$fragshp))
  frags.flat <- frags[idx, ]
  
  # parafrags
  idx <- which(parafrags$fragshp == 'Nonflat' | is.na(parafrags$fragshp))
  parafrags.nonflat <- parafrags[idx, ]
  
  idx <- which(parafrags$fragshp == 'Flat' & ! is.na(parafrags$fragshp))
  parafrags.flat <- parafrags[idx, ]
  
  ## sieve
  # non-flat fragments
  d <- ifelse(is.na(frags.nonflat$fragsize_h), frags.nonflat$fragsize_r, frags.nonflat$fragsize_h)
  frags.nonflat$class <- .sieve(d, flat = FALSE)
  
  # non-flat parafragments
  d <- ifelse(is.na(parafrags.nonflat$fragsize_h), parafrags.nonflat$fragsize_r, parafrags.nonflat$fragsize_h)
  parafrags.nonflat$class <- .sieve(d, flat = TRUE, para = TRUE)
  
  # flat fragments
  d <- ifelse(is.na(frags.flat$fragsize_h), frags.flat$fragsize_r, frags.flat$fragsize_h)
  frags.flat$class <- .sieve(d, flat = TRUE)
  
  # flat parafragments
  d <- ifelse(is.na(parafrags.flat$fragsize_h), parafrags.flat$fragsize_r, parafrags.flat$fragsize_h)
  parafrags.flat$class <- .sieve(d, flat = TRUE, para = TRUE)
  
  
  ## re-combine and return
  return(rbind(frags.nonflat, frags.flat, parafrags.nonflat, parafrags.flat))
}


# rf: un-coded contents of the phfrags table
# id.var: id column name
# fragClass: fragment class column name
# convert NA to 0?
simplfyFragmentData <- function(rf, id.var, fragClass, nullFragsAreZero=TRUE) {
  
  # extract classes
  rf.classes <- .rockFragmentSieve(rf)
  
  ## note: this must include all classes that related functions return
  # set levels of classes
  rf.classes[[fragClass]] <- factor(rf.classes[[fragClass]], levels=c('fine_gravel', 'gravel', 'cobbles', 'stones', 'boulders', 'channers', 'flagstones', 'parafine_gravel', 'paragravel', 'paracobbles', 'parastones', 'paraboulders', 'parachanners', 'paraflagstones'))
  
  # sum volume by id and class
  rf.sums <- ddply(rf.classes, c(id.var, fragClass), plyr::summarise, volume=sum(fragvol, na.rm=TRUE))
  
  # convert to wide format
  rf.wide <- dcast(rf.sums, labsampnum ~ class, value.var = 'volume', drop = FALSE)
  
  # fix "NA" column name
  if(any(names(rf.wide) == 'NA'))
    names(rf.wide)[which(names(rf.wide) == 'NA')] <- 'unspecified'
  
  # convert NULL frags -> 0
  if(nullFragsAreZero & ncol(rf.wide) > 1) {
    rf.wide <- as.data.frame(
      cbind(rf.wide[, 1, drop=FALSE], 
            lapply(rf.wide[, -1], function(i) ifelse(is.na(i), 0, i))
      ), stringsAsFactors=FALSE)
  }
 
  return(rf.wide)
  
}



