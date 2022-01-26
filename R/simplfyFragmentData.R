## TODO: generalize, export, and make sieve sizes into an argument

# latest NSSH part 618
# https://directives.sc.egov.usda.gov/OpenNonWebContent.aspx?content=44371.wba

# internally-used function to test size classes
# diameter is in mm
# NA diameter results in NA class
.sieve <- function(diameter, flat=FALSE, para=FALSE, new.names = NULL) {

  # flat fragments
  if(flat == TRUE)
    sieves <- c(channers=150, flagstones=380, stones=600, boulders=10000000000)

  # non-flat fragments
  if(flat == FALSE)
    sieves <- c(fine_gravel=5, gravel=75, cobbles=250, stones=600, boulders=10000000000)

  if(!is.null(new.names))
    names(sieves) <- new.names

  # test for NA, and filter-out
  res <- vector(mode='character', length = length(diameter))
  res[which(is.na(diameter))] <- NA
  no.na.idx <- which(!is.na(diameter))

  # only assign classes to non-NA diameters
  if(length(no.na.idx) > 0) {
    # pass diameters "through" sieves
    # 2020: latest part 618 uses '<' for all upper values of class range
    classes <- t(sapply(diameter[no.na.idx], function(i) i < sieves))

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
.rockFragmentSieve <- function(x, vol.var = "fragvol", prefix = "frag") {

  xvar <- vol.var
  hardvar <- paste0(prefix, "hard")
  shpvar <- paste0(prefix, "shp")
  sizevar <- paste0(paste0(prefix, "size"), c("_l","_r","_h"))
  
  # convert to lower case: NASIS metadata uses upper for labels, lower for values
  x[[hardvar]] <- tolower(x[[hardvar]])
  x[[shpvar]] <- tolower(x[[shpvar]])

  ## assumptions
  # missing hardness = rock fragment
  x[[hardvar]][which(is.na(x[[hardvar]]))] <- 'strongly cemented'
  # missing shape = Nonflat
  x[[shpvar]][which(is.na(x[[shpvar]]))] <- 'nonflat'

  ## the RV fragment size is likely the safest estimate,
  ## given the various upper bounds for GR (74mm, 75mm, 76mm)
  # calculate if missing
  x[[sizevar[2]]] <- ifelse(
    is.na(x[[sizevar[2]]]),
    (x[[sizevar[1]]] + x[[sizevar[3]]]) / 2,
    x[[sizevar[2]]]
  )

  ## split frags / parafrags
  # frags: >= strongly cemented
  # this should generalize across old / modern codes
  idx <- grep('strong|indurated', x[[hardvar]], ignore.case = TRUE)
  frags <- x[idx, ]

  idx <- grep('strong|indurated', x[[hardvar]], ignore.case = TRUE, invert = TRUE)
  parafrags <- x[idx, ]


  ## split flat / non-flat
  # frags
  idx <- which(frags[[shpvar]] == 'nonflat')
  frags.nonflat <- frags[idx, ]

  idx <- which(frags[[shpvar]] == 'flat')
  frags.flat <- frags[idx, ]

  # parafrags
  idx <- which(parafrags[[shpvar]] == 'nonflat')
  parafrags.nonflat <- parafrags[idx, ]

  idx <- which(parafrags[[shpvar]] == 'flat')
  parafrags.flat <- parafrags[idx, ]

  ## sieve
  # non-flat fragments
  frags.nonflat$class <- .sieve(frags.nonflat[[sizevar[2]]], flat = FALSE)

  # non-flat parafragments
  parafrags.nonflat$class <- .sieve(parafrags.nonflat[[sizevar[2]]], flat = FALSE, para = TRUE)

  # flat fragments
  frags.flat$class <- .sieve(frags.flat[[sizevar[2]]], flat = TRUE)

  # flat parafragments
  parafrags.flat$class <- .sieve(parafrags.flat[[sizevar[2]]], flat = TRUE, para = TRUE)

  # combine pieces, note may contain  RF classes == NA
  res <- rbind(frags.nonflat, frags.flat, parafrags.nonflat, parafrags.flat)

  # what does an NA fragment class mean?
  #
  # typically, fragment size missing
  # or, worst-case, .sieve() rules are missing criteria
  #
  # keep track of these for QC in an 'unspecified' column
  # but only when there is a fragment volume specified
  idx <- which(is.na(res$class) & !is.na(res[[vol.var]]))
  if( length(idx) > 0 ) {
    res$class[idx] <- 'unspecified'
  }

  # done
  return(res)
}


# rf: un-coded contents of the phfrags table
# id.var: id column name
# nullFragsAreZero: convert NA to 0?


#' Simplify Coarse Fraction Data
#'
#' Simplify multiple coarse fraction (>2mm) records by horizon.
#'
#' This function is mainly intended for processing of NASIS pedon/component
#' data which contains multiple coarse fragment descriptions per horizon.
#' `simplifyFragmentData` will "sieve out" coarse fragments into the USDA
#' classes, split into hard and para- fragments. Likewise, `simplifyArtifactData` will sieve out human artifacts, and split total volume into "cohesive", "penetrable", "innocuous", and "persistent".
#'
#' These functions can be applied to data sources other than NASIS by careful use of the `id.var` and `vol.var` arguments. 
#'  - \code{rf} must contain rock or other fragment volumes in the column "fragvol" (or be specified with `vol.var`), fragment size (mm) in columns "fragsize_l", "fragsize_r", "fragsize_h", fragment cementation class in "fraghard" and flat/non-flat in "fragshp".
#'  - \code{art} must contain artifact volumes in the column "huartvol" (or be specified with `vol.var`), fragment size (mm) in columns "huartsize_l", "huartsize_r", "huartsize_h", artifact cementation class in "huarthard" and flat/non-flat in "huartshp".
#'
#' Examples:
#'  - [KSSL data](http://ncss-tech.github.io/AQP/soilDB/KSSL-demo.html)
#'
#' @aliases simplifyFragmentData simplfyFragmentData simplifyArtifactData
#' @param rf a \code{data.frame} object, typically returned from NASIS, see
#' details
#' @param id.var character vector with the name of the column containing an ID
#' that is unique among all horizons in \code{rf}
#' @param vol.var character vector with the name of the column containing the coarse fragment volume. Default `"fragvol"` or `"huartvol`".
#' @param prefix a character vector prefix for input
#' @param nullFragsAreZero should fragment volumes of NULL be interpreted as 0? (default: `TRUE`), see details
#' @param msg Identifier of data being summarized. Default is `"rock fragment volume"` but this routine is also used for `"surface fragment cover"`
#' @author D.E. Beaudette, A.G Brown
#' @keywords manip
#' @export simplifyFragmentData
simplifyFragmentData <- function(rf, id.var, vol.var = "fragvol", prefix = "frag", nullFragsAreZero = TRUE, msg = "rock fragment volume") {

  fragvol <- NULL

  # fragment classes used in this function
  # note that we are adding a catch-all for those strange phfrags records missing fragment size
  frag.classes <- c('fine_gravel', 'gravel', 'cobbles', 'stones', 'boulders', 'channers', 'flagstones', 'parafine_gravel', 'paragravel', 'paracobbles', 'parastones', 'paraboulders', 'parachanners', 'paraflagstones', 'unspecified')

  result.columns <- c(id.var, frag.classes, "total_frags_pct", "total_frags_pct_nopf")

  # warn the user and remove the NA records
  
  # if all fragvol are NA then rf is an empty data.frame and we are done
  if (nrow(rf[which(!is.na(rf[[vol.var]])),]) == 0) {
    message(sprintf('NOTE: all records are missing %s', msg))
    dat <- as.data.frame(t(rep(NA, length(result.columns))))
    for(i in 1:length(rf[[id.var]])) {
      dat[i,] <- dat[1,]
      dat[i,which(result.columns == id.var)] <- rf[[id.var]][i]
    }
    colnames(dat) <- result.columns
    return(dat)
  } else if (any(is.na(rf[[vol.var]]))) {
    rf <- rf[which(!is.na(rf[[vol.var]])), ]
    message(sprintf('NOTE: some records are missing %s', msg))
  }

  # extract classes
  # note: these will put any fragments without fragsize into an 'unspecified' class
  rf.classes <- .rockFragmentSieve(rf, vol.var = vol.var, prefix = prefix)

  ## NOTE: this is performed on the data, as-is: not over all possible classes as enforced by factor levels
  # sum volume by id and class
  # class cannot contain NA
  rf.sums <- aggregate(rf.classes[[vol.var]], by=list(rf.classes[[id.var]], rf.classes[['class']]), FUN=sum, na.rm=TRUE)
  # fix defualt names from aggregate()
  names(rf.sums) <- c(id.var, 'class', 'volume')

  ## NOTE: we set factor levels here because the reshaping (long->wide) needs to account for all possible classes
  ## NOTE: this must include all classes that related functions return
  # set levels of classes
  rf.sums$class <- factor(rf.sums$class, levels=frag.classes)
  
  # convert to wide format
  if (nrow(rf.sums) == 0) {
    rf.wide <- as.data.frame(rep(list(numeric(0)), length(frag.classes)))
    colnames(rf.wide) <- frag.classes
    iddf <- data.frame(character(0))
    colnames(iddf) <- id.var
    rf.wide <- cbind(iddf, rf.wide)
  } else {
    fm <- as.formula(paste0(id.var, ' ~ class'))
    rf.wide <- as.data.frame(data.table::dcast(data.table::data.table(rf.sums), fm, value.var = 'volume', drop = FALSE))
  }
  # must determine the index to the ID column in the wide format
  id.col.idx <- which(names(rf.wide) == id.var)

  ## optionally convert NULL frags -> 0
  if(nullFragsAreZero & ncol(rf.wide) > 1) {
    rf.wide <- as.data.frame(
      cbind(rf.wide[, id.col.idx, drop=FALSE],
            lapply(rf.wide[, -id.col.idx], function(i) ifelse(is.na(i), 0, i))
      ), stringsAsFactors = FALSE)
  }

  # final sanity check: are there any fractions or the total >= 100%
  # note: sapply() was previously used here
  #       1 row in rf.wide --> result is a vector
  #       >1 row in rf.wide --> result is a matrix
  # solution: keep as a list
  gt.100 <- lapply(rf.wide[, -id.col.idx, drop = FALSE], FUN = function(i) i >= 100)

  # check each size fraction and report id.var if there are any
  gt.100.matches <- sapply(gt.100, any, na.rm=TRUE)
  if(any(gt.100.matches)) {
    # search within each fraction
    class.idx <- which(gt.100.matches)
    idx <- unique(unlist(lapply(gt.100[class.idx], which)))
    flagged.ids <- rf.wide[[id.var]][idx]

    warning(sprintf("%s >= 100%%\n%s:\n%s", msg, id.var, paste(flagged.ids, collapse = "\n")), call. = FALSE)
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
    rf.wide$total_frags_pct <- rowSums(rf.wide[, -idx], na.rm = TRUE)
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

