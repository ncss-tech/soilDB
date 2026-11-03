## re-write of previous version, should be more robust to missing weights and generalize to other uses
## note: this isn't real mixing, reflectance curves and / or Kubelka-Monk modeling required for that
## all colors are mixed, should be applied to groups of related colors

## TODO: future release wrap / replace with aqp::mixMunsell(mixingMethod = 'estimate') as it is better maintained / tested


#' @title Estimate color mixtures using weighted average of CIELAB color coordinates
#'
#' @note See [aqp::mixMunsell()] for a more realistic (but slower) simulation of subtractive mixing of pigments. An efficient replacement for this function (wt. mean in CIELAB coordinates) is implemented in `aqp::mixMunsell(..., mixingMethod = 'estimate')`.
#' 
#' @author D.E. Beaudette
#'
#' @param x data.frame, typically from NASIS containing at least CIE LAB ('L', 'A', 'B') and some kind of weight
#' 
#' @param wt numeric. fractional weights, usually area of horizon face
#' 
#' @param backTransform logical, should the mixed sRGB representation of soil color be transformed to closest Munsell chips? This is performed by [aqp::col2Munsell()] default: `FALSE`
#'
#' @return A data.frame containing estimated color mixture
#' @export estimateColorMixture
estimateColorMixture <- function(x, wt = 'pct', backTransform = FALSE) {
  
  # .Deprecated(msg = paste0("estimateColorMixture() is deprecated, please use aqp::mixMunsell() instead."))

  if (!requireNamespace("aqp")) {
    stop("package 'aqp' is required", call. = FALSE)
  }
  
  ## TODO: account for `backTransform == TRUE`, different return structure
  
  # sanity check: no NA
  if(any(c(is.na(x$L), is.na(x$A), is.na(x$B)))) {
    return(data.frame(r = NA_real_, g = NA_real_, b = NA_real_))
  }
  
  # attempt to fill missing weights
  missing.wts <- is.na(x[[wt]])
  if(all(missing.wts)) {
    # constant wt if all are missing
    est.wt <- 1
    x[[wt]][which(missing.wts)] <- est.wt
  } else if (any(missing.wts)) {
    # estimated weight is the mean of all other non-NA weights
    est.wt <- mean(x[[wt]], na.rm = TRUE)
    x[[wt]][which(missing.wts)] <- est.wt
  }
  
  ## consider weighted geometric mean:
  ## https://arxiv.org/ftp/arxiv/papers/1710/1710.06364.pdf
  ## http://en.wikipedia.org/wiki/Weighted_geometric_mean
  
  # 2020-01-22 DEB: mixing always in CIELAB, 
  # better than sRGB but still not a true mixture
  # roughly linear in terms of avg. human perception of color
  L <- weighted.mean(x[['L']], w = x[[wt]], na.rm = TRUE)
  A <- weighted.mean(x[['A']], w = x[[wt]], na.rm = TRUE)
  B <- weighted.mean(x[['B']], w = x[[wt]], na.rm = TRUE)
  
  # back to sRGB
  mixed.color <- data.frame(
    grDevices::convertColor(
      cbind(L, A, B),
      from = 'Lab',
      to = 'sRGB',
      from.ref.white = 'D65',
      to.ref.white = 'D65'
    )
  )
  names(mixed.color) <- c('r', 'g', 'b')
  
  # optionally back-transform mixture to Munsell
  # performance penalty due to color distance eval against entire Munsell library
  if (backTransform) {
    # convert sRGB -> Munsell
    # requires >= aqp 2.0.2
    m <- aqp::col2Munsell(mixed.color[, c('r', 'g', 'b')])
    
    # adjust names to match NASIS
    names(m) <- c("colorhue", "colorvalue", "colorchroma", "sigma")
    
    # combine with mixed sRGB coordinates
    mixed.color <- cbind(mixed.color, m)
  }
  
  return(mixed.color)
}
