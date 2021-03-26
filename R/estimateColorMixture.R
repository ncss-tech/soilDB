## re-write of previous version, should be more robust to missing weights and generalize to other uses
## note: this isn't real mixing, reflectance curves and / or Kubella-Monk modeling required for that
## all colors are mixed, should be applied to groups of related colors


#' @title Estimate color mixtures using weighted average of CIELAB color coordinates
#'
#' @note See \code{\link[aqp]{mixMunsell}} for a more realistic (but slower) simulation of subtractive mixing of pigments.
#' 
#' @author D.E. Beaudette
#'
#' @param x data.frame, typically from NASIS containing at least CIE LAB ('L', 'A', 'B') and some kind of weight
#' @param wt  fractional weights, usually area of hz face
#' @param backTransform logical, should the mixed sRGB representation of soil color be transformed to closest Munsell chips? This is performed by aqp::rgb2Munsell default: \code{FALSE}
#'
#' @return A data.frame containing estimated color mixture
#' @export estimateColorMixture
#'
estimateColorMixture <- function(x, wt='pct', backTransform=FALSE) {
  
  ## TODO: account for backtransform == TRUE, different return structure
  # sanity check: no NA
  if(any(c(is.na(x$L), is.na(x$A), is.na(x$B)))) {
    return(data.frame(r=NA, g=NA, b=NA))
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
  
  # 2020-01-22 DEB: mixing always in CIELAB, roughly linear in terms of avg. human perception of color
  L <- weighted.mean(x[['L']], w=x[[wt]], na.rm = TRUE)
  A <- weighted.mean(x[['A']], w=x[[wt]], na.rm = TRUE)
  B <- weighted.mean(x[['B']], w=x[[wt]], na.rm = TRUE)
  
  # back to sRGB
  mixed.color <- data.frame(convertColor(cbind(L, A, B), from='Lab', to='sRGB', from.ref.white='D65', to.ref.white = 'D65'))
  names(mixed.color) <- c('r', 'g', 'b')
  
  # optionally back-transform mixture to Munsell
  # performance penalty due to color distance eval against entire munsell library
  if(backTransform) {
    
    # convert with best available metric
    m <- rgb2munsell(mixed.color[, c('r', 'g', 'b')])
    
    # adjust names to match NASIS
    names(m) <- c("colorhue", "colorvalue", "colorchroma", "sigma")
    
    # combine with mixed sRGB coordinates
    mixed.color <- cbind(mixed.color, m)
  }
  
  
  # done
  return(mixed.color)
  
}
