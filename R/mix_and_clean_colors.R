
## re-write of previous version, should be more robust to missing weights and generalize to other uses
## note: this isn't real mixing, relfectance curves and kubella-monk modeling required for that
## all colors are mixed, should be applied to groups of related colors

# x: data.frame, typically from NASIS containing at least 'r', 'g', 'b' colors {0,1} and some kind of weight
# wt: fractional weights, usually area of hz face
#' Mix and Clean Colors
#'
#' Deprecated: only used in PedonPC functionality; use `estimateColorMixture` instead
#'
#' @param x a \code{data.frame} object containing sRGB coordinates (`'r'`, `'g'`, `'b'`) in \[0,1]
#' @param wt fractional weights, usually area of hz face
#' @param backTransform logical, should the mixed sRGB representation of soil
#' color be transformed to closest Munsell chips? This is performed by
#'
#' @return A data.frame containing mixed colors
#' @export
mix_and_clean_colors <- function(x, wt='pct', backTransform=FALSE) {

  ## TODO finish this
  .Deprecated('estimateColorMixture', msg = '')

  # sanity check: no NA
  if(any(c(is.na(x$r), is.na(x$g), is.na(x$b))))
    return(data.frame(r=NA, g=NA, b=NA, colorhue=NA, colorvalue=NA, colorchroma=NA, sigma=NA))

  # attempt to fill missing weights
  missing.wts <- is.na(x[[wt]])
  if(any(missing.wts)) {
    # estimated weight is the mean of all other non-NA weights
    est.wt <- mean(x[[wt]], na.rm = TRUE)

    # if there are no weights, then all colors are equally weighted
    if(is.na(est.wt)) {
      est.wt <- 1
    }

    # fill missing weights
    x[[wt]][which(missing.wts)] <- est.wt
  }

  ## 2020-01-22 DEB: mixing always in CIELAB, roughly linear in terms of avg. human perception of color
  ## simulate mixture via weighted average
  # convert sRGB -> LAB
  lab.cols <- data.frame(
    convertColor(x[, c('r', 'g', 'b')], from='sRGB', to='Lab', from.ref.white='D65', to.ref.white = 'D65')
  )
  # simpler names
  names(lab.cols) <- c('L', 'A', 'B')

  # copy over weights, typically a percent by area
  lab.cols$pct <- x[[wt]]

  # compute weighted mixtures in LAB space
  # 2019-11-04 DEB: dropping Hmisc import
  L <- with(lab.cols, weighted.mean(L, w=pct, na.rm = TRUE))
  A <- with(lab.cols, weighted.mean(A, w=pct, na.rm = TRUE))
  B <- with(lab.cols, weighted.mean(B, w=pct, na.rm = TRUE))

  # back to sRGB
  mixed.color <- data.frame(convertColor(cbind(L, A, B), from='Lab', to='sRGB', from.ref.white='D65', to.ref.white = 'D65'))
  names(mixed.color) <- c('r', 'g', 'b')

  # optionally back-transform mixture to Munsell
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

