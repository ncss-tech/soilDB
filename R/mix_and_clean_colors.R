
# re-write of previous version, should be more robust to missing weights and generalize to other uses
# mixing done in CIE LAB by default now
# all colors are mixed, should be applied to groups of related colors
# x: data.frame, typically from NASIS containing at least 'r', 'g', 'b' colors {0,1} and some kind of weight
mix_and_clean_colors <- function(x, wt='pct', colorSpace='LAB') {
  
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
  
  # default: mixing is done in CIE LAB space
  if(colorSpace == 'LAB') {
    
    # convert sRGB -> LAB
    lab.cols <- data.frame(convertColor(x[, c('r', 'g', 'b')], from='sRGB', to='Lab', from.ref.white='D65', to.ref.white = 'D65'))
    names(lab.cols) <- c('L', 'A', 'B')
    
    # copy over weights, typically a percent by area
    lab.cols$pct <- x[[wt]]
    
    # compute weighted mixtures in LAB space
    L <- with(lab.cols, wtd.mean(L, weights=pct))
    A <- with(lab.cols, wtd.mean(A, weights=pct))
    B <- with(lab.cols, wtd.mean(B, weights=pct))
    
    # back to sRGB
    mixed.color <- data.frame(convertColor(cbind(L, A, B), from='Lab', to='sRGB', from.ref.white='D65', to.ref.white = 'D65'))
    names(mixed.color) <- c('r', 'g', 'b')
  } else {
    # use sRGB space
    
    # compute weighted mixtures in sRGB space
    r <- with(x, wtd.mean(r, weights=pct))
    g <- with(x, wtd.mean(g, weights=pct))
    b <- with(x, wtd.mean(b, weights=pct))
    
    mixed.color <- data.frame(r, g, b)
  }
  
  # back-transform mixture to Munsell
  m <- rgb2munsell(mixed.color)
  
  # adjust names to match NASIS
  names(m) <- c("colorhue", "colorvalue", "colorchroma", "sigma")
  
  # composite
  res <- cbind(mixed.color, m)
  
  # done
  return(res)
  
}

