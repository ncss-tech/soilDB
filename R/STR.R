
#
STRplot <- function(mast, msst, mwst, O.hz, saturated, pt.cex=2.75, leg.cex=0.85) {
  
  # make a row of rectangles with colors based on STR
  .makeRow <- function(x, y, STR, colors){
    x.offset <- 0.4
    y.offset <- 0.25
    
    rect(xleft=x - x.offset, 
         xright = x + x.offset, 
         ybottom = rep(y, times=length(x)) - y.offset, 
         ytop = rep(y, times=length(x)) + y.offset, 
         col=colors[as.numeric(STR)])
  }
  
  
  # reasonable colors for STR
  cols <- c("white", "purple", "blue", "lightblue", "green", "lightgreen", 
            "orange", "yellow", "brown", "red")
  
  
  
  # can only iterate over a single axis
  if(length(mast) > 1 & length(msst) > 1)
    stop()
  
  # which axis are we iterating over?
  if(length(mast) > 1) {
    
    # iterate over MAST
    x <- mast
    x.label <- 'MAST (C)'
    alt.label <- 'MSST'
    alt.summary <- msst
    
    msst <- rep(msst, times=length(mast))
    mwst <- rep(mwst, times=length(mast))
    
  } else if(length(msst) > 1) {
    
    # iterate over MSST
    x <- msst
    x.label <- 'MSST (C)'
    alt.label <- 'MAST'
    alt.summary <- mast
    
    mast <- rep(mast, times=length(msst))
    mwst <- rep(mwst, times=length(msst))
  }
  
  
  ## estimate STR over 9 possible situations
  # no information
  x.1 <- estimateSTR(mast, msst, mwst, O.hz = rep(NA, times=length(mast)), saturated = rep(NA, times=length(mast)))
  # O hz information
  x.2 <- estimateSTR(mast, msst, mwst, O.hz = rep(TRUE, times=length(mast)), saturated = rep(NA, times=length(mast)))
  x.3 <- estimateSTR(mast, msst, mwst, O.hz = rep(FALSE, times=length(mast)), saturated = rep(NA, times=length(mast)))
  # saturation information
  x.4 <- estimateSTR(mast, msst, mwst, O.hz = rep(NA, times=length(mast)), saturated = rep(TRUE, times=length(mast)))
  x.5 <- estimateSTR(mast, msst, mwst, O.hz = rep(NA, times=length(mast)), saturated = rep(FALSE, times=length(mast)))
  # both O and saturation information
  x.6 <- estimateSTR(mast, msst, mwst, O.hz = rep(TRUE, times=length(mast)), saturated = rep(TRUE, times=length(mast)))
  x.7 <- estimateSTR(mast, msst, mwst, O.hz = rep(FALSE, times=length(mast)), saturated = rep(FALSE, times=length(mast)))
  x.8 <- estimateSTR(mast, msst, mwst, O.hz = rep(TRUE, times=length(mast)), saturated = rep(FALSE, times=length(mast)))
  x.9 <- estimateSTR(mast, msst, mwst, O.hz = rep(FALSE, times=length(mast)), saturated = rep(TRUE, times=length(mast)))
  
  # init plot
  plot(x, rep(1, times=length(x.1)), type='n', 
       axes=FALSE, ylab='', xlab='', xlim=c(min(x) - 0.75, max(x) + 0.5), ylim=c(1, 11))
  
  # horizontal and vertical guides
  segments(x0 = x, x1 = x, y0 = 0, y1 = 9, lty=3, col='grey')
  segments(y0 = (2:9)-0.4, y1 = (2:9)-0.4, x0 = -0.5, x1 = max(x)+0.5, lty=1, col=c('black', 'grey', 'black', 'grey'))
  
  # thematic rectangles with STR information
  .makeRow(x=x, y=1, STR=x.1, colors = cols)
  .makeRow(x=x, y=2, STR=x.2, colors = cols)
  .makeRow(x=x, y=3, STR=x.3, colors = cols)
  .makeRow(x=x, y=4, STR=x.4, colors = cols)
  .makeRow(x=x, y=5, STR=x.5, colors = cols)
  .makeRow(x=x, y=6, STR=x.6, colors = cols)
  .makeRow(x=x, y=7, STR=x.7, colors = cols)
  .makeRow(x=x, y=8, STR=x.8, colors = cols)
  .makeRow(x=x, y=9, STR=x.9, colors = cols)
  
  # label x-axis
  axis(side = 1, at = x, cex.axis=0.75)
  mtext(x.label, side=1, line=2.5, font=2)
  
  # label non-varying parameters
  mtext(sprintf('MWST: %s (C)', unique(mwst)), side=1, at=min(x), line=2.5, adj=0, font=2)
  mtext(sprintf('%s: %s (C)', alt.label, alt.summary), side=1, at=max(x)+0.5, line=2.5, adj=1, font=2)
  
  # labels cases
  mtext('O / Saturation', side=3, at=min(x)+0.25, line=-5.5, adj=1, font=3, cex=0.75)
  text(min(x) - 1, 1:9, labels = 
         c('? | ?', 
           'O | ?', 
           'X | ?', 
           '? | S', 
           '? | X', 
           'O | S', 
           'X | X', 
           'O | X', 
           'X | S'
         ), cex=0.75, font=2, adj=0.5)
  
  # legend
  legend('top', 
         legend=c("gelic", "cryic", "frigid", "isofrigid", "mesic", "isomesic", 
                  "thermic", "isothermic", "hyperthermic", "isohyperthermic"),
         pt.bg=c("white", "purple", "blue", "lightblue", "green", "lightgreen", 
                 "orange", "yellow", "brown", "red"),
         pch=22,
         pt.cex=2,
         cex=leg.cex,
         ncol=5,
         inset = 0.1
  )
  
  
}



# vectors of MAST, summer mean, winter mean all in Deg C
estimateSTR <- function(mast, mean.summer, mean.winter, O.hz=NA, saturated=NA) {
  
  # check to make sure that the lengths of vectors are the same
  if(! all.equal(length(mast), length(mean.summer), length(mean.winter)))
    stop('inputs must all have the same length', call. = TRUE)
  
  # iterate over input
  n <- length(mast)
  res <- vector(mode = 'character', length = n)
  
  for(i in seq_along(mast)) {
    # check for NA
    if(any(is.na(c(mast[i], mean.summer[i], mean.winter[i])))){
      res[i] <- NA
      next
    }
    
    # gelic, suborder and GG levels
    if(mast[i] <= 0) {
      res[i] <- 'gelic'
      next
    }
    
    # gelic, order level
    if(mast[i] <= 1) {
      res[i] <- 'gelic'
      next
    }
    
    
    # possibly cryic, because we don't know saturation and O hz status
    if(mast[i] < 8) {
      
      # no additional information
      # assume: saturated during part of summer & no O horizon
      if( is.na(saturated[i] & is.na(O.hz[i]))) {
        # otherwise it is just an estimate: 
        if(mean.summer[i] < 13) {
          res[i] <- 'cryic'
          next
        }
      }
      
      
      # O horizon information only
      # assume: saturated during part of summer
      if(!is.na(O.hz[i])) {
        
        # no O horizon
        if(! O.hz[i]) {
          if(mean.summer[i] > 0 & mean.summer[i] < 13) {
            res[i] <- 'cryic'
            next
          }
        }
        
        # O horizon
        if(O.hz[i]) {
          if(mean.summer[i] > 0 & mean.summer[i] < 6) {
            res[i] <- 'cryic'
            next
          }
        }
        
      }
      
      # if we have both saturation and O hz information we can be sure
      if(! is.na(saturated[i]) & ! is.na(O.hz[i])) {
        
        # not saturated
        if(! saturated[i]) {
          
          # no O horizon
          if(! O.hz[i]) {
            if(mean.summer[i] > 0 & mean.summer[i] < 15) {
              res[i] <- 'cryic'
              next
            }
          }
          
          # O horizon
          if(O.hz[i]) {
            if(mean.summer[i] > 0 & mean.summer[i] < 8) {
              res[i] <- 'cryic'
              next
            }
          }
        }
        
        # saturated
        if(saturated[i]) {
          
          # no O horizon
          if(! O.hz[i]) {
            if(mean.summer[i] > 0 & mean.summer[i] < 13) {
              res[i] <- 'cryic'
              next
            }
          }
          
          # O horizon
          if(O.hz[i]) {
            if(mean.summer[i] > 0 & mean.summer[i] < 6) {
              res[i] <- 'cryic'
              next
            }
          }
        }
        
      } 
      
    }
    
    
    ## frigid
    if(mast[i] < 8) {
      if(mean.summer[i] - mean.winter[i] >= 6) {
        res[i] <- 'frigid'
        next
      }
      else {
        res[i] <- 'isofrigid'
        next
      }
    }
    
    # mesic
    if(mast[i] >= 8 & mast[i] < 15) {
      if(mean.summer[i] - mean.winter[i] >= 6) {
        res[i] <- 'mesic'
        next
      }
      else {
        res[i] <- 'isomesic'
        next
      }
    }
    
    # thermic
    if(mast[i] >= 15 & mast[i] < 22) {
      if(mean.summer[i] - mean.winter[i] >= 6){
        res[i] <- 'thermic'
        next
      }
      else {
        res[i] <- 'isothermic'
        next
      }
    }
    
    # hyperthermic
    if(mast[i] >= 22) {
      if(mean.summer[i] - mean.winter[i] >= 6) {
        res[i] <- 'hyperthermic'
        next
      }
      else {
        res[i] <- 'isohyperthermic'
        next
      }
    }
    
    # unknown
    res[i] <- NA
  }
  
  # set levels
  res <- factor(res, levels=c('gelic', 'cryic','frigid','isofrigid','mesic','isomesic','thermic','isothermic','hyperthermic','isohyperthermic'))
  
  # done
  return(res)
}
