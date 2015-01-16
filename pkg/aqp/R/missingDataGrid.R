# s: soil profile collection object
# s.fm: slicing formula, including variables requested for missing data test
# cols: vector of colors palette
missingDataGrid <- function(s, max_depth, vars, filter.column = NULL, filter.regex=NULL, cols=NULL, ...) {
  
  # default color scheme
  if(is.null(cols))
    cols <- rev(brewer.pal(8, 'Spectral'))
  
  # make color pallete and define number of cuts
  cols.palette <- colorRampPalette(cols)
  ncuts <- 20
  
  # optionally filter horizon data in original object and replace
  if(!is.null(filter.column) & !is.null(filter.regex)) {
    h <- horizons(s)
    idx <- grep(filter.regex, h[, filter.column], invert=TRUE)
    horizons(s) <- h[idx, ]
    rm(h)
  }
  
  
  # get a list of horizon boundary depths for latter annotation of sliced data
  obd <- profileApply(s, simplify=FALSE, FUN=function(i) {
    hd <- horizonDepths(i)
    h <- horizons(i)
    hz.boundaries <- unique(c(h[[hd[1]]], h[[hd[2]]]))
  })
  
  
  # compute percent missing data by pedon/variable
  pct_missing <- ddply(horizons(s), idname(s), .fun=function(i, v=vars) {
    round(sapply(i[, v], function(j) length(which(is.na(j)))) / nrow(i) * 100)
  })
  
    
  # slice according to rules
  s.fm <- as.formula(paste('0:', max_depth, ' ~ ', paste(vars, collapse=' + '), sep=''))
  ss <- slice(s, s.fm)
  
  # get sliced horizon depth names
  hd <- horizonDepths(ss)
  
  # extract horizons from sliced data
  h <- horizons(ss)
  
  # get slice mid-points
  h$mid <- (h[[hd[1]]] + h[[hd[2]]]) / 2
  
  
  # NOTE: since we are converting profile IDs to a factor, 
  # we need to explicitly set levels to match the original ordering of profiles
  forced.levels <- paste("c('", paste(profile_id(ss), collapse="','"), "')", sep='')
  
  # construct levelplot formula using horizon top boundaries
  f <- as.formula(paste('.pctMissing',  ' ~ ', 'factor(', idname(ss), ', levels=', forced.levels, ') * mid', sep=''))
  
  # ylab adjustments
  ylab <- paste('Depth ', '(', depth_units(ss), ')', sep='')
  
  # depth-range adjustments
  ylim <- c(max(h$mid) + 5, -3)
  
  # plot missing data fraction
  lp <- levelplot(f, data=h, ylim=ylim, col.regions=cols.palette(ncuts), cuts=ncuts-1, ylab=ylab, xlab='', scales=list(x=list(rot=90), y=list(tick.number=10)), ..., panel=function(...) {
    panel.levelplot(...)
    panel.abline(v=1:(length(ss)+1)-0.5)
    panel.grid(h=-1, v=FALSE, lty=2, col=grey(0.25))
    for(i in 1:length(obd)) {
      panel.segments(i-0.5, obd[[i]], i+0.5, obd[[i]], col='black')
    }
  })
  
  # print level plot
  print(lp)
  
  # return missing data percentages by pedon
  return(pct_missing)
  
}
