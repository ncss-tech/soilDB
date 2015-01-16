## depreciated
# # compute pair-wise weightes for soil-relation graph
# .pair.wise.wts <- function(d, wt) {
#   x <- d[[wt]]
#   x <- x / sum(x)
#   w <- outer(x, x, FUN='*')
#   return(w)
# }


## new method based on dissimilarity eval of community matrix
component.adj.matrix <-function(d, mu='mukey', co='compname', wt='comppct_r', method='community.matrix', standardization='max', metric='jaccard', rm.orphans=TRUE, similarity=TRUE) {
	
  # appease R CMD check
  .wt <- NULL
  
  # sanity check
  if(! method %in% c('community.matrix', 'occurrence')) {
    stop('please specify a method: `community.matrix` | `occurrence`')
  }
  
  ## hack: move wt column into '.wt'
  d$.wt <- d[[wt]]
  
  # aggregate component percentages when multiple components of the same name are present
  ## note that we are using weights as moved into the new column '.wt'
  d <- ddply(d, c(mu, co), summarise, weight=sum(.wt))
  # re-order
  d <- d[order(d[[mu]], d[[co]]), ]
  
  # extract a list of component names that occur together
  l <- dlply(d, mu, function(i) unique(i[[co]]))
  
  # optionally keep map units with only a single component
  if(rm.orphans) {
    # include only those components that occur with other components
    mu.multiple.components <- names(which(sapply(l, function(i) length(i) > 1)))
    
    # subset, keeping only those map units with > 1 component
    d <- d[which(d[[mu]] %in% mu.multiple.components), ]
  }
  
  # naive method, no weighting
  if(method == 'occurrence') {
    # get unique vector of component names and sort
    u <- unique(d[[co]])
    u <- sort(u)
    
    # empty adjacency matrix
    m <- matrix(rep(0, times=length(u)^2), nrow=length(u))
    dimnames(m) <- list(u, u)
    
    # compute co-occurances
    for(i in 1:length(l)) {
      # get current list element
      series.vect <- na.omit(l[[i]])	
      # check to make sure there are more than 1 components in this map unit
      if(length(series.vect) > 1) {
        ## increment affected elements by 1
        m[series.vect, series.vect] <- m[series.vect, series.vect] + 1			
      }
    }
    
    # set diagonal and lower triangle to 0
    m[lower.tri(m)] <- 0
    diag(m) <- 0
  }
  
  
  # default method, based on ideas from ecological community matrix analysis
  if(method == 'community.matrix') {
    
    # sanity checks
    if(is.null(wt))
      stop('must supply a column name containing weights')
    
    # reshape to component x mukey community matrix
    fm <- as.formula(paste(co, ' ~ ', mu, sep=''))
    d.wide <- dcast(d, fm, value.var='weight', fill=0)
    
    # convert into community matrix by moving first column -> row.names
    d.mat <- as.matrix(d.wide[, -1])
    dimnames(d.mat)[[1]] <- d.wide[[co]]
    
    # convert community matrix into dissimilarity matrix
    ## standardization method and distance metric MATTER
    m <- vegdist(decostand(d.mat, method=standardization), method=metric)
    
    # convert to similarity matrix: S = max(D) - D [Kaufman & Rousseeuw]
    if(similarity == TRUE) {
      m <- as.matrix(max(m) - m)
      mat.type <- 'similarity'
      
      # set diagonal and lower triangle to 0
      m[lower.tri(m)] <- 0
      diag(m) <- 0
      
      # set attributes related to this method
      attr(m, 'standardization') <- standardization
      attr(m, 'metric') <- metric
    }
        
  }
  
  # set method attribute
  attr(m, 'method') <- method
  
	# done
	return(m)
}
