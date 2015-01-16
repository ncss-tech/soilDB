
# convert soil depth into a set of depth-class factors
getSoilDepthClass <- function(f, depth.classes=c('very.shallow'=25, 'shallow'=50, 'mod.deep'=100, 'deep'=150, 'very.deep'=1000), ...) {
  
  # apply soil-depth finding function
  soil.depth <- profileApply(f, estimateSoilDepth, ...)
  
  # convert to 1-column matrix
  soil.depth <- matrix(soil.depth)
  
  # evaluate depth-class rules
  depth.class.matrix <- t(sapply(soil.depth, function(i) i < depth.classes))
  
  # determine shallowest matching rule
  best.match <- apply(depth.class.matrix, 1, function(i) names(which.min(which(i))))
  
  # zero-out the depth class matrix
  depth.class.matrix[] <- FALSE
  
  # load best matching depth class by row
  for(i in 1:nrow(depth.class.matrix)) {
    depth.class.matrix[i, best.match[i]] <- TRUE
  }
  
  # extract a vector of depth classes
  dc <- names(depth.classes)[apply(depth.class.matrix, 1, which)]
  
  # add-in ID and actual depth
  d <- data.frame(profile_id(f), depth=soil.depth, depth.class.matrix, depth.class=dc, stringsAsFactors=FALSE)
  # fix ID name
  names(d)[1] <- idname(f)
  
  return(d)
}
