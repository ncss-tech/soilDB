# sample by polygon, must be from a projected CRS
# p: Polygon object
# n: number of points per acre (results will be close)
# min.samples: minimum requested samples / polygon
# iterations: number of sampling "tries"
# ...: additional arguments to spsample
sample.by.poly <- function(p, n.pts.per.ac=1, min.samples=5, sampling.type='hexagonal', iterations=10, ...) {
  # convert _projected_ units to acres
  ac.i <- p@area * 2.47e-4
  # determine number of points based on requested density
  n.samples <- round(ac.i * n.pts.per.ac)
  
  # polygon must be at least large enough to support requested number of samples
  if(n.samples >= min.samples) {
    # trap errors caused by bad geometry
    s.i <- try(spsample(p, n=n.samples, type=sampling.type, iter=iterations, ...), silent=TRUE)
    
    # errors? return NULL
    if(class(s.i) == 'try-error')
      s.i <- NULL
  }
  
  # not enough samples, return NULL
  else
    s.i <- NULL
  
  # return spatial points object in the same CRS
  return(s.i)
}
