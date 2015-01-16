## TODO: encode some kind of ID into the dissimilarity matrix for later use

evalGenHZ <- function(obj, genhz, vars, non.matching.code='not-used', stand=TRUE, trace=FALSE, metric='euclidean') {
  # hack to make R CMD check happy
  value <- summarize <- NULL
  
  # extract horizons
  h <- as(obj, 'data.frame')
  
  # make an index to complete data
  no.na.idx <- which(complete.cases(h[, vars]))
  
  # compute pair-wise dissimilarities using our variables of interest
  d <- daisy(h[no.na.idx, vars], stand=stand, metric=metric)
  
  # fudge-factor in case of duplicate data (0s in the dissimilarity matrix)
  fudge <- min(d) / 100
  
  # perform non-metric MDS of dissimilarity matrix
  mds <- isoMDS(d + fudge, trace=trace)
  
  # compute silhouette widths after removing not-used genhz class
  sil.idx <-  which(complete.cases(h[, vars]) & h[[genhz]] != non.matching.code)
  d.sil <- daisy(h[sil.idx, vars], stand=stand)
  sil <- silhouette(as.numeric(h[[genhz]])[sil.idx], d.sil)
    
  # add new columns
  h$mds.1 <- NA
  h$mds.2 <- NA
  h$sil.width <- NA
  h$neighbor <- NA
  
  # copy values
  h$mds.1[no.na.idx] <- mds$points[, 1]
  h$mds.2[no.na.idx] <- mds$points[, 2]
  h$sil.width[sil.idx] <- sil[, 3]
  h$neighbor[sil.idx] <- levels(h[[genhz]])[sil[, 2]]
  
  # melt into long form
  m <- melt(h, id.vars=genhz, measure.vars=c(vars, 'sil.width'))
  
  # compute group-wise summaries-- note that text is returned
  m.summary <- ddply(m, c(genhz, 'variable'), summarize, 
                     stats=format(paste0(round(mean(value, na.rm=TRUE), 2), ' (' , sd=round(sd(value, na.rm=TRUE), 2), ')'), justify='right')
                     )
  fm <- paste0(genhz, ' ~ variable')
  genhz.stats <- cast(m.summary, fm, value='stats')
  
  # composite into a list
  res <- list(horizons=h[, c('mds.1', 'mds.2', 'sil.width', 'neighbor')], stats=genhz.stats, dist=d)
  return(res)
}
