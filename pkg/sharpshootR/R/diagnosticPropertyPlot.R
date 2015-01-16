## TODO: figure out a better approach for alignment of dendrogram / image axis labels

# f: SPC with diagnostic boolean variables
# v: named variables
# k: number of groups to highlight
# id: id to print next to dendrogram
diagnosticPropertyPlot <- function(f, v, k, id='pedon_id') {
  
  # extract site data
  s <- site(f)
  
  ## TODO: why would there be NA in here?
  # filter NA
  no.na.idx <- which(complete.cases(s[, v]))
  s <- s[no.na.idx, ]
  
  # save diagnostic properties
  m <- s[, v]
  
  # optionally check for any vars that are all FALSE and kick them out
  vars.not.missing <- apply(m, 2, any)
  
  # if any are all FALSE, then remove from m and v
  if(any(!vars.not.missing)) {
    not.missing <- which(vars.not.missing)
    m <- m[, not.missing]
    v <- v[not.missing]
  }
  
  # convert to factors, we have to specify the levels as there are cases with all TRUE or FALSE
  m <- as.data.frame(lapply(m, factor, levels=c('FALSE', 'TRUE')))
  
  # make a copy of the matrix for plotting, as numerical data and transpose
  m.plot <- t(as.matrix(as.data.frame(lapply(m, as.numeric))))
  
  # compute dissimilarity between profiles
  d <- daisy(m, metric='gower')
  h.profiles <- as.hclust(diana(d))
  h.profiles$labels <- s[[id]]
  p <- as.phylo(h.profiles)
  
  # cut tree at user-specified number of groups
  h.cut <- cutree(h.profiles, k=k)
  
  # setup plot layout
  layout(matrix(c(1,2), nrow=1, ncol=2), widths=c(1,1))
  
  # get number of vars + number of profiles
  n.vars <- ncol(m)
  n.profiles <- nrow(m)
    
  # plot profile dendrogram
  par(mar=c(1,1,6,1))
  plot(p, cex=0.75, label.offset=0.05, y.lim=c(1.125, n.profiles))
  tiplabels(pch=15, col=h.cut, cex=1.125, adj=0.52)
  
  # compute dissimilarity between variables
  d.vars <- daisy(as.data.frame(t(m)), metric='gower')
  h.vars <- as.hclust(diana(d.vars))
  
  # order of profiles in dendrogram
  o.profiles <- h.profiles$order
  
  # vector of variable names as plotted in dendrogram
  o.vars <- h.vars$order
  
  # plot image matrix, with rows re-ordered according to dendrogram
  par(mar=c(1,6,6,1))
  image(x=1:n.vars, y=1:n.profiles, z=m.plot[o.vars, o.profiles], axes=FALSE, col=c(grey(0.9), 'RoyalBlue'), xlab='', ylab='', ylim=c(0.5, n.profiles+0.5))
  axis(side=2, at=1:n.profiles, labels=s$pedon_id[o.profiles], las=1, cex.axis=0.75)
  axis(side=3, at=1:n.vars, labels=v[o.vars], las=2, cex.axis=0.75)
  abline(h=1:(n.profiles+1)-0.5)
  abline(v=1:(n.vars+1)-0.5)
  
  ## TODO: back-fill with any missing values?
  # return values
  rd <- cbind(s[, c('peiid', id)], g=h.cut)
  return(invisible(list(rd=rd, profile.order=o.profiles, var.order=o.vars)))
}


# this will break when using a non-unique ID
diagnosticPropertyPlot2 <- function(f, v, k, id='pedon_id') {
    
  # extract site data
  s <- site(f)
  
  ## TODO: why would there be NA in here?
  # filter NA
  no.na.idx <- which(complete.cases(s[, v]))
  s <- s[no.na.idx, ]
  
  # save diagnostic properties
  m <- s[, v]
  
  # optionally check for any vars that are all FALSE and kick them out
  vars.not.missing <- apply(m, 2, any)
  
  # if any are all FALSE, then remove from m and v
  if(any(!vars.not.missing)) {
    not.missing <- which(vars.not.missing)
    m <- m[, not.missing]
    v <- v[not.missing]
  }
  
  # convert to factors, we have to specify the levels as there are cases with all TRUE or FALSE
  m <- as.data.frame(lapply(m, factor, levels=c('FALSE', 'TRUE')))
  
  # get number of vars + number of profiles
  n.vars <- ncol(m)
  n.profiles <- nrow(m)
  
  # compute dissimilarity between profiles
  d <- daisy(m, metric='gower')
  h.profiles <- as.hclust(diana(d))
  
  # compute dissimilarity between variables
  d.vars <- daisy(as.data.frame(t(m)), metric='gower')
  h.vars <- as.hclust(diana(d.vars))
  
  # cut tree at user-specified number of groups
  h.cut <- cutree(h.profiles, k=k)
  
  # format for plotting
  m.plot <- data.frame(id=s[[id]], m, stringsAsFactors=FALSE)
  m.plot.long <- melt(m.plot, id.vars='id')
  # convert TRUE/FALSE into factor
  m.plot.long$value <- factor(m.plot.long$value, levels=c('FALSE', 'TRUE'))
  
  # order of profiles in dendrogram
  o.profiles <- h.profiles$order
  
  # vector of variable names as plotted in dendrogram
  o.vars <- h.vars$order
  
  # set factor levels for ordering of level plot
  m.plot.long$id <- factor(m.plot.long$id, levels=m.plot$id[o.profiles])
  m.plot.long$variable <- factor(m.plot.long$variable, levels=v[o.vars])
  
  # lattice plot
  p <- levelplot(value ~ variable * id, data=m.plot.long,
  col.regions=c(grey(0.9), 'RoyalBlue'), cuts=1, xlab='', ylab='', 
  colorkey = FALSE, 
  scales=list(tck=0, x=list(rot=90)),
  legend=list(
      right=list(fun=dendrogramGrob, args=list(x = as.dendrogram(h.profiles), side="right", size=15, add=list(
        rect=list(fill=h.cut, cex=0.5)))),
      top=list(fun=dendrogramGrob, args=list(x=as.dendrogram(h.vars), side="top", size=4))
      ),
  panel=function(...) {
    panel.levelplot(...)
    # horizontal lines
    panel.segments(x0=0.5, y0=1:(n.profiles+1)-0.5, x1=n.vars+0.5, y1=1:(n.profiles+1)-0.5)
    # vertical lines
    panel.segments(x0=1:(n.vars+1)-0.5, y0=0.5, x1=1:(n.vars+1)-0.5, y1=n.profiles + 0.5)
  }
  )
  
  # print to graphics device
  print(p)
  
  ## TODO: back-fill with any missing values?
  # return values
  rd <- cbind(s[, c('peiid', id)], g=h.cut)
  return(invisible(list(rd=rd, profile.order=o.profiles, var.order=o.vars)))
}


