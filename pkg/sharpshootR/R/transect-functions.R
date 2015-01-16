
# function for computing gradient vs. distance along gradient
dist.along.grad <- function(coords, var, grad.scaled.min, grad.scaled.max) {
  # order points along gradient
  grad.order <- order(var)
  # order coordinates by variable
  coords <- coords[grad.order, ]
  # compute cumulative distance along gradient
  grad.distances <- cumsum(c(0, sqrt(diff(coords[, 1])^2 + diff(coords[, 2])^2)))
  # rescale distances to number of profiles in collection
  scaled.distances <- scales::rescale(grad.distances, to=c(0.5, nrow(coords)+0.5))
  # rescale gradients to profile-scale
  # note that range is inverted because we are plotting depth as positive values
  scaled.grad <- scales::rescale(var[grad.order], to=c(grad.scaled.max, grad.scaled.min))
  # composite result
  res <- data.frame(scaled.grad=scaled.grad, scaled.distance=scaled.distances, distance=grad.distances, variable=var[grad.order], x=coords[grad.order, 1], y=coords[grad.order, 2], grad.order=grad.order)
  return(res)
}

# plot a transect with profiles below
plotTransect <- function(s, grad.var.name, transect.col='RoyalBlue', tick.number=7, y.offset=100, scaling.factor=0.5, distance.axis.title='Distance Along Transect (km)', crs=NULL, grad.axis.title=NULL, ...){
  
  # internal offsets
  
  # optionally convert to planar CRS
  if(!missing(crs)) {
    # need rgdal
    if(!require(rgdal))
      stop('Transformation of coordinates requires the `rgdal` package.', call.=FALSE)
    
    # perform transformation and extract coordinates from SPC
    coords <- coordinates(spTransform(as(s, 'SpatialPointsDataFrame'), crs))
  }
  # extract coordinates from SPC without transformation, CRS must be planar
  else
    coords <- coordinates(s)
  
  # create transect
  transect <- dist.along.grad(coords, site(s)[[grad.var.name]], grad.scaled.min=0, grad.scaled.max=y.offset-15)
  
  # use a linear model to translate original gradient -> scaled gradient 
  l <- lm(scaled.grad ~ variable, data=transect)
  
  # establish gradient axis tick marks
  axis.labels <- pretty(transect$variable, n=tick.number)
  axis.pos <- round(predict(l, data.frame(variable=axis.labels)))
    
  # setup basic plot
  plot(s, plot.order=transect$grad.order, y.offset=y.offset, scaling.factor=scaling.factor, id.style='side', ...)
  
  # add gridlines
  abline(h=axis.pos, lty=2, col='grey')
  
  # add gradient axis
  axis(side=2, at=axis.pos, labels=axis.labels, las=2, line=-0.5, cex.axis=0.75)
  
  # optionally label gradient axis
  if(!missing(grad.axis.title))
    mtext(grad.axis.title, at=median(transect$scaled.grad), side=2, line=2.5, font=2, cex=0.75)
  
  # add distance along gradient
  axis(side=1, at=1:length(s), labels=round(transect$distance/1000), cex.axis=0.75)
  mtext(distance.axis.title, side=1, line=2, font=2, cex=0.75)
  
  # link gradient points to profiles
  segments(x0=1:length(s), y0=y.offset-15, x1=transect$scaled.distance, y1=transect$scaled.grad, lty=1)
  
  # add arrows
  arrows(x0=1:length(s), y0=y.offset-15, x1=1:length(s), y1=y.offset-2, length = 0.1, code=2, lty=1)
  
  # add gradient line
  lines(transect$scaled.distance, transect$scaled.grad, lwd=2, col=transect.col)
  
  # add gradient points
  points(transect$scaled.distance, transect$scaled.grad, lwd=2, pch=21, cex=1.25, bg=transect.col, col='black')
  
  # invisibly return computed transect geometry
  invisible(transect)
}
