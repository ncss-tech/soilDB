\name{plotTransect}
\alias{plotTransect}

\title{Plot a collection of Soil Profiles linked to their position along some gradient (e.g. transect).}
\description{Plot a collection of Soil Profiles linked to their position along some gradient (e.g. transect).}
\usage{
plotTransect(s, grad.var.name, transect.col = "RoyalBlue", 
tick.number=7, y.offset = 100, 
scaling.factor = 0.5, 
distance.axis.title = "Distance Along Transect (km)", 
crs = NULL, grad.axis.title = NULL, ...)
}

\arguments{
  \item{s}{a \code{SoilProfileCollection} object}
  \item{grad.var.name}{the name of a site-level attribute containing gradient values}
  \item{transect.col}{color used to plot gradient (transect) values}
  \item{tick.number}{number of desired ticks and labels on the gradient axis}
  \item{y.offset}{vertical offset used to position profile sketches}
  \item{scaling.factor}{scaling factor used to adjust profile sketches}
  \item{distance.axis.title}{a tital for the along-transect distances}
  \item{crs}{an optional \code{CRS} object used to convert coordinates into a planar system}
  \item{grad.axis.title}{a title for the gradient axis}
  \item{\dots}{further arguments passed to \code{plotSPC}}
}

\details{Depending on the nature of your SoilProfileCollection and associated gradient values, it may be necessary to tinker with figure margins, \code{y.offset} and \code{scaling.factor}.}
\note{This function is very much a work in progress, ideas welcome!}

\value{An invisibly-returned \code{data.frame} object:
\describe{
  \item{scaled.grad}{scaled gradient values}
  \item{scaled.distance}{cumulative distance, scaled to the interval of \code{0.5, nrow(coords) + 0.5}}
  \item{distance}{cumulative distance computed along gradient, e.g. transect distance}
  \item{variable}{sorted gradient values}
  \item{x}{x coordinates, ordered by gradient values}
  \item{y}{y coordinate, ordered by gradient values}
  \item{grad.order}{a vector index describing the sort order defined by gradient values}
  }}

\author{D.E. Beaudette}


\examples{
\dontrun{
data(loafercreek, package='soilDB')

x <- loafercreek[1:10, ]
coordinates(x) <- ~ x_std + y_std
proj4string(x) <- '+proj=longlat +datum=NAD83'

par(mar=c(4,3,1,1))
plotTransect(x, 'elev_field', crs=CRS('+proj=utm +zone=10 +datum=NAD83'), max.depth=185)
}
}

\keyword{hplots}

