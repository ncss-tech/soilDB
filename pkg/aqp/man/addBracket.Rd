\name{addBracket}
\alias{addBracket}
\alias{addDiagnosticBracket}
\title{Add Depth Brackets}
\description{Add depth-wise brackets to an existing plot of a SoilProfileCollection object.}
\usage{
addBracket(top, bottom, idx=NULL, tick.length = 0.05, 
arrow.length = 0.05, offset = -0.3, missing.bottom.depth = 25, 
...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{top}{numeric vector of bracket top depths}
  \item{bottom}{numeric vector of bracket bottom depths}
  \item{idx}{optional integer index, associating bracket with profile}
  \item{tick.length}{length of bracket "tick" mark}
  \item{arrow.length}{length of arrowhead}
  \item{offset}{numeric, length of left-hand offset from each profile}
  \item{missing.bottom.depth}{distance (in depth units) to extend brackets that are missing a lower depth}
  \item{\dots}{further arguments passed on to \code{segments} or \code{arrows}}
}
\details{The optional argument \code{idx} can be used to manually specify which profile a given bracket will be associated with. When \code{idx} is NULL, an integer sequence associated with plotting order (via \code{plotSPC}) is used. See examples below.}

\note{This is a `low-level` plotting function: you must first plot a \code{SoilProfileCollection} object before using this function.}

\author{D.E. Beaudette}

\seealso{\code{\link{plotSPC}}}
\examples{
# sample data
data(sp1)

# add color vector
sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))

# promote to SoilProfileCollection
depths(sp1) <- id ~ top + bottom

# plot profiles
plot(sp1)

# extract top/bottom depths associated with all A horizons
f <- function(i) {
  h <- horizons(i)
  idx <- grep('^A', h$name)
  c(min(h$top[idx]), max(h$bottom[idx], na.rm=TRUE))
}

# apply function to sp1, result is a list
a <- profileApply(sp1, f, simplify=FALSE)
# convert list into matrix
a <- do.call('rbind', a)

# plot
plot(sp1)
# annotate with brackets
# note that plotting order is derived from the call to `plot(sp1)`
addBracket(a[, 1], a[, 2], col='red')

# more interesting example using diagnostic horizons
if(require(soilDB)) {
  # load some sample data with diagnostic horizons
  data(loafercreek)
  
  # extract first 15 profiles
  x <- loafercreek[1:15, ]
  s <- site(x)

  # plot
  par(mar=c(0,0,0,0))
  plot(x, name='hzname', id.style='top')
  
  # add brackets describing the argillic horizon
  addDiagnosticBracket(x, 'argillic horizon', col='red')
  # add brackets describing paralithic contact
  addDiagnosticBracket(x, 'paralithic contact', col='blue')
}

}
