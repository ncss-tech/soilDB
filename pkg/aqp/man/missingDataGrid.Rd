\name{missingDataGrid}
\alias{missingDataGrid}

\title{Missing Data Grid}
\description{Generate a levelplot of missing data from a SoilProfileCollection object.}
\usage{
missingDataGrid(s, max_depth, vars, filter.column = NULL, 
filter.regex = NULL, cols = NULL, ...)
}

\arguments{
  \item{s}{a SoilProfilecollection object}
  \item{max_depth}{integer specifying the max depth of analysis}
  \item{vars}{character vector of column names over which to evaluate missing data}
  \item{filter.column}{a character string naming the column to apply the filter REGEX to}
  \item{filter.regex}{a character string with a regular expression used to filter horizon data OUT of the analysis}
  \item{cols}{a vector of colors}
  \item{\dots}{additional arguments passed on to \code{levelplot}}
}

\details{This function evaluates a `missing data fraction` based on slice-wise evaulation of named variables in a \code{SoilProfileCollection} object.}
\value{A \code{data.frame} describing the percentage of missing data by variable.}
\note{A lattice graphic is printed to the active output device.}
\author{D.E. Beaudette}

\seealso{\code{\link{slice}}}
\examples{
## visualizing missing data
# 10 random profiles
require(plyr)
s <- ldply(1:10, random_profile)

# randomly sprinkle some missing data
s[sample(nrow(s), 5), 'p1'] <- NA
s[sample(nrow(s), 5), 'p2'] <- NA
s[sample(nrow(s), 5), 'p3'] <- NA

# set all p4 and p5 attributes of `soil 1' to NA
s[which(s$id == '1'), 'p5'] <- NA
s[which(s$id == '1'), 'p4'] <- NA

# upgrade to SPC
depths(s) <- id ~ top + bottom

# plot missing data via slicing + levelplot
missingDataGrid(s, max_depth=100, vars=c('p1', 'p2', 'p3', 'p4', 'p5'), 
main='Missing Data Fraction')
}

\keyword{hplots}
