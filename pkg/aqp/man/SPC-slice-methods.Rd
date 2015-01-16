\name{slice-methods}
\docType{methods}
\alias{get.slice}
\alias{slice.fast}
\alias{slice}
\alias{slice,SoilProfileCollection-method}
\title{Slicing of SoilProfilecollection Objects}
\description{Slicing of SoilProfilecollection Objects}


\usage{
# method for SoilProfileCollection objects
slice(object, fm, top.down=TRUE, just.the.data=FALSE, strict=TRUE)
}

\arguments{
  \item{object}{a SoilProfileCollection}
  
  \item{fm}{A formula: either `integer.vector ~ var1 + var2 + var3' where named variables are sliced according to `integer.vector' OR where all variables are sliced accordin to `integer.vector' `integer.vector ~.'.}
  
  \item{top.down}{Logical, should slices be defined from the top-down? The default is usually what you want.}
  
  \item{just.the.data}{Logical, return just the sliced data or a new SoilProfileCollection object.}
  
  \item{strict}{Logical, should the horizonation be strictly checked for self-consistency?}
}


\section{Methods}{
\describe{
\item{data = "SoilProfileCollection"}{Typical usage, where input is a \code{\link{SoilProfileCollection}}.}
}
}

\note{\code{slab()} and \code{slice()} are much faster and require less memory if input data are either numeric or character.}

\value{Either a new SoilProfileCollection with data sliced according to \code{fm}, or a \code{data.frame}.}

\references{
D.E. Beaudette, P. Roudier, A.T. O'Geen, Algorithms for quantitative pedology: A toolkit for soil scientists, Computers & Geosciences, Volume 52, March 2013, Pages 258-268, 10.1016/j.cageo.2012.10.020.
}
\author{D.E. Beaudette}

\seealso{\code{\link{slab}}}

\examples{
# simulate some data, IDs are 1:20
library(plyr)
d <- ldply(1:20, random_profile)

# init SoilProfilecollection object
depths(d) <- id ~ top + bottom
head(horizons(d))

# generate single slice at 10 cm
# output is a SoilProfilecollection object
s <- slice(d, 10 ~ name + p1 + p2 + p3)

# generate single slice at 10 cm, output data.frame
s <- slice(d, 10 ~ name + p1 + p2 + p3, just.the.data=TRUE)

# generate integer slices from 0 - 25 cm
s <- slice(d, 0:25 ~ name + p1 + p2 + p3)
plot(s)

# generate slices from 0 - 10 cm, for all variables
s <- slice(d, 0:10 ~ .)
print(s)

# note that pct missing is computed for each slice,
# if all vars are missing, then NA is returned
d$p1[1:10] <- NA
s <- slice(d, 10 ~ ., just.the.data=TRUE)
print(s)

## 
## check sliced data
##

# test that mean of 1 cm slices property is equal to the 
# hz-thickness weighted mean value of that property
data(sp1)
depths(sp1) <- id ~ top + bottom

# get the first profile
sp1.sub <- sp1[which(profile_id(sp1) == 'P009'), ]

# compute hz-thickness wt. mean
hz.wt.mean <- with(horizons(sp1.sub), 
sum((bottom - top) * prop) / sum(bottom - top) 
)

# hopefully the same value, calculated via slice()
s <- slice(sp1.sub, 0:max(sp1.sub) ~ prop)
hz.slice.mean <- mean(s$prop, na.rm=TRUE)

# same?
if(!all.equal(hz.slice.mean, hz.wt.mean))
	stop('there is a bug in slice() !!!')


}
\keyword{methods}
\keyword{manip}
