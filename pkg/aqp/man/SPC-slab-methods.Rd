\name{slab-methods}
\docType{methods}
\alias{slab}
\alias{slab2}
\alias{slab,SoilProfileCollection-method}


\title{Slab-Wise Aggregation of SoilProfileCollection Objects}
\description{Aggregate soil properties along user-defined `slabs`, and optionally within groups.}

\usage{
# method for SoilProfileCollection objects
slab(object, fm, slab.structure=1, strict=FALSE, 
slab.fun=.slab.fun.numeric.default, cpm=1, weights=NULL, ...)
}

\arguments{
  \item{object}{a SoilProfileCollection}
  
  \item{fm}{A formula: either `groups ~ var1 + var2 + var3' where named variables are aggregated within `groups' OR where named variables are aggregated across the entire collection ` ~ var1 + var2 + var3'. If `groups` is a factor it must not contain NA.}
  
  \item{slab.structure}{A user-defined slab thickness (defined by an integer), or user-defined structure (numeric vector). See details below.}
  
  \item{strict}{logical: should horizons be strictly checked for self-consistency?}
  
  \item{slab.fun}{Function used to process each 'slab' of data, ideally returning a vector with names attribute. Defaults to a wrapper function around \code{hdquantile}. See details.}
  
  \item{cpm}{Strategy for normalizing slice-wise probabilities, dividing by either: number of profiles with data at the current slice (cpm=1), or by the number of profiles in the collection (cpm=2). Mode 2 values will always sum to the contributing fraction, while mode 1 values will always sum to 1.}
  
  \item{weights}{Column name containing weights. NOT YET IMPLEMENTED}

	\item{\dots}{further arguments passsed to \code{slab.fun}}

}


\section{Methods}{
\describe{
\item{data = "SoilProfileCollection"}{Typical usage, where input is a \code{\link{SoilProfileCollection}}.}
}
}

\details{
Multiple continuous variables OR a single categorical (factor) variable can be aggregated within a call to \code{slab}. Basic error checking is performed to make sure that top and bottom horizon boundaries make sense. User-defined aggregate functions (\code{slab.fun}) should return a named vector of results. A new, named column will appear in the results of \code{slab} for every named element of a vector returned by \code{slab.fun}. See examples below for a simple example of a slab function that computes mean, mean-1SD and mean+1SD. The default slab function wraps \code{\link{hdquantile}} from the Hmisc package, which requires at least 2 observations per chunk. Note that if `group` is a factor it must not contain NAs.

Execution time scales linearly (slower) with the total number of profiles in \code{object}, and exponentially (faster) as the number of profiles / group is increased. \code{slab()} and \code{slice()} are much faster and require less memory if input data are either numeric or character.

There are several possible ways to define slabs, using \code{slab.structure}:

\describe{
	\item{a single integer}{e.g. 10: data are aggregated over a regular sequence of 10-unit thickness slabs}
	\item{a vector of 2 integers}{e.g. c(50, 60): data are aggregated over depths spanning 50--60 units}
	\item{a vector of 3 or more integers}{e.g. c(0, 5, 10, 50, 100): data are aggregated over the depths spanning 0--5, 5--10, 10--50, 50--100 units}
	} 
}

\value{
Output is returned in long format, such that slice-wise aggregates are returned once for each combination of grouping level (optional), variable described in the \code{fm} argument, and depth-wise 'slab'.

Aggregation of numeric variables, using the default slab function:
	\describe{
		\item{variable}{The names of variables included in the call to \code{slab}.}
		\item{groupname}{The name of the grouping variable when provided, otherwise a fake grouping variable named 'all.profiles'.}
		\item{p.q5}{The slice-wise 5th percentile.}
		\item{p.q25}{The slice-wise 25th percentile}
		\item{p.q50}{The slice-wise 50th percentile (median)}
		\item{p.q75}{The slice-wise 75th percentile}
		\item{p.q95}{The slice-wise 95th percentile}
		\item{top}{The slab top boundary.}
		\item{bottom}{The slab bottom boundary.}
		\item{contributing_fraction}{The fraction of profiles contributing to the aggregate value, ranges from 1/n_profiles to 1.}
		}

When a single factor variable is used, slice-wise probabilities for each level of that factor are returned as:
	\describe{
		\item{variable}{The names of variables included in the call to \code{slab}.}
		\item{groupname}{The name of the grouping variable when provided, otherwise a fake grouping variable named 'all.profiles'.}
		\item{A}{The slice-wise probability of level A}
		\item{B}{The slice-wise probability of level B}
		\item{\dots}{}
		\item{n}{The slice-wise probability of level n}
		\item{top}{The slab top boundary.}
		\item{bottom}{The slab bottom boundary.}
		\item{contributing_fraction}{The fraction of profiles contributing to the aggregate value, ranges from 1/n_profiles to 1.}

	}
}

\references{
D.E. Beaudette, P. Roudier, A.T. O'Geen, Algorithms for quantitative pedology: A toolkit for soil scientists, Computers & Geosciences, Volume 52, March 2013, Pages 258-268, 10.1016/j.cageo.2012.10.020.

Harrell FE, Davis CE (1982): A new distribution-free quantile estimator. Biometrika 69:635-640.
}
\author{D.E. Beaudette}

\note{Arguments to \code{slab} have changed with \code{aqp} 1.5 (2012-12-29) as part of a code clean-up and optimization. Calculation of weighted-summaries was broken in \code{aqp} 1.2-6 (2012-06-26), and removed as of \code{aqp} 1.5 (2012-12-29).  \code{slab} replaced the previously defined \code{soil.slot.multiple} function as of \code{aqp} 0.98-8.58 (2011-12-21).}


\seealso{
\code{\link{slice}, \link{hdquantile}}
}
\examples{
##
## basic examples
##
library(lattice)
library(grid)

# load sample data, upgrade to SoilProfileCollection
data(sp1)
depths(sp1) <- id ~ top + bottom

# aggregate entire collection with two different segment sizes
a <- slab(sp1, fm = ~ prop)
b <- slab(sp1, fm = ~ prop, slab.structure=5)

# check output
str(a)

# stack into long format
ab <- make.groups(a, b)
ab$which <- factor(ab$which, levels=c('a','b'), 
labels=c('1-cm Interval', '5-cm Interval'))

# plot median and IQR
# custom plotting function for uncertainty viz.
xyplot(top ~ p.q50 | which, data=ab, ylab='Depth',
			 xlab='median bounded by 25th and 75th percentiles',
			 lower=ab$p.q25, upper=ab$p.q75, ylim=c(250,-5),
			 panel=panel.depth_function, 
			 prepanel=prepanel.depth_function,
			 cf=ab$contributing_fraction,
			 layout=c(2,1), scales=list(x=list(alternating=1))
			 )


##
## categorical variable example
##
library(reshape)

# normalize horizon names: result is a factor
sp1$name <- generalize.hz(sp1$name, 
new=c('O','A','B','C'), 
pat=c('O', '^A','^B','C'))

# compute slice-wise probability so that it sums to contributing fraction, from 0-150
a <- slab(sp1, fm= ~ name, cpm=1, slab.structure=0:150)

# reshape into long format for plotting
a.long <- melt(a, id.vars=c('top','bottom'), measure.vars=c('O','A','B','C'))

# plot horizon type proportions using panels
xyplot(top ~ value | variable, data=a.long, subset=value > 0,
			 ylim=c(150, -5), type=c('S','g'), horizontal=TRUE, layout=c(4,1), col=1 )

# again, this time using groups
xyplot(top ~ value, data=a.long, groups=variable, subset=value > 0,
			 ylim=c(150, -5), type=c('S','g'), horizontal=TRUE, asp=2)

# adjust probability to size of collection, from 0-150
a.1 <- slab(sp1, fm= ~ name, cpm=2, slab.structure=0:150)

# reshape into long format for plotting
a.1.long <- melt(a.1, id.vars=c('top','bottom'), measure.vars=c('O','A','B','C'))

# group mode 1 and mode 2 data
g <- make.groups(mode_1=a.long, mode_2=a.1.long)

# plot horizon type proportions
xyplot(top ~ value | variable, groups=which, data=g, subset=value > 0,
			 ylim=c(240, -5), type=c('S','g'), horizontal=TRUE, layout=c(4,1), 
			 auto.key=list(lines=TRUE, points=FALSE, columns=2),
			 par.settings=list(superpose.line=list(col=c(1,2))))


# apply slice-wise evaluation of max probability, and assign ML-horizon at each slice
(gen.hz.ml <- get.ml.hz(a, c('O','A','B','C')))

\dontrun{
##
## multivariate examples
##
data(sp3)

# add new grouping factor
sp3$group <- 'group 1'
sp3$group[as.numeric(sp3$id) > 5] <- 'group 2'
sp3$group <- factor(sp3$group)

# upgrade to SPC
depths(sp3) <- id ~ top + bottom
site(sp3) <- ~ group

# custom 'slab' function, returning mean +/- 1SD
mean.and.sd <- function(values) {
	m <- mean(values, na.rm=TRUE)
	s <- sd(values, na.rm=TRUE)
	upper <- m + s
	lower <- m - s
	res <- c(mean=m, lower=lower, upper=upper)
	return(res)
	}

# aggregate several variables at once, within 'group'
a <- slab(sp3, fm=group ~ L + A + B, slab.fun=mean.and.sd)

# check the results:
# note that 'group' is the column containing group labels
library(lattice)
xyplot(
	top ~ mean | variable, data=a, groups=group,
	lower=a$lower, upper=a$upper, sync.colors=TRUE, alpha=0.5,
	cf=a$contributing_fraction,
	ylim=c(125,-5), layout=c(3,1), scales=list(x=list(relation='free')),
	par.settings=list(superpose.line=list(lwd=2, col=c('RoyalBlue', 'Orange2'))),
	panel=panel.depth_function, 
	prepanel=prepanel.depth_function,
	auto.key=list(columns=2, lines=TRUE, points=FALSE)
)


# compare a single profile to the group-level aggregate values
a.1 <- slab(sp3[1, ], fm=group ~ L + A + B, slab.fun=mean.and.sd)

# manually update the group column
a.1$group <- 'profile 1'

# combine into a single data.frame:
g <- rbind(a, a.1)

# plot with customized line styles
xyplot(
	top ~ mean | variable, data=g, groups=group, subscripts=TRUE, 
	lower=a$lower, upper=a$upper, ylim=c(125,-5),
	layout=c(3,1), scales=list(x=list(relation='free')),
	panel=panel.depth_function, 
	prepanel=prepanel.depth_function,
	sync.colors=TRUE, alpha=0.25,
	par.settings=list(superpose.line=list(col=c('orange', 'royalblue', 'black'), 
  lwd=2, lty=c(1,1,2))),
	auto.key=list(columns=3, lines=TRUE, points=FALSE)
)



## convert mean value for each variable into long format
library(reshape)

# note that depths are no longer in order 
a.wide <- cast(a, group + top + bottom ~ variable, value=c('mean'))

## again, this time for a user-defined slab from 40-60 cm
a <- slab(sp3, fm=group ~ L + A + B, slab.structure=c(40,60), slab.fun=mean.and.sd)

# now we have weighted average properties (within the defined slab)
# for each variable, and each group
(a.wide <- cast(a, group + top + bottom ~ variable, value=c('mean')))

## this time, compute the weighted mean of selected properties, by profile ID
a <- slab(sp3, fm= id ~ L + A + B, slab.structure=c(40,60), slab.fun=mean.and.sd)
(a.wide <- cast(a, id + top + bottom ~ variable, value=c('mean')))


## aggregate the entire collection, using default slab function (hdquantile)
## note the missing left-hand side of the formula
a <- slab(sp3, fm= ~ L + A + B)


## weighted-aggregation -- NOT YET IMPLEMENTED --
# load sample data, upgrade to SoilProfileCollection
data(sp1)
depths(sp1) <- id ~ top + bottom

# generate pretend weights as site-level attribute
set.seed(10101)
sp1$site.wts <- runif(n=length(sp1), min=20, max=100)
}
}

\keyword{methods}
\keyword{manip}
