\name{random_profile}
\alias{random_profile}
\alias{.lpp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Random Profile}
\description{
Generate a random soil profile according to set criteria, with correlated depth trends.
}
\usage{
random_profile(id, n = c(3, 4, 5, 6), min_thick = 5,
 max_thick = 30, n_prop = 5, exact = FALSE, method = 'random_walk', 
 HzDistinctSim=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{a character or numeric id used for this profile}
  \item{n}{vector of possible number of horizons, or the exact number of horizons (see below)}
  \item{min_thick}{minimum thickness criteria for a simulated horizon}
  \item{max_thick}{maximum thickness criteria for a simulated horizon}
  \item{n_prop}{number of simulated soil properties (columns in the returned dataframe)}
  \item{exact}{should the exact number of requested horizons be generated? (defaults to FALSE)}
  \item{method}{named method used to synthesize depth function ('random_walk' or 'LPP'), see details}
  \item{HzDistinctSim}{optionally simulate horizon boundary distinctness codes}
  \item{\dots}{additional parameters passed-in to the LPP (\code{.lpp}) function}
}

\value{
A dataframe with the simulated profile.
}
\details{The random walk method produces profiles with considerable variation between horizons and is based on values from the normal distribution seeded with means and standard deviations drawn from the uniform distribtion of [0, 10].

The logistic power peak (LPP) function can be used to generate random soil property depth functions that are sharply peaked. LPP parameters can be hard-coded using the optional arguments: "lpp.a", "lpp.b", "lpp.u", "lpp.d", "lpp.e". Amplitude of the peak is controlled by ("lpp.a + "lpp.b"), depth of the peak by "lpp.u", and abruptness by "lpp.d" and "lpp.e". Further description of the method is outlined in (Brenton et al, 2011). Simulated horizon distinctness codes are based on the USDA-NCSS field description methods (\url{http://soils.usda.gov/technical/fieldbook/}). Simulated distinctness codes are constrained according to horizon thickness, i.e. a gradual boundary (+/- 5cm) will not be simulated for horizons that are thinner than 3x this vertical distance
}
\references{
Myers, D. B.; Kitchen, N. R.; Sudduth, K. A.; Miles, R. J.; Sadler, E. J. & Grunwald, S. Peak functions for modeling high resolution soil profile data Geoderma, 2011, 166, 74-83.
}
\author{Dylan E. Beaudette}
\note{See examples for ideas on simulating several profiles at once.}
\seealso{\code{\link{profile_compare}, \link{hzDistinctnessCodeToOffset}}}
\examples{
# generate 10 random profiles with default settings:
require(plyr)
d <- ldply(1:10, random_profile)

# add a fake color
d$soil_color <- 'white'

# promote to SoilProfileCollection and plot
depths(d) <- id ~ top + bottom
plot(d)


# simulate horizon boundary distinctness codes:
d <- ldply(1:10, random_profile, HzDistinctSim=TRUE)
d$soil_color <- grey(0.85)
depths(d) <- id ~ top + bottom
d$HzD <- hzDistinctnessCodeToOffset(d$HzDistinctCode)
plot(d, hz.distinctness.offset='HzD')



# depth functions are generated using the LPP function
opar <- par(mfrow=c(2,1), mar=c(0,0,3,0))
# generate data
d <- ldply(1:10, random_profile, n=c(6, 7, 8), n_prop=1, method='LPP')

# promote to SPC and plot
depths(d) <- id ~ top + bottom
plot(d, color='p1')

# do this again, this time set all of the LPP parameters
d <- ldply(1:10, random_profile, n=c(6, 7, 8), n_prop=1, method='LPP', 
lpp.a=5, lpp.b=10, lpp.d=5, lpp.e=5, lpp.u=25)

depths(d) <- id ~ top + bottom
plot(d, color='p1')

# reset plotting defaults
par(opar)



# try plotting the LPP-derived simulated data
# aggregated over all profiles
a <- slab(d, fm= ~ p1)
a$mid <- with(a, (top + bottom) / 2)

library(lattice)
(p1 <- xyplot(mid ~ p.q50, data=a,
lower=a$p.q25, upper=a$p.q75, ylim=c(150,-5), alpha=0.5,
panel=panel.depth_function, prepanel=prepanel.depth_function,
cf=a$contributing_fraction, xlab='Simulated Data', ylab='Depth',
main='LPP(a=5, b=10, d=5, e=5, u=25)',
par.settings=list(superpose.line=list(col='black', lwd=2))
))

# optionally add original data as step-functions
if(require(latticeExtra)) {
  h <- horizons(d)
  p1 + as.layer(xyplot(top ~ p1, groups=id, data=h,
  horizontal=TRUE, type='S',
  par.settings=list(superpose.line=list(col='blue', lwd=1, lty=2))))
  }


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
