\name{estimateSTR}
\alias{estimateSTR}

\title{Estimate Soil Temperature Regime}
\description{Estimate soil temperature regime (STR) based on mean annual soil temperature (MAST), mean summer temperature (MSST), mean winter soil temperature (MWST), presence of O horizons, saturated conditions, and presence of permafrost. Several assumptions are made when O horizon  or saturation are undefined.}
\usage{
estimateSTR(mast, mean.summer, mean.winter, O.hz = NA, saturated = NA, permafrost = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mast}{
%%     ~~Describe \code{mast} here~~
}
  \item{mean.summer}{
%%     ~~Describe \code{mean.summer} here~~
}
  \item{mean.winter}{
%%     ~~Describe \code{mean.winter} here~~
}
  \item{O.hz}{
%%     ~~Describe \code{O.hz} here~~
}
  \item{saturated}{
%%     ~~Describe \code{saturated} here~~
}
  \item{permafrost}{
%%     ~~Describe \code{permafrost} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{STRplot}}
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (mast, mean.summer, mean.winter, O.hz = NA, saturated = NA, 
    permafrost = FALSE) 
{
    if (!all.equal(length(mast), length(mean.summer), length(mean.winter))) 
        stop("inputs must all have the same length", call. = TRUE)
    n <- length(mast)
    res <- vector(mode = "character", length = n)
    for (i in seq_along(mast)) {
        if (any(is.na(c(mast[i], mean.summer[i], mean.winter[i])))) {
            res[i] <- NA
            next
        }
        if (mast[i] <= 0) {
            res[i] <- "gelic"
            next
        }
        if (mast[i] <= 1 & permafrost) {
            res[i] <- "gelic"
            next
        }
        if (mast[i] < 8) {
            if (!is.na(saturated[i]) & !is.na(O.hz[i])) {
                if (!saturated[i]) {
                  if (!O.hz[i]) {
                    if (mean.summer[i] > 0 & mean.summer[i] < 
                      15) {
                      res[i] <- "cryic"
                      next
                    }
                  }
                  if (O.hz[i]) {
                    if (mean.summer[i] > 0 & mean.summer[i] < 
                      8) {
                      res[i] <- "cryic"
                      next
                    }
                  }
                }
                if (saturated[i]) {
                  if (!O.hz[i]) {
                    if (mean.summer[i] > 0 & mean.summer[i] < 
                      13) {
                      res[i] <- "cryic"
                      next
                    }
                  }
                  if (O.hz[i]) {
                    if (mean.summer[i] > 0 & mean.summer[i] < 
                      6) {
                      res[i] <- "cryic"
                      next
                    }
                  }
                }
            }
            if (!is.na(saturated[i]) & is.na(O.hz[i])) {
                if (!saturated[i]) {
                  if (mean.summer[i] > 0 & mean.summer[i] < 11.5) {
                    res[i] <- "cryic"
                    next
                  }
                }
                if (saturated[i]) {
                  if (mean.summer[i] > 0 & mean.summer[i] < 9.5) {
                    res[i] <- "cryic"
                    next
                  }
                }
            }
            if (!is.na(O.hz[i]) & is.na(saturated[i])) {
                if (!O.hz[i]) {
                  if (mean.summer[i] > 0 & mean.summer[i] < 14) {
                    res[i] <- "cryic"
                    next
                  }
                }
                if (O.hz[i]) {
                  if (mean.summer[i] > 0 & mean.summer[i] < 7) {
                    res[i] <- "cryic"
                    next
                  }
                }
            }
            if (is.na(saturated[i] & is.na(O.hz[i]))) {
                if (mean.summer[i] < 13) {
                  res[i] <- "cryic"
                  next
                }
            }
        }
        if (mast[i] > 0 & mast[i] < 8) {
            if (mean.summer[i] - mean.winter[i] >= 6) {
                res[i] <- "frigid"
                next
            }
            else {
                res[i] <- "isofrigid"
                next
            }
        }
        if (mast[i] >= 8 & mast[i] < 15) {
            if (mean.summer[i] - mean.winter[i] >= 6) {
                res[i] <- "mesic"
                next
            }
            else {
                res[i] <- "isomesic"
                next
            }
        }
        if (mast[i] >= 15 & mast[i] < 22) {
            if (mean.summer[i] - mean.winter[i] >= 6) {
                res[i] <- "thermic"
                next
            }
            else {
                res[i] <- "isothermic"
                next
            }
        }
        if (mast[i] >= 22) {
            if (mean.summer[i] - mean.winter[i] >= 6) {
                res[i] <- "hyperthermic"
                next
            }
            else {
                res[i] <- "isohyperthermic"
                next
            }
        }
        res[i] <- NA
    }
    res <- factor(res, levels = c("gelic", "cryic", "frigid", 
        "isofrigid", "mesic", "isomesic", "thermic", "isothermic", 
        "hyperthermic", "isohyperthermic"))
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
