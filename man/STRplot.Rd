\name{STRplot}
\alias{STRplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
STRplot(mast, msst, mwst, permafrost = FALSE, pt.cex = 2.75, leg.cex = 0.85)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mast}{
%%     ~~Describe \code{mast} here~~
}
  \item{msst}{
%%     ~~Describe \code{msst} here~~
}
  \item{mwst}{
%%     ~~Describe \code{mwst} here~~
}
  \item{permafrost}{
%%     ~~Describe \code{permafrost} here~~
}
  \item{pt.cex}{
%%     ~~Describe \code{pt.cex} here~~
}
  \item{leg.cex}{
%%     ~~Describe \code{leg.cex} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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

\seealso{
\code{\link{estimateSTR}}
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (mast, msst, mwst, permafrost = FALSE, pt.cex = 2.75, 
    leg.cex = 0.85) 
{
    .makeRow <- function(x, y, STR, colors, O, S) {
        x.offset <- 0.4
        y.offset <- 0.25
        rect(xleft = x - x.offset, xright = x + x.offset, ybottom = rep(y, 
            times = length(x)) - y.offset, ytop = rep(y, times = length(x)) + 
            y.offset, col = colors[as.numeric(STR)])
        text(min(x) - 1, y, labels = sprintf("\%s | \%s", O, S), 
            cex = 0.75, font = 2, adj = 0.5)
    }
    cols <- c("white", "purple", "blue", "lightblue", "darkgreen", 
        "lightgreen", "orange", "yellow", "brown", "red")
    if (length(mast) > 1 & length(msst) > 1) 
        stop()
    if (length(mast) > 1) {
        x <- mast
        x.label <- "MAST (C)"
        alt.label <- "MSST"
        alt.summary <- msst
        msst <- rep(msst, times = length(mast))
        mwst <- rep(mwst, times = length(mast))
    }
    else if (length(msst) > 1) {
        x <- msst
        x.label <- "MSST (C)"
        alt.label <- "MAST"
        alt.summary <- mast
        mast <- rep(mast, times = length(msst))
        mwst <- rep(mwst, times = length(msst))
    }
    x.1 <- estimateSTR(mast, msst, mwst, O.hz = rep(NA, times = length(mast)), 
        saturated = rep(NA, times = length(mast)), permafrost = permafrost)
    x.2 <- estimateSTR(mast, msst, mwst, O.hz = rep(TRUE, times = length(mast)), 
        saturated = rep(NA, times = length(mast)), permafrost = permafrost)
    x.3 <- estimateSTR(mast, msst, mwst, O.hz = rep(FALSE, times = length(mast)), 
        saturated = rep(NA, times = length(mast)), permafrost = permafrost)
    x.9 <- estimateSTR(mast, msst, mwst, O.hz = rep(FALSE, times = length(mast)), 
        saturated = rep(FALSE, times = length(mast)), permafrost = permafrost)
    x.8 <- estimateSTR(mast, msst, mwst, O.hz = rep(TRUE, times = length(mast)), 
        saturated = rep(FALSE, times = length(mast)), permafrost = permafrost)
    x.7 <- estimateSTR(mast, msst, mwst, O.hz = rep(FALSE, times = length(mast)), 
        saturated = rep(TRUE, times = length(mast)), permafrost = permafrost)
    x.6 <- estimateSTR(mast, msst, mwst, O.hz = rep(TRUE, times = length(mast)), 
        saturated = rep(TRUE, times = length(mast)), permafrost = permafrost)
    x.5 <- estimateSTR(mast, msst, mwst, O.hz = rep(NA, times = length(mast)), 
        saturated = rep(FALSE, times = length(mast)), permafrost = permafrost)
    x.4 <- estimateSTR(mast, msst, mwst, O.hz = rep(NA, times = length(mast)), 
        saturated = rep(TRUE, times = length(mast)), permafrost = permafrost)
    plot(x, rep(1, times = length(x.1)), type = "n", axes = FALSE, 
        ylab = "", xlab = "", xlim = c(min(x) - 0.75, max(x) + 
            0.5), ylim = c(1, 11))
    segments(x0 = x, x1 = x, y0 = 0, y1 = 9.5, lty = 3, col = "grey")
    segments(y0 = (2:9) - 0.4, y1 = (2:9) - 0.4, x0 = min(x) - 
        0.5, x1 = max(x) + 0.5, lty = 1, col = c("black", "grey", 
        "black", "grey"))
    .makeRow(x = x, y = 1, STR = x.1, colors = cols, O = "?", 
        S = "?")
    .makeRow(x = x, y = 2, STR = x.2, colors = cols, O = "O", 
        S = "?")
    .makeRow(x = x, y = 3, STR = x.3, colors = cols, O = "X", 
        S = "?")
    .makeRow(x = x, y = 4, STR = x.4, colors = cols, O = "?", 
        S = "S")
    .makeRow(x = x, y = 5, STR = x.5, colors = cols, O = "?", 
        S = "X")
    .makeRow(x = x, y = 6, STR = x.6, colors = cols, O = "O", 
        S = "S")
    .makeRow(x = x, y = 7, STR = x.7, colors = cols, O = "X", 
        S = "S")
    .makeRow(x = x, y = 8, STR = x.8, colors = cols, O = "O", 
        S = "X")
    .makeRow(x = x, y = 9, STR = x.9, colors = cols, O = "X", 
        S = "X")
    axis(side = 1, at = x, cex.axis = 0.75)
    mtext(x.label, side = 1, line = 2.5, font = 2)
    mtext(sprintf("MWST: \%s (C)", unique(mwst)), side = 1, at = min(x), 
        line = 2.5, adj = 0, font = 2)
    mtext(sprintf("\%s: \%s (C)", alt.label, alt.summary), side = 1, 
        at = max(x) + 0.5, line = 2.5, adj = 1, font = 2)
    mtext("O / Saturation", side = 3, at = min(x) + 0.25, line = -5, 
        adj = 1, font = 3, cex = 0.75)
    legend("top", legend = c("gelic", "cryic", "frigid", "isofrigid", 
        "mesic", "isomesic", "thermic", "isothermic", "hyperthermic", 
        "isohyperthermic"), pt.bg = cols, pch = 22, pt.cex = 2, 
        cex = leg.cex, ncol = 5, inset = 0.05, bty = "n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
