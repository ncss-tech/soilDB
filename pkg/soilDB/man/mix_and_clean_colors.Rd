\name{mix_and_clean_colors}
\alias{mix_and_clean_colors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mix_and_clean_colors(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x)
  {
  # fill missing weights with 1
  x$pct[is.na(x$pct)] <- 1
  
  # skip horizons with a single color
  tab <- table(x$hz_id)
  if(tab > 1) {
    r <- with(x, wtd.mean(r, weights=pct))
    g <- with(x, wtd.mean(g, weights=pct))
    b <- with(x, wtd.mean(b, weights=pct))
    # composite
    df <- data.frame(r,g,b)
    }
  else
    df <- x[, c('r','g','b')]
  
  # done
  return(df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
