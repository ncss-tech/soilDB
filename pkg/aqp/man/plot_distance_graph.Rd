\name{plot_distance_graph}
\Rdversion{1.1}
\alias{plot_distance_graph}
\title{Between Individual Distance Plot}
\description{Plot pair-wisee distances between individuals as line segments.}
\usage{
plot_distance_graph(D, idx = 1:dim(as.matrix((D)))[1])
}

\arguments{
  \item{D}{distance matrix, should be of class 'dist' or compatible class}
  \item{idx}{an integer sequence defining which individuals should be compared}
}

\details{By default all individuals are plotting on the same axis. When there are more than about 10 individuals, the plot can become quite messy. See examples below for ideas.}

\value{No value is returned.}
\references{http://casoilresource.lawr.ucdavis.edu/}
\author{Dylan E Beaudette}


\seealso{\code{\link{sp2}}, \code{\link{profile_compare}}}

\examples{
data(sp2)

d <- profile_compare(sp2, vars=c('prop','field_ph','hue','value'), 
max_d=100, k=0.01, sample_interval=5)

par(mfcol=c(3,1), mar=c(2.5,4.5,1,1))
plot_distance_graph(d, idx=1:6)
plot_distance_graph(d, idx=7:12)
plot_distance_graph(d, idx=12:18)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
