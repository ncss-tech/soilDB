\name{sp3}
\Rdversion{1.1}
\alias{sp3}
\docType{data}
\title{
Soil Profile Data Example 3
}
\description{
Soil samples from 10 soil profiles, taken from the Sierra Foothill Region of California.
}
\usage{data(sp3)}
\format{
  A data frame with 46 observations on the following 15 variables.
  \describe{
    \item{\code{id}}{soil id}
    \item{\code{top}}{horizon upper boundary (cm)} 
    \item{\code{bottom}}{horizon lower boundary (cm)}
    \item{\code{clay}}{clay content}
    \item{\code{cec}}{CEC by amonium acetate at pH 7}
    \item{\code{ph}}{pH in 1:1 water-soil mixture}
    \item{\code{tc}}{total carbon percent}
    \item{\code{hue}}{Munsell hue (dry)}
    \item{\code{value}}{Munsell value (dry)}
    \item{\code{chroma}}{Munsell chroma (dry)}
    \item{\code{mid}}{horizon midpoint (cm)}
    \item{\code{ln_tc}}{natural log of total carbon percent}
    \item{\code{L}}{color: l-coordinate, CIE-LAB colorspace (dry)}
    \item{\code{A}}{color: a-coordinate, CIE-LAB colorspace (dry)}
    \item{\code{B}}{color: b-coordinate, CIE-LAB colorspace (dry)}
    \item{name}{horizon name}
    \item{soil_color}{horizon color}
  }
}
\details{
These data were collected to support research funded by the Kearney Foundation of Soil Science.
}

\references{http://casoilresource.lawr.ucdavis.edu/}


\examples{
## this example investigates the concept of a "median profile"

# required packages
library(ape)
data(sp3)

# generate a RGB version of soil colors
# and convert to HSV for aggregation
sp3$h <- NA ; sp3$s <- NA ; sp3$v <- NA
sp3.rgb <- with(sp3, munsell2rgb(hue, value, chroma, return_triplets=TRUE))
sp3[, c('h','s','v')] <- t(with(sp3.rgb, rgb2hsv(r, g, b, maxColorValue=1)))

# promote to SoilProfileCollection
depths(sp3) <- id ~ top + bottom

# aggregate across entire collection
a <- slab(sp3, fm= ~ clay + cec + ph + h + s + v, slab.structure=10)

# check
str(a)

# convert back to wide format
library(reshape)
a.wide.q25 <- cast(a, top + bottom ~ variable, value=c('p.q25'))
a.wide.q50 <- cast(a, top + bottom ~ variable, value=c('p.q50'))
a.wide.q75 <- cast(a, top + bottom ~ variable, value=c('p.q75'))

# add a new id for the 25th, 50th, and 75th percentile pedons
a.wide.q25$id <- 'Q25'
a.wide.q50$id <- 'Q50'
a.wide.q75$id <- 'Q75'

# combine original data with "mean profile"
vars <- c('top','bottom','id','clay','cec','ph','h','s','v')
# make data.frame version of sp3
sp3.df <- as(sp3, 'data.frame')
sp3.grouped <- rbind(
sp3.df[, vars], a.wide.q25[, vars], a.wide.q50[, vars], a.wide.q75[, vars]
)

# re-constitute the soil color from HSV triplets
# convert HSV back to standard R colors
sp3.grouped$soil_color <- with(sp3.grouped, hsv(h, s, v))

# give each horizon a name
sp3.grouped$name <- paste(round(sp3.grouped$clay), '/' , 
round(sp3.grouped$cec), '/', round(sp3.grouped$ph,1))



## perform comparison, and convert to phylo class object
## D is rescaled to [0,]
d <- profile_compare(sp3.grouped, vars=c('clay','cec','ph'), max_d=100, 
k=0.01, replace_na=TRUE, add_soil_flag=TRUE, rescale.result=TRUE)

require(cluster)
h <- agnes(d, method='ward')
p <- ladderize(as.phylo(as.hclust(h)))


# look at distance plot-- just the median profile
plot_distance_graph(d, 12)

# similarity relative to median profile (profile #12)
round(1 - (as.matrix(d)[12, ] / max(as.matrix(d)[12, ])), 2)

## make dendrogram + soil profiles
# first promote to SoilProfileCollection
depths(sp3.grouped) <- id ~ top + bottom

# setup plot: note that D has a scale of [0,1]
par(mar=c(1,1,1,1))
p.plot <- plot(p, cex=0.8, label.offset=3, direction='up', y.lim=c(2,0), 
x.lim=c(1.25,length(sp3.grouped)+1), show.tip.label=FALSE)

# get the last plot geometry
lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)

# the original labels, and new (indexed) order of pedons in dendrogram
d.labels <- attr(d, 'Labels')

new_order <- sapply(1:lastPP$Ntip,
function(i) which(as.integer(lastPP$xx[1:lastPP$Ntip]) == i))

# plot the profiles, in the ordering defined by the dendrogram
# with a couple fudge factors to make them fit
plot(sp3.grouped, color="soil_color", plot.order=new_order,
scaling.factor=0.01, width=0.1, cex.names=0.5,
y.offset=max(lastPP$yy)+0.1, add=TRUE)
}

\keyword{datasets}
