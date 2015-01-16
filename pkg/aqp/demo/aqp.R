##
## main demo for AQP package-- a work in progress, largely just material from help files
##

# required packages
require(aqp)
require(ape)
require(cluster)
require(lattice)
require(reshape)


# 
# 1. basic profile aggregation and plotting
# 
data(sp1)
depths(sp1) <- id ~ top + bottom

# aggregate all profiles into 1,5,10,20 cm thick slabs, computing mean values by slab
s1 <- slab(sp1, ~ prop, slab.fun=mean, na.rm=TRUE)
# 5cm
s5 <- slab(sp1, ~ prop, slab.fun=mean, na.rm=TRUE, slab.structure=5)
# 10cm segments:
s10 <- slab(sp1, ~ prop, slab.fun=mean, na.rm=TRUE, slab.structure=10)
# 20cm
s20 <- slab(sp1, ~ prop, slab.fun=mean, na.rm=TRUE, slab.structure=20)

# check results
head(s1)

# variation in segment-weighted mean property: very little
round(sapply(
list(s1, s5, s10, s20), 
function(i) {
	with(i, sum((bottom - top) * value) / sum(bottom - top)) 
	}
), 1)

# combined viz
g2 <- make.groups("1cm interval"=s1, "5cm interval"=s5, 
"10cm interval"=s10, "20cm interval"=s20)

# note special syntax for plotting step function
xyplot(cbind(top,bottom) ~ value, groups=which, data=g2, id=g2$which,
panel=panel.depth_function, ylim=c(250,-10), 
scales=list(y=list(tick.number=10)), xlab='Property', 
ylab='Depth (cm)', main='Soil Profile Aggregation by Regular Depth-slice',
auto.key=list(columns=2, points=FALSE, lines=TRUE)
)



# 
# 2. investigate the concept of a 'median profile'
# note that this involves aggregation between two dissimilar groups of soils
# 
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
