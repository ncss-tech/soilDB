library(lattice)
library(Hmisc)
library(aqp)

# convert angle in degrees to percent
deg2pct <- function(theta) {
	tan(theta * pi/180) * 100
}

# convert angle in percent to radians
pct2rad <- function(pct) {
	atan(pct / 100)
}

# compute difference in thickness as a function of slope pct
thickDiff <- function(pct, Tv) {
	Tp <- Tv * cos(pct2rad(pct))
	return(Tv - Tp)
}


# make a grid of horizon thickness and slope percents
d <- expand.grid(thick=c(5,10,20,50,100), slope=1:70)
d$thickDiff <- thickDiff(d$slope, d$thick)

# plot effects for different horizon thickness
xYplot(thickDiff ~ slope, data=d, groups=factor(thick, labels=paste(c(5,10,20,50,100), 'cm depth')), type='l', ylab='(Vertical - Perpendicular) Hz Thickness', xlab='Slope (%)', main='Difference in Apparent Horizon Thickness as a Function of Slope', scales=list(alternating=3, tick.number=10), par.settings=list(superpose.line=list(col='black', lwd=1.5)), label.curves=list(cex=1), panel=function(...) {
	panel.abline(v=seq(0, 70, by=10), h=seq(0, 20, by=2), col=grey(0.75), lty=3)
	panel.xYplot(...)
})



## plot the effects via 10 pedons

# generate a profile, keep only the relevant stuff
p <- random_profile(1)[, c('id', 'top', 'bottom', 'name')]

# stack 10 of these up
pp <- rbind(p, p, p, p, p, p, p, p, p, p)

# fix IDs
pp$id <- rep(1:10, each=nrow(p))

# upgrade to SPC
depths(pp) <- id ~ top + bottom

# add fake slope to site
s <- data.frame(id=1:10, slope=seq(from=1, to=70, length.out=10))
site(pp) <- s


# function for transforming vertical to perpendicular depths
# operates on a single SPC object
VtoP <- function(i, which) {
	# get vector of bottom depths
	b <- horizons(i)[[which]]
	# convert to perpendicular depths and round to an integer
	b.p <- round(b * cos(pct2rad(i$slope)))
	return(b.p)
}


# add perpendicular depths to original data
pp$Tp <- profileApply(pp, VtoP, which='bottom')


# basic plot
par(mar=c(2,0,0.75,0))
plot(pp, n.depth.ticks=10)

# title
title('Vertical Horizon Depths (black) vs. Perpendicular Horizon Depths (blue)', cex.main=0.75)

# add axis with slope values
axis(side=1, cex.axis=0.75, line=-1.25, at=1:10, labels=round(s$slope))
mtext(side=1, 'Slope (%)', line=0.5, cex=0.75)

# iterate over profiles and add offset hz boundaries
for(i in 1:length(pp)) {
	pp.i <- pp[i, ]
	# use color to show only those offsets that are > 1 cm
	cols <- ifelse(abs(pp.i$bottom - pp.i$Tp) > 1, 'RoyalBlue', NA)
	arrows(x0=i, x1=i, y0=pp.i$bottom, y1=pp.i$Tp, length=0.08, col=cols)
	segments(x0=i-0.2, x1=i+0.2, y0=pp.i$Tp, y1=pp.i$Tp, col=cols, lty=2)
}


## another approach- change actual hz depths
pp.perp <- pp

pp.perp$top <- profileApply(pp.perp, VtoP, which='top')
pp.perp$bottom <- profileApply(pp.perp, VtoP, which='bottom')

par(mar=c(2,0,0.75,0))
plot(pp.perp, n.depth.ticks=10)
title('Changes in Horizon Depth as a Function of Surface Slope', cex.main=0.75)
axis(side=1, cex.axis=0.75, line=-1.25, at=1:10, labels=round(s$slope))
mtext(side=1, 'Slope (%)', line=0.5, cex=0.75)
abline(h=seq(from=5, to=75, by=5), lty=3, col='grey')


## compute mean and total difference in horizon thickness:
# compute difference in horizon thickness
diff.thick <- (pp$bottom - pp$top) - (pp.perp$bottom - pp.perp$top)
theID <- horizons(pp)$id # extract IDs
# compute mean and total difference by pedon
diff.by.pedon <- by(diff.thick, theID, function(i) cbind(mean.diff.cm=mean(i), total.diff.cm=sum(i)))
# composite into a data.frame and add slope values
data.frame(do.call('rbind', diff.by.pedon), slope=round(s$slope))

