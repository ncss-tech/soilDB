plotAvailWater <- function(x, width=0.25, cols=c(grey(0.5), 'DarkGreen', 'LightBlue', 'RoyalBlue'), name.cex=0.8) {
	x1 <- x$solid
	x2 <- x$solid + x$pwp
	x3 <- x$solid + x$fc
	x4 <- x$solid + x$sat
	n <- nrow(x)
	
	avail.water <- round(x$fc - x$pwp, 2)
	avail.water.lab.x <- ((x$solid + x$pwp) + (x$solid + x$fc)) / 2
	
	s.xaxis <- seq(from=0, to=1, by=0.1)
	idx <- 1:n
	
	plot(0,0, xlim=c(0, 1), ylim=c(1-width, n+0.5+width), type='n', ylab='', xlab='Volume Fraction', axes=FALSE)
	axis(1, at=s.xaxis)
	mtext(x$name, side=2, las=1, at=idx, cex=name.cex)
	
	segments(x0=s.xaxis, x1=s.xaxis, y0=0, y1=n, col=grey(0.25), lty=3)
	
	rect(0, idx-width, x1, idx+width, col=cols[1])
	rect(x1, idx-width, x2, idx+width, col=cols[2])
	rect(x2, idx-width, x3, idx+width, col=cols[3])
	rect(x3, idx-width, x4, idx+width, col=cols[4])
	
	text(x=avail.water.lab.x, y=idx, label=avail.water, cex=0.75, font=2)

	# legend: http://r.789695.n4.nabble.com/legend-outside-plot-area-td2325864.html
	legend("top", legend=c('Solid', 'Unavailable', 'Available', 'Gravitational'), pt.bg=cols, pch=22, bty='n', horiz=TRUE, pt.cex=2.5, cex=1.1)
	}





