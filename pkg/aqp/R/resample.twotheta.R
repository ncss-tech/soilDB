
## TODO: generalize to working with several patterns at once
# resample intensities according to a new 2-theta interval
resample.twotheta <- function(twotheta, x, tt.min=min(twotheta), tt.max=max(twotheta), new.res=0.02)
	{
	# fit a spline-function to the data 	
	sf <- splinefun(twotheta, x)
	
	# generate a new sequence of two-theta values
	# according to the requested resolution
	s <- seq(tt.min, tt.max, by=new.res)
	
	# interpolate onto new two-theta sequence
	x.new <- data.frame(twotheta=s, x=sf(s))
	
	return(x.new)
	}
	

