library(aqp)
library(soilDB)
library(lattice)
library(viridisLite)
library(tactile)
library(ragg)

# get rid of this
library(plyr)


x <- fetchSCAN(site.code=c(2144), year=2008:2015)

# extract SMS data to a new data.frame
sms <- x$SMS

# add parts of date for plotting
sms$year <- format(sms$Date, "%Y")
sms$doy <- as.numeric(format(sms$Date, "%j"))

sms <- sms[which(sms$year == '2015' & sms$depth < 100), ]


# get KSSL data
s <- fetchKSSL(pedon_id = 'S08NV003003')

# get VG parameters for sensor at 20cm depth
s.vg.hz <- slice(s, 20 ~ theta_r + theta_s + alpha + npar, just.the.data = TRUE)

# get VG curve and inverse function
# this is based on several assumptions about the source data... details pending
vg.hz <- KSSL_VG_model(VG_params = s.vg.hz)



# split soil moisture data by sensor depth
sms.list <- split(sms, sms$depth)

# convert VWC -> phi (KPa)
sms.phi <- ldply(sms.list, function(i) {
  # i is a DF associated with a single sensor depth
  this.depth <- unique(i$depth)
  # get VG params for this depth
  fm <- as.formula(paste0(this.depth, ' ~ theta_r + theta_s + alpha + npar'))
  s.vg.hz <- slice(s, fm, just.the.data = TRUE)
  
  # get VG curve and inverse function
  # this is based on several assumptions about the source data...
  vg.hz <- KSSL_VG_model(VG_params = s.vg.hz)
  
  # estimate phi
  if(!is.null(vg.hz$VG_inverse_function))
    # convert 0-100 scale VWC to 0-1 scale
    i$phi <- vg.hz$VG_inverse_function(i$value / 100.0)
  else
    i$phi <- NA
  
  # done
  return(i)
})


## this is very confusing, elaborate on all of this
# classify each day as > 1500 kPa (15 bar)
# this is suction, so logic is reversed
sms.phi$phi_leq_15_bar <- factor(sms.phi$phi > 1500, levels=c('FALSE', 'TRUE'))


# get texture classes at sensor depths
fm <- as.formula(
  sprintf(
    "c(%s) ~ lab_texture_class",
    paste(unique(sms$depth), collapse = ',')
  )
)

slice(s, fm = fm, just.the.data = TRUE)


# dry -> wet : low -> high VWC
cols <- colorRampPalette(viridis(n = 128, direction = -1), space='Lab', interpolate='spline', bias=2)
tps <- tactile.theme(regions = list(col = cols))


# plot VWC by year/day, panels by depth
# plot VWC by year/day, panels by year with depths stacked
p1 <- levelplot(
  value ~ doy * rev(factor(depth)), main='Daily Mean VWC (%)',
  data=sms,
  colorkey=list(space='top', width = 0.66), 
  par.settings = tps,
  scales=list(alternating=1, cex=0.75, x=list(tick.number=20)), 
  par.strip.text=list(cex=0.85), strip=strip.custom(bg='grey'), 
  xlab='Julian Day', ylab='Sensor Depth (cm)', ylim=rev(levels(factor(sms$depth)))
)




# plot daily mean water potential in log10-kPa by year and sensor stack

# wet -> dry : low -> high suction
cols.phi <- colorRampPalette(viridis(n = 128), space='Lab', interpolate='spline', bias = 1.5)
tps.phi <- tactile.theme(regions = list(col = cols.phi))


# convert kPa to bar for smaller numbers
# custom make color key in log10 space
# annotate on original scale
phi.log.pretty <- pretty(log(sms.phi$phi, base = 10), n = 8)
phi.kPa.pretty <- (10 ^ phi.log.pretty) / 100

ck <- list(space = 'top', col = cols.phi, width = 0.66, at = phi.log.pretty, labels = list(labels = phi.kPa.pretty, at = phi.log.pretty))

p2 <- levelplot(
  log(phi, base=10) ~ doy * rev(factor(depth)), main='Daily Mean Water Potential (suction, bar)',
  data=sms.phi, 
  colorkey = ck,  
  par.settings = tps.phi,
  scales=list(alternating=1, cex=0.75, x=list(tick.number=20)), 
  par.strip.text=list(cex=0.85), strip=strip.custom(bg='grey'), 
  xlab='Julian Day', ylab='Sensor Depth (cm)', ylim=rev(levels(factor(sms.phi$depth)))
)


## interpretation of phi drier than PWP

cols.dry <- c('firebrick', grey(0.9))
tps.dry <- tactile.theme(regions = list(col = cols.dry))



ck <- list(space = 'top', col = cols.dry, width = 0.66, at = c(0, 1, 2), labels = list(labels = c('TRUE', 'FALSE'), at = c(0.5, 1.5)))

p3 <- levelplot(
  phi_leq_15_bar ~ doy * factor(depth), 
  main='Drier than PWP (15 bar suction)',
  data=sms.phi, 
  par.settings = tps.dry,
  colorkey = ck, 
  as.table=TRUE, 
  scales=list(alternating=1, cex=0.75, x=list(tick.number=20)), 
  par.strip.text=list(cex=0.85), strip=strip.custom(bg='grey'), 
  xlab='Julian Day', ylab='Sensor Depth (cm)',
  ylim=rev(levels(factor(sms.phi$depth)))
)


agg_png(file = 'awc-phi-demonstration.png', width = 1000, height = 1000, scaling = 1.75)

print(p1, split = c(1, 1, 1, 3), more = TRUE)
print(p2, split = c(1, 2, 1, 3), more = TRUE)
print(p3, split = c(1, 3, 1, 3), more = FALSE)

dev.off()



