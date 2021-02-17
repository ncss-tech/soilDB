
## TODO: 
# * realistic comparisons / case studies
# * Ksat comparisons
# * 


library(aqp)
library(soilDB)
library(latticeExtra)
library(tactile)

# make a table of soil textures
# sand sand/silt/clay values at geometric centroids
tex <- SoilTextureLevels()
x <- texcl_to_ssc(tex)
x <- cbind(x, tex)

r1 <- ROSETTA(x, vars = c('sand', 'silt', 'clay'), v = '1')
r2 <- ROSETTA(x, vars = c('sand', 'silt', 'clay'), v = '2')
r3 <- ROSETTA(x, vars = c('sand', 'silt', 'clay'), v = '3')

r <- rbind(r1, r2, r3)

# iterate over results and generate VG model curve
awc <- lapply(1:nrow(r), function(i) {
  
  # model bounds are given in kPA of suction
  vg <- KSSL_VG_model(VG_params = r[i, ], phi_min = 10^-3, phi_max=10^6)
  
  # extract VWC at specific matric potentials
  d <- data.frame(
    texture = r$tex[i], 
    sat = vg$VG_function(0),
    fc = vg$VG_function(33),
    pwp = vg$VG_function(1500)
  )
  
  # simplistic determination of AWC using 33 kPa -> 1500 kPa interval
  d$awc <- with(d, fc - pwp)
  
  # save model version and texture class
  d$.rosetta.version <- r$.rosetta.version[i]
  d$tex <- r$tex[i]
  
  return(d)
})

awc <- do.call('rbind', awc)

# iterate over results and generate VG model curve
res <- lapply(1:nrow(r), function(i) {
  
  # model bounds are given in kPA of suction
  vg <- KSSL_VG_model(VG_params = r[i, ], phi_min = 10^-3, phi_max=10^6)
  
  # extract curve and add texture ID
  m <- vg$VG_curve
  m$texture <- r$tex[i]
  
  # save model version
  m$.rosetta.version <- r$.rosetta.version[i]
  
  return(m)
})

# flatten to data.frame
res <- do.call('rbind', res)


tps <- tactile.theme(
  plot.line=list(col='royalblue', lwd = 2) 
)


dotplot(
  factor(.rosetta.version) ~ awc | tex, data = awc,
  ylab = 'Rosetta Model Version', 
  scales = list(alternating=3, x=list(tick.number=6)), 
  xlab = expression(AWC~~(cm^3/cm^3)), 
  par.settings = tps, 
  strip = strip.custom(bg=grey(0.85)), 
  as.table = TRUE,
  layout = c(7, 3),
  main='Idealized Water Retention'
)

xyplot(
  phi ~ theta | texture, data = res, groups = .rosetta.version,
  type = c('l', 'g'), 
  auto.key = list(lines = TRUE, points = FALSE, columns = 3),
  scales = list(alternating=3, x=list(tick.number=6, cex = 0.75), y=list(log=10, tick.number=6)), 
  yscale.components = yscale.components.logpower, 
  ylab = expression(Matric~~Potential~~(-kPa)), 
  xlab = expression(Volumetric~Water~Content~~(cm^3/cm^3)), 
  par.settings = tps, 
  strip = strip.custom(bg=grey(0.85)), 
  as.table = TRUE,
  layout = c(7, 3),
  main='Idealized Water Retention'
)
