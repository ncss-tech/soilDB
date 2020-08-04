library(aqp)
library(soilDB)

# KSSL
x <- fetchKSSL(series = 'tulare')
# SSURGO
y <- fetchSDA("compname = 'tulare'")

# grouping ID for later
site(x)$origin <- 'KSSL'
site(y)$origin <- 'SSURGO'

# normalize select hz properties
x$db_thirdbar <- x$db_13b
y$db_thirdbar <- y$dbthirdbar_r

x$om <- x$estimated_om
y$om <- y$om_r

x$name <- x$hzn_desgn
y$name <- y$hzname

# combine
z <- aqp::union(list(x, y))

# no fragments
horizons(z)$soil_fraction <- 1

# horizon SOM
hd <- horizonDepths(z)

z$SOC_kg_sq_m <- profileApply(z, FUN = function(i) {
  
  # horizon thickness
  i$thick <- i[[hd[2]]] - i[[hd[1]]]
  
  # horizon SOM kg / sq. m.
  # horizon thickness (cm) * OM fraction (0-1) * soil fraction (0-1) * Db 1/3 bar (g/cm^3) * unit conversion
  res <- i$thick * i$om / 100.0 * i$soil_fraction * i$db_thirdbar * 10
  
  return(res)
})

# sum
z$sum_SOC_kg_sq_m <- profileApply(z, function(i) sum(i$SOC_kg_sq_m, na.rm=TRUE))


# simple plot
par(mar=c(0,0,3,1))
groupedProfilePlot(z, groups = 'origin', color = 'om', group.name.offset = -20, print.id=TRUE)
groupedProfilePlot(z, groups = 'origin', color = 'db_thirdbar', group.name.offset = -20, print.id=TRUE)
groupedProfilePlot(z, groups = 'origin', color = 'SOC_kg_sq_m', group.name.offset = -20, print.id=TRUE)

# summarize: note that one of the SSURGO components is bogus: missing lower horizon data
s <- site(z)[, c('origin', 'sum_SOC_kg_sq_m')]
tapply(s$sum_SOC_kg_sq_m, s$origin, median)

