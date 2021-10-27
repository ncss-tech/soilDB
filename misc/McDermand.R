library(soilDB)
library(aqp)

x <- fetchOSD('McDermand')

plotSPC(x, width = 0.1)
cols <- sprintf('%s %s/%s', x$hue, x$value, x$chroma)

par(mar = c(0, 0, 0, 0))
soilPalette(x$soil_color, lab = cols, lab.cex = 1)


