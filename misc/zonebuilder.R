# https://github.com/zonebuilders/zonebuilder

# remotes::install_github("zonebuilders/zonebuilder")


library(sf)
library(soilDB)

library(zonebuilder)
library(tmap)


x <- fetchSDA_spatial(x = "459314")

x <- st_as_sf(x)

x <- st_transform(x, 6350)

a <- x[1, ]
p <- st_point_on_surface(a)

plot(st_geometry(a))
plot(st_geometry(p), add = TRUE, cex = 2, pch = 15, col = 'firebrick')

z <- zb_zone(x = p, area = a)
zb_plot(z)

z <- zb_zone(x = p, labeling = 'NESW')
zb_plot(z)

mapview::mapview(z)


par(mfrow=c(1,3))
zb_plot(zb_doughnut(p, a, n_circles = 5), title = "Doughnuts")
zb_plot(zb_segment(p, n_segments = 20), title = "Segments")
zb_plot(zb_zone(p, n_circles = 4, n_segments = 4), title = "4 segments, 4 circles")
