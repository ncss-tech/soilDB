library(sf)
library(terra)

# larger area
# 270m soil color data
a.CA <- st_bbox(
  c(xmin = -121, xmax = -120, ymin = 37, ymax = 38), 
  crs = st_crs(4326)
)

# convert bbox to sf geometry
a.CA <- st_as_sfc(a.CA)

# query WCS
# result is in EPSG:5070
m <- soilColor.wcs(a.CA, var = 'sc015cm', res = 270)

# looks OK
plot(m, col = cats(m)[[1]]$col, legend = FALSE, axes = FALSE, main = attr(m, 'layer name'))
box()


# smaller area
# 30m soil color data
a <- st_bbox(
  c(xmin = -96.7696, xmax = -96.6477, ymin = 36.5477, ymax = 36.6139), 
  crs = st_crs(4326)
)

m <- soilColor.wcs(a, var = 'sc075cm_hr', res = 30)

# looks OK
plot(m, col = cats(m)[[1]]$col, legend = FALSE, axes = FALSE, main = attr(m, 'layer name'))
box()


# -89.2633 37.9833,-89.2633 38.0151,-89.1809 38.0151,-89.1809 37.9833,-89.2633 37.9833