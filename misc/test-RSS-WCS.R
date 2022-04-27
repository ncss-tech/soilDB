devtools::load_all()

library(terra)
library(sf)

# slightly larger than Coweeta RSS BBOX
a <- st_bbox(
  c(xmin = 1129000, xmax = 1135000, ymin = 1403000, ymax = 1411000), 
  crs = st_crs(5070)
)

a <- st_as_sfc(a)


par(mfcol = c(1, 2))
(x <- mukey.wcs(a, db = 'gSSURGO', res = 30))
attr(x, 'layer name')
plot(x)
plot(a, add = TRUE)

(y <- mukey.wcs(a, db = 'RSS', res = 10))
attr(y, 'layer name')
plot(y, ext = x)
plot(a, add = TRUE)
