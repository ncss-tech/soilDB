test_that("seriesExtent works", {
  
  skip_if_offline()
  
  skip_on_cran()
  
  x <- seriesExtent('Amador')
  expect_true(inherits(x, 'sf'))
  
  x2 <- seriesExtent('Amador', type = 'raster')
  expect_true(inherits(x2, 'SpatRaster'))
})
