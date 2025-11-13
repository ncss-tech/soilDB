context("seriesExtent() -- requires internet connection")


test_that("seriesExtent works", {
  
  skip_if_offline()
  skip_on_cran()
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  
  # test vector representation
  x <- seriesExtent('Amador')
  
  # NULL result on network-related error
  skip_if(is.null(x))
  
  expect_true(inherits(x, 'sf'))
  
  # test gridded representation
  x2 <- seriesExtent('Amador', type = 'raster')
  
  # NULL result on network-related error
  skip_if(is.null(x2))
  
  expect_true(inherits(x2, 'SpatRaster'))
})
