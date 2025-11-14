context("seriesExtent() -- requires internet connection")


test_that("seriesExtent works", {
  
  skip_if_offline()
  skip_on_cran()
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  
  # test vector representation
  x <- seriesExtent('Amador')
  
  # test gridded representation
  x2 <- seriesExtent('Amador', type = 'raster')
  
  # NULL result on network-related error
  # NULL could also mean incomplete sf/terra installation, likely missing proj.db
  skip_if(is.null(x))
  skip_if(is.null(x2))
  
  # expected data type
  expect_true(inherits(x, 'sf'))
  expect_true(inherits(x2, 'SpatRaster'))
})
