context("taxaExtent() -- requires internet connection")



test_that("taxaExtent() works", {
  
  skip_if_offline()
  skip_on_cran()
  skip_if_not_installed("terra")
  
  # test vector representation
  x <- taxaExtent(x = 'typic haploxeralfs', type = 'taxon', level = 'subgroup')
  
  # NULL result on network-related error
  skip_if(is.null(x))
  
  expect_true(inherits(x, 'SpatRaster'))
  
})
