context("taxaExtent() -- requires internet connection")

test_that("taxaExtent() works", {
  
  skip_if_offline()
  skip_on_cran()
  skip_if_not_installed("terra", minimum_version = "1.8.93")
  skip_if_not(terra::proj_ok())
  
  # typical request, fully specified arguments
  x <- taxaExtent(x = 'typic haploxeralfs', type = 'taxon', level = 'subgroup')
  
  # NULL result on network-related error
  # NULL could also mean incomplete sf/terra installation, likely missing proj.db
  skip_if(is.null(x))
  
  expect_true(inherits(x, 'SpatRaster'))
})


test_that("common usage errors", {
  
  skip_if_offline()
  skip_on_cran()
  skip_if_not_installed("terra", minimum_version = "1.8.93")
  skip_if_not(terra::proj_ok())
  
  # problematic arguments
  expect_error(taxaExtent(x = 'typic haploxeralfs', type = 'x', level = 'subgroup'))
  
  # incorrect level
  expect_null(taxaExtent(x = 'typic haploxeralfs', type = 'taxon', level = 'suborder'))
  
  # formative elements only available at greatgroup and subgroup levels
  expect_error(taxaExtent(x = 'alfs', type = 'formative element', level = 'suborder'))
})

