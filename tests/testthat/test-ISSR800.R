context("ISSR800.wcs() -- requires internet connection")

test_that("works as expected", {
  
  skip_if_offline()
  
  skip_on_cran()
  
  x <- NULL
  
  expect_true(inherits(WCS_details("ISSR800"), 'data.frame'))
  
  skip_if_not_installed("sf")
  
  skip_if_not_installed("terra")
  
  # 800m grid
  x <- ISSR800.wcs(
      aoi = list(aoi = c(-114.16, 47.6,-114.15, 47.7),
                 crs = 'EPSG:4326'),
      var = 'paws',
      quiet = TRUE
    )
  
  expect_true(inherits(x, 'SpatRaster') || inherits(x, 'try-error'))
  
  # if it downloaded
  if (inherits(x, 'SpatRaster')) {
    
    # expected dimensions
    expect_true(all(dim(x) == c(14, 4, 1)))
    
    # no RAT
    expect_null(terra::cats(x)[[1]])
  }
  
})


test_that("categorical data", {
  
  skip_if_offline()
  
  skip_on_cran()
  
  skip_if_not_installed("sf")
  
  skip_if_not_installed("terra")
  
  x <- NULL
  
  # 800m grid
  x <- ISSR800.wcs(
      aoi = list(aoi = c(-114.16, 47.6,-114.15, 47.7),
                 crs = 'EPSG:4326'),
      var = 'texture_2550cm',
      quiet = TRUE
    )
  
  
  expect_true(inherits(x, 'SpatRaster') || inherits(x, 'try-error'))
  
  # if it downloaded
  if (inherits(x, 'SpatRaster')) {
    # expected dimensions
    expect_true(all(dim(x) == c(14, 4, 1)))
    
    # there must be a RAT
    expect_false(is.null(terra::levels(x)))
    expect_equivalent(colnames(terra::cats(x)[[1]]), c('ID','class','hex','names'))
  }
  
  x2 <- ISSR800.wcs(
    aoi = list(aoi = c(-114.16, 47.6,-114.15, 47.7),
               crs = 'EPSG:4326'),
    var = 'suborder',
    quiet = TRUE
  )
  
  expect_true(inherits(x2, 'SpatRaster') || inherits(x2, 'try-error'))
  
  if (!inherits(x2, 'try-error')) {
    expect_equivalent(colnames(terra::cats(x2)[[1]]), c('ID', 'suborder'))
  }

})



