context("ISSR800.wcs() -- requires internet connection")

test_that("works as expected", {
  
  skip_if_offline()
  
  skip_on_cran()
  
  x <- NULL
  
  expect_true(inherits(WCS_details("ISSR800"), 'data.frame'))
  
  suppressWarnings({
    
    # 800m grid
    x <- ISSR800.wcs(aoi = list(aoi = c(-114.16, 47.6, -114.15, 47.7),
                                crs = '+init=epsg:4326'),
                     var = 'paws', quiet = TRUE)
    
  })
  
  expect_true(inherits(x, 'RasterLayer') || inherits(x, 'try-error'))
  
  # if it downloaded
  if(inherits(x, 'RasterLayer')) {
    
    # expected dimensions
    expect_true(
      all(
        dim(x) == c(14, 4, 1)
      )
    )
    
    # no RAT
    expect_true(is.null(levels(x)))
  }
  
})


test_that("categorical data", {
  
  skip_if_offline()
  
  skip_on_cran()
  
  x <- NULL
  
  suppressWarnings({
    
    # 800m grid
    x <- ISSR800.wcs(aoi = list(aoi = c(-114.16, 47.6, -114.15, 47.7),
                                crs = '+init=epsg:4326'),
                     var = 'texture_2550cm', quiet = TRUE)
    
  })
  
  expect_true(inherits(x, 'RasterLayer') || inherits(x, 'try-error'))
  
  # if it downloaded
  if(inherits(x, 'RasterLayer')) {
    # expected dimensions
    expect_true(
      all(
        dim(x) == c(14, 4, 1)
      )
    )
    
    # there must be a RAT
    expect_true(! is.null(levels(x)))
  }
  
})



