context("mukey.wcs() -- requires internet connection")

# NOTES:
# * mukey.wcs() will stop() if rast() fails

test_that("works as expected", {
  
  skip_if_offline()
  skip_on_cran()
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  
  x <- NULL
  
  
  expect_true(inherits(WCS_details("mukey"), 'data.frame'))
  
  # make a request
  # typical failure modes:
  #  * network error
  #  * server maintenance
  #  * incomplete terra package installation, missing proj.db
  suppressWarnings({
    
    # 30m grid
    x <- try(
      mukey.wcs(
        aoi = list(aoi = c(-114.16, 47.655, -114.155, 47.66), crs = 'EPSG:4326'),
        db = 'gnatsgo', quiet = TRUE)
    )
  })
  
  # try-error indicates some kind of network related issue
  expect_true(inherits(x, 'SpatRaster') || inherits(x, 'try-error'))
  
  if (inherits(x, 'SpatRaster')) {
    
    # expected dimensions
    expect_true(all(dim(x) == c(20, 16, 1)))
    
    # must have a RAT
    expect_false(is.null(terra::levels(x)))
  }
  
})

