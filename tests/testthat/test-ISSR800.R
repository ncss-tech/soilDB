test_that("ISSR800.wcs works", {
  
  skip_if_offline()
  
  skip_on_cran()
  
  x <- NULL
  
  expect_true(inherits(WCS_details("ISSR800"), 'data.frame'))
  
  x <-
    ISSR800.wcs(
      aoi = list(aoi = c(-114.16, 47.6,-114.15, 47.7),
                 crs = 'EPSG:4326'),
      var = 'paws',
      quiet = TRUE
    )
  
  expect_true(inherits(x, 'SpatRaster') || inherits(x, 'try-error'))

  x2 <- ISSR800.wcs(
      aoi = list(aoi = c(-114.16, 47.6,-114.15, 47.7),
                 crs = 'EPSG:4326'),
      var = 'suborder',
      quiet = TRUE
    )
  
  ## DEB: disabling for now, there is no guaranteed result
  
  # expect_true(inherits(x2, 'SpatRaster') || inherits(x2, 'try-error'))
  # if (!inherits(x2, 'try-error')) {
  #   expect_equal(nrow(terra::cats(x2)[[1]]), 66)
  # }
})
