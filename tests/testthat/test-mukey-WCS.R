test_that("mukey.wcs works", {
  
  skip_if_offline()
  
  skip_on_cran()
  
  x <- NULL
  
  expect_true(inherits(WCS_details("mukey"), 'data.frame'))

  suppressWarnings({

    x <- mukey.wcs(aoi = list(aoi = c(-114.16, 47.655, -114.155, 47.66),
                              crs = '+init=epsg:4326'),
                   db = 'gnatsgo', quiet = TRUE)

  })
  
  expect_true(inherits(x, 'RasterLayer') || inherits(x, 'try-error'))
})

