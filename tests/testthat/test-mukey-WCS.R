test_that("mukey.wcs works", {

  x <- NULL
  
  expect_true(inherits(WCS_details("mukey"), 'data.frame'))

  expect_silent(suppressWarnings({

    x <- mukey.wcs(aoi = list(aoi = c(-114.16, 47.655, -114.155, 47.66),
                              crs = '+init=epsg:4326'),
                   db = 'gnatsgo', quiet = TRUE)

  }))

  expect_true(inherits(x, 'RasterLayer'))
})

