test_that("ISSR800.wcs works", {
  
  x <- NULL
  
  expect_true(inherits(WCS_details("ISSR800"), 'data.frame'))
  
  expect_silent(suppressWarnings({
    
    x <- ISSR800.wcs(aoi = list(aoi = c(-114.16, 47.6, -114.15, 47.7),
                                crs = '+init=epsg:4326'),
                     var = 'paws', quiet = TRUE)
    
  }))
  
  expect_true(inherits(x, 'RasterLayer'))
})
