context("OSDquery() -- requires internet connection")

test_that("OSDquery() works", {

  skip_if_not_installed("httr")
  
  skip_if_not_installed("jsonlite")
  
  skip_if_offline()
  
  skip_on_cran()
  
  res <- try(suppressMessages(OSDquery(geog_assoc_soils = 'pardee')))
  
  skip_if(inherits(res, 'try-error') || is.null(res))
  
  # standard request
  expect_true(inherits(res, 'data.frame'))
  
  # search entire OSD text
  res <- try(suppressMessages(OSDquery(everything = 'floodplain', mlra = '18')))
  
  skip_if(inherits(res, 'try-error') || is.null(res))
  
  # standard request
  expect_true(inherits(res, 'data.frame'))
  
  # a message is printed and NULL returned when no results
  res <- try(suppressMessages(OSDquery(geog_assoc_soils = 'XXXXXX')))
  
  skip_if(inherits(res, 'try-error'))
  
  expect_null(res)
  
})


