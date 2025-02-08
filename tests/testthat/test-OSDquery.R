context("OSDquery() -- requires internet connection")

test_that("OSDquery() works", {
  
  skip_if_not_installed("httr")
  
  skip_if_not_installed("jsonlite")
  
  skip_if_offline()

  skip_on_cran()

  # a message is printed and NULL returned when no results
  res <- suppressMessages(OSDquery(geog_assoc_soils = 'pardee'))

  # if load balancer or server is down, skip tests
  skip_if(is.null(res))
  
  # standard request
  expect_true(inherits(res, 'data.frame'))
  
  
  # search entire OSD text
  res <- suppressMessages(OSDquery(everything = 'floodplain', mlra = '18'))
  
  # standard request
  expect_true(inherits(res, 'data.frame'))

})


test_that("OSDquery() returns NULL with bogus query", {
  
  skip_if_not_installed("httr")
  
  skip_if_not_installed("jsonlite")
  
  skip_if_offline()

  skip_on_cran()

  # a message is printed and NULL returned when no results
  res <- suppressMessages(OSDquery(geog_assoc_soils = 'XXXXXX'))
  expect_null(res)

})


