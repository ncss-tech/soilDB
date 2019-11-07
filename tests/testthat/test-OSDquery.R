context("OSDquery() -- requires internet connection")

test_that("OSDquery() works", {
  
  skip_if_offline()
  
  # a message is printed and NULL returned when no results
  res <- suppressMessages(OSDquery(geog_assoc_soils = 'pardee'))
  
  # standard request
  expect_match(class(res), 'data.frame')
  
})


test_that("OSDquery() returns NULL with bogus query", {
  
  skip_if_offline()
  
  # a message is printed and NULL returned when no results
  res <- suppressMessages(OSDquery(geog_assoc_soils = 'XXX'))
  expect_null(res)
  
})


