context("OSDquery() -- requires internet connection")

## sample data
x <- OSDquery(geog_assoc_soils = 'pardee')


test_that("OSDquery() returns NULL with bogus query", {
  
  # a message is printed and NULL returned when no results
  res <- suppressMessages(OSDquery(geog_assoc_soils = 'XXX'))
  expect_null(res)
  
})

test_that("OSDquery() returns a data.frame", {
  
  # standard request
  expect_match(class(x), 'data.frame')
  
})

