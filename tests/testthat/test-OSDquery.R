context("OSDquery() -- requires internet connection")

test_that("OSDquery() works", {

  skip_if_offline()

  skip_on_cran()

  # a message is printed and NULL returned when no results
  res <- suppressMessages(OSDquery(geog_assoc_soils = 'pardee'))

  # standard request
  expect_true(inherits(res, 'data.frame'))

})


test_that("OSDquery() returns NULL with bogus query", {

  skip_if_offline()

  skip_on_cran()

  # a message is printed and NULL returned when no results
  res <- suppressMessages(OSDquery(geog_assoc_soils = 'XXX'))
  expect_null(res)

})


