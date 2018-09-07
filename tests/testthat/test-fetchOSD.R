context("fetchOSD() -- requires internet connection")


## sample data
x <- fetchOSD(soils = c('sierra', 'cecil'))


test_that("fetchOSD() returns NULL with bogus query", {
  
  # a message is printed and NULL returned when no results
  res <- suppressMessages(fetchOSD(soils='XXX'))
  expect_null(res)
  
})

test_that("fetchOSD() returns an SPC", {
  
  # standard request
  expect_match(class(x), 'SoilProfileCollection')
  
})

test_that("fetchOSD() returns reasonable data", {
  
  # standard request
  expect_equal(nrow(site(x)) == 2, TRUE)
  expect_equal(nrow(horizons(x)) > 0, TRUE)
  expect_equal(idname(x), 'id')
  expect_equal(horizonDepths(x), c("top", "bottom"))
  
})

test_that("fetchOSD() returns data associated with named series (sierra|cecil)", {
  
  # all of the results should contain the search term
  f <- grepl('sierra|cecil', x$id, ignore.case = TRUE)
  expect_equal(all(f), TRUE)
  
})

