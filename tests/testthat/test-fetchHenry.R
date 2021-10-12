context("fetchHenry and related")




test_that("month2season() works as expected", {
  
  x <- c('Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May')
  res <- month2season(x)
  
  # classification
  expect_equal(
    as.character(res), 
    c("Summer", "Summer", "Summer", "Fall", "Fall", "Fall", "Winter", "Winter", "Winter", "Spring", "Spring", "Spring")
    )
  
  # factor levels
  expect_equal(
    levels(res),
    c("Winter", "Spring", "Summer", "Fall")
  )
  
  # bogus input
  expect_true(
    is.na(month2season('da'))
  )
  
})



test_that("summarizeSoilTemperature() works as expected", {
  
  expect_true(TRUE)
  
})


test_that(".fill_missing_days() works as expected", {
  
  expect_true(TRUE)
  
})


test_that(".formatDates() works as expected", {
  
  expect_true(TRUE)
  
})

test_that(".formatDates() works as expected", {
  
  expect_true(TRUE)
  
})


test_that("fetchHenry() works as expected", {
  
  expect_true(TRUE)
  
})








