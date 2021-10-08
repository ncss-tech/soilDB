context("get_OSD() -- requires internet connection")



test_that("get_OSD works", {
  
  skip_if_offline()
  
  skip_on_cran()
  
  series <- c("Musick", "Hector", NA, "Foobar", "Chewacla", "San Joaquin")
  
  # warning: F/FOOBAR.json does not exist
  expect_warning({res1 <- get_OSD(series)})
  
  # data.frame result w/ 4 existing official series
  expect_equal(nrow(res1), 4)
  
  # same 404 messages with list result="html" output
  res2 <- get_OSD(series, result = "html")
  expect_equal(length(res2), 6)
  expect_null(res2[[4]])
})



test_that("correct laundering of HTML source", {
  
  skip_if_offline()
  
  skip_on_cran()
  
  series <- c("Musick", "Hector", NA, "Foobar", "Chewacla", 'Fresno', 'zook')
  
  # get the data, should be a list
  res <- get_OSD(series, result = 'html')
  expect_true(inherits(res, 'list'))
  expect_true(length(res) == length(series))
  
  # records 3,4 are NULL
  expect_null(res[[3]])
  expect_null(res[[4]])
  
  # check lines 5 / 6
  line.5 <- sapply(res, '[', 5)
  line.6 <- sapply(res, '[', 6)
  
  # line 5 should be in the form of MM/YYY
  date.test <- sapply(line.5, grep, pattern = '[0-9][0-9]\\/[0-9][0-9][0-9][0-9]')
  # NULL list elements dropped by unlist()
  expect_true(all(unlist(date.test) == 1))
  
  # line 6 should be "XXX SERIES"
  series.test <- names(unlist(line.6)) %in% gsub(' SERIES', '', unlist(line.6))
  expect_true(all(series.test))
})


