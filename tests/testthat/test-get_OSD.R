test_that("get_OSD works", {
  
  skip_if_offline()
  
  skip_on_cran()
  
  series <- c("Musick", "Hector", NA, "Foobar", "Chewacla")
  
  # warning: F/FOOBAR.json does not exist
  expect_warning({res1 <- get_OSD(series)})
  
  # data.frame result w/ 3 existing official series
  expect_equal(nrow(res1), 3)
  
  # same 404 messages with list result="html" output
  res2 <- get_OSD(series, result = "html")
  expect_equal(length(res2), 5)
  expect_null(res2[[4]])
})
