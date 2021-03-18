test_that("fetchOSD_JSON works", {
  
  skip_if_offline()
  
  skip_on_cran()
  
  series <- c("Musick", "Hector", NA, "Foobar", "Chewacla")
  
  # warning: F/FOOBAR.json does not exist
  expect_warning({res <- fetchOSD_JSON(series)})
  
  # data.frame result w/ 3 existing official series
  expect_equal(nrow(res), 3)
  
})
