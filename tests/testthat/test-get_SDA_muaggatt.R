test_that("get_SDA_muaggatt works", {
  
  skip_if_not_installed("httr")
  
  skip_if_offline()

  skip_on_cran()
  
  res <- get_SDA_muaggatt(areasymbols = c("CA077", "CA630"))
  
  skip_if(is.null(res))
  
  expect_length(unique(res$mukey), nrow(res))
  expect_identical(nrow(get_SDA_muaggatt(mukeys = c(461994, 461995))), 2)
})

