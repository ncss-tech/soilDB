test_that("get_SDA_muaggatt works", {
  skip_if_offline()

  skip_on_cran()
  
  d <- get_SDA_muaggatt(areasymbols = c("CA077", "CA630"))

  expect_equal(nrow(d), length(unique(d$mukey)))
  expect_equal(nrow(get_SDA_muaggatt(mukeys = c(461994, 461995))), 2)
})

