test_that("get_SDA_pmgroupname works", {
  skip_if_offline()

  skip_on_cran()
  
  x <- get_SDA_pmgroupname(areasymbols = c("CA077", "CA630"))
  expect_equal(nrow(x), length(unique(x$mukey)))
  
  expect_equal(nrow(get_SDA_pmgroupname(mukeys = c(461994, 461995), simplify = FALSE)), 2)
  expect_equal(nrow(get_SDA_pmgroupname(mukeys = c(461994, 461995), simplify = FALSE, method = "none")), 7)
  expect_equal(nrow(get_SDA_pmgroupname(mukeys = c(461994, 461995), simplify = FALSE, method = "dominant condition")), 2)
})
