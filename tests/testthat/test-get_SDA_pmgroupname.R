test_that("get_SDA_pmgroupname works", {
  skip_if_offline()

  skip_on_cran()
  
  res <- get_SDA_pmgroupname(areasymbols = c("CA077", "CA630"))
  skip_if(is.null(res))
  expect_equal(nrow(res), length(unique(res$mukey)))
  
  res <- get_SDA_pmgroupname(mukeys = c(461994, 461995), simplify = FALSE)
  skip_if(is.null(res))
  expect_equal(nrow(res), 2)
  
  res <- get_SDA_pmgroupname(mukeys = c(461994, 461995), simplify = FALSE, method = "none")
  skip_if(is.null(res))
  expect_equal(nrow(res), 7)
  
  res <- get_SDA_pmgroupname(mukeys = c(461994, 461995), simplify = FALSE, method = "dominant condition")
  skip_if(is.null(res))
  expect_equal(nrow(res), 2)
})
