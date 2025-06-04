test_that("get_SDA_pmgroupname works", {
  
  skip_if_not_installed("httr")
  
  skip_if_offline()

  skip_on_cran()
  
  res <- get_SDA_pmgroupname(areasymbols = c("CA077", "CA630"))
  skip_if(is.null(res))
  expect_length(unique(res$mukey), nrow(res))
  
  # some misc areas have geomorph populated (e.g. "Mixed alluvial land", but others, like "Water" are NULL)
  res <- get_SDA_pmgroupname(mukeys = c(462409, 2462630, 465186), simplify = FALSE, method = "dominant condition") # default is miscellaneous_areas=FALSE
  skip_if(is.null(res))
  expect_identical(nrow(res), 3)
  
  res <- get_SDA_pmgroupname(mukeys = c(462409, 2462630, 465186), simplify = FALSE, miscellaneous_areas = TRUE, method = "dominant condition")
  skip_if(is.null(res))
  expect_identical(nrow(res), 3)
  
  res <- get_SDA_pmgroupname(mukeys = c(461994, 461995, 465186), simplify = FALSE, method = "none", miscellaneous_areas = TRUE, include_minors = FALSE)
  skip_if(is.null(res))
  expect_identical(nrow(res), 5)  
  
  res <- get_SDA_pmgroupname(mukeys = c(461994, 461995, 465186), simplify = FALSE, method = "none", miscellaneous_areas = TRUE)
  skip_if(is.null(res))
  expect_identical(nrow(res), 14)
  
  res <- get_SDA_pmgroupname(mukeys = c(461994, 461995, 465186), simplify = FALSE, method = "dominant condition")
  skip_if(is.null(res))
  expect_identical(nrow(res), 3)
})
