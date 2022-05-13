target_areas <-  c("CA649", "CA630")
target_area_rows <- 220
target_area_rows_all <- 1021

target_mukeys <- c(463263, 463264)

test_that("SDA interpretations (dominant component) works", {
  skip_if_offline()

  skip_on_cran()

  res <- get_SDA_interpretation("FOR - Potential Seedling Mortality",
                                method = "Dominant Component", areasymbols = target_areas)
  expect_equal(nrow(res), target_area_rows)

  res <- get_SDA_interpretation(c("FOR - Potential Seedling Mortality",
                                  "FOR - Road Suitability (Natural Surface)"),
                                method = "Dominant Component", mukeys = target_mukeys)
  expect_equal(sort(res$mukey), sort(target_mukeys))
})

test_that("SDA interpretations (dominant condition) works", {
  skip_if_offline()

  skip_on_cran()

  res <- get_SDA_interpretation("FOR - Potential Seedling Mortality",
                                method = "Dominant Condition", areasymbols = target_areas)
  expect_equal(nrow(res), target_area_rows)


  res <- get_SDA_interpretation(c("FOR - Potential Seedling Mortality",
                                  "FOR - Road Suitability (Natural Surface)"),
                                method = "Dominant Condition", mukeys = target_mukeys)
  expect_equal(sort(res$mukey), sort(target_mukeys))
})

test_that("SDA interpretations (weighted average) works", {
  skip_if_offline()

  skip_on_cran()

  res <- get_SDA_interpretation("FOR - Potential Seedling Mortality",
                                method = "Weighted Average", areasymbols = target_areas)
  expect_equal(nrow(res), target_area_rows)


  res <- get_SDA_interpretation(c("FOR - Potential Seedling Mortality",
                                  "FOR - Road Suitability (Natural Surface)"),
                                method = "Weighted Average", mukeys = target_mukeys)
  expect_equal(sort(res$mukey), sort(target_mukeys))
})

test_that("SDA interpretations (no aggregation) works", {
  skip_if_offline()

  skip_on_cran()

  res <- get_SDA_interpretation(c("FOR - Potential Seedling Mortality",
                                  "FOR - Road Suitability (Natural Surface)"),
                                method = "NONE",
                                areasymbols = target_areas)
  expect_equal(nrow(res), target_area_rows_all)


})
