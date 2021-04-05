target_areas <-  c("CA649", "CA630")
target_area_rows <- 231

target_mukeys <- c(463263, 463264)

# test get_SDA_property results -- expect a data.frame result for two legends

test_that("SDA properties (dominant condition) works", {
  skip_if_offline()

  skip_on_cran()

  expect_equal(nrow(get_SDA_property(property = "Taxonomic Suborder",
                                     method = "Dominant Condition",
                                     areasymbols = target_areas)), target_area_rows)

  expect_equal(get_SDA_property(property = "Taxonomic Suborder",
                                     method = "Dominant Condition",
                                     mukeys = target_mukeys)$mukey, target_mukeys)

})

test_that("SDA properties (dominant component category) works", {
  skip_if_offline()

  skip_on_cran()
  expect_equal(nrow(get_SDA_property(property = "Taxonomic Suborder",
                                     method = "Dominant Component (Category)",
                                     areasymbols = target_areas)), target_area_rows)

  expect_equal(get_SDA_property(property = "Taxonomic Suborder",
                                     method = "Dominant Component (Category)",
                                     mukeys = target_mukeys)$mukey, target_mukeys)
})

test_that("SDA properties (dominant component numeric) works", {
  skip_if_offline()

  skip_on_cran()

  expect_equal(nrow(get_SDA_property(
    property = "Very Coarse Sand - Rep Value",
    method = "Dominant Component (Numeric)",
    areasymbols = target_areas,
    top_depth = 25,
    bottom_depth = 50
  )), target_area_rows)

  expect_equal(get_SDA_property(
    property = "Very Coarse Sand - Rep Value",
    method = "Dominant Component (Numeric)",
    mukeys = target_mukeys,
    top_depth = 25,
    bottom_depth = 50
  )$mukey, target_mukeys)

})

test_that("SDA properties (weighted average) works", {
  skip_if_offline()

  skip_on_cran()

  expect_equal(nrow(get_SDA_property(
    property = "Total Clay - Rep Value",
    method = "Weighted Average",
    areasymbols = target_areas,
    top_depth = 25,
    bottom_depth = 50
  )), target_area_rows)

  expect_equal(get_SDA_property(
    property = "Total Clay - Rep Value",
    method = "Weighted Average",
    mukeys = target_mukeys,
    top_depth = 25,
    bottom_depth = 50
  )$mukey, target_mukeys)
})

test_that("SDA properties (min/max) works", {
  skip_if_offline()

  skip_on_cran()

  expect_equal(nrow(get_SDA_property(
    property = "Saturated Hydraulic Conductivity - Rep Value",
    method = "Min/Max",
    areasymbols = target_areas,
    FUN = "MIN"
  )), target_area_rows)

  expect_equal(get_SDA_property(
    property = "Saturated Hydraulic Conductivity - Rep Value",
    method = "Min/Max",
    mukeys = target_mukeys,
    FUN = "MIN"
  )$mukey, target_mukeys)
})
