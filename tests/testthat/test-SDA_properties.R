target_areas <-  c("CA649", "CA630")
target_area_rows <- 231 # 1:1 with mukey
target_area_rows_all <- 1085 # 1:1 with component
target_area_rows_all_chorizon <- 3033 # 1:1 with chorizon
n_misc_area_rows <- 9

target_mukeys <- c(463263, 463264)

# test get_SDA_property results -- expect a data.frame result for two legends

test_that("SDA properties (dominant condition) works", {
  skip_if_offline()

  skip_on_cran()

  expect_equal(nrow(get_SDA_property(property = "Taxonomic Suborder",
                                     method = "Dominant Condition",
                                     areasymbols = target_areas)), target_area_rows)

  expect_equal(get_SDA_property(property = c("Taxonomic Suborder","Taxonomic Order"),
                                     method = "Dominant Condition",
                                     mukeys = target_mukeys)$mukey, target_mukeys)

})

test_that("SDA properties (dominant component category) works", {
  skip_if_offline()

  skip_on_cran()
  expect_equal(nrow(get_SDA_property(property = "Taxonomic Suborder",
                                     method = "Dominant Component (Category)",
                                     areasymbols = target_areas)), target_area_rows)

  expect_equal(get_SDA_property(property = c("Taxonomic Suborder","Taxonomic Order"),
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
    property = c("sandvc_l","sandvc_r","sandvc_h"),
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
    property = c("claytotal_l","claytotal_r","claytotal_h"),
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
    FUN = "MIN",
    miscellaneous_areas = TRUE
  )), target_area_rows)
  
  expect_equal(nrow(get_SDA_property(
    property = "Saturated Hydraulic Conductivity - Rep Value",
    method = "Min/Max",
    areasymbols = target_areas,
    FUN = "MIN"
  )), target_area_rows)
  
  expect_equal(get_SDA_property(
    property = c("ksat_l","ksat_r","ksat_h"),
    method = "Min/Max",
    mukeys = target_mukeys,
    FUN = "MIN"
  )$mukey, target_mukeys)
})

test_that("SDA properties (no aggregation) works", {
  skip_if_offline()

  skip_on_cran()

  # return results 1:1 with component for component properties
  expect_equal(nrow(get_SDA_property(property = c('rsprod_l','rsprod_r','rsprod_h'),
                                     method = "NONE",
                                     areasymbols = target_areas)), target_area_rows_all)


  # return results 1:1 with chorizon for horizon properties (includes cokey)
  expect_equal(nrow(get_SDA_property(c('sandtotal_l','sandtotal_r','sandtotal_h'),
                    method = "NONE",
                    areasymbols = target_areas)), target_area_rows_all_chorizon)
})
