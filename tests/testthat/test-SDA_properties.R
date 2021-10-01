target_areas <-  c("CA649", "CA630")
target_area_rows <- 220 # 1:1 with mukey
target_area_rows_all <- 1021 # 1:1 with component
target_area_rows_all_chorizon <- 3178 # 1:1 with chorizon
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
  
  # check filtering of NULL
  agg <- get_SDA_property(property = c("sandtotal_r","silttotal_r","claytotal_r"),
                   method = "Weighted Average", 
                   mukeys = 545857,
                   top_depth = 0,
                   bottom_depth = 25)
  
  # create SPC from disaggregated data
  noagg <- get_SDA_property(property = c("sandtotal_r","silttotal_r","claytotal_r"),
                            method = "None", 
                            mukeys = 545857)
  aqp::depths(noagg) <- cokey ~ hzdept_r + hzdepb_r
  aqp::site(noagg) <- ~ comppct_r
  
  # calculate weighted average in each component
  compwtsand <- aqp::profileApply(aqp::glom(noagg, 0, 25, truncate = TRUE), function(p) {
    p$hzwt <- p$hzdepb_r - p$hzdept_r
    # explicitly set weights to 0 for NULL records
    p$hzwt[is.na(p$sandtotal_r)] <- 0
    p$hzwt <- p$hzwt / sum(p$hzwt)
    sum(p$hzwt * p$sandtotal_r)
  })
  
  noagg$compwt <- noagg$comppct_r
  
  ## component weight not counted if all data are null
  noagg$compwt[is.nan(compwtsand)] <- 0
  compwtsand[is.nan(compwtsand)] <- 0
  noagg$compwt <- noagg$compwt / sum(noagg$compwt)
  
  expect_equal(sum(noagg$compwt * compwtsand), agg$sandtotal_r)
  
  # check previously filtered textures and including minors
  agg <- soilDB::get_SDA_property(
      property = "om_r",
      method = "Weighted Average",
      mukeys = 286248,
      top_depth = 0,
      bottom_depth = 100,
      include_minors = TRUE
    )
  
  noagg <- soilDB::get_SDA_property("om_r", mukeys = 286248, method = "None", include_minors=TRUE)
  aqp::depths(noagg) <- cokey ~ hzdept_r + hzdepb_r
  aqp::site(noagg) <- ~ comppct_r
  
  # calculate weighted average in each component
  compwtom <- aqp::profileApply(aqp::glom(noagg, 0, 100, truncate = TRUE), function(p) {
    p$hzwt <- p$hzdepb_r - p$hzdept_r
    # explicitly set weights to 0 for NULL records
    p$hzwt[is.na(p$om_r)] <- 0
    p$hzwt <- p$hzwt / sum(p$hzwt)
    sum(p$hzwt * p$om_r)
  })
  
  noagg$compwt <- noagg$comppct_r
  
  ## component weight not counted if all data are null
  noagg$compwt[is.nan(compwtom)] <- 0
  compwtom[is.nan(compwtom)] <- 0
  noagg$compwt <- noagg$compwt / sum(noagg$compwt)
  
  expect_equal(sum(noagg$compwt * compwtom), agg$om_r)
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
