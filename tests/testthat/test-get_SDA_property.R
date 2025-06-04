target_areas <-  c("CA649", "CA630")

target_area_rows <- 221 # 1:1 with mukey
target_area_rows_all <- 1021 # 1:1 with component
target_area_rows_all_chorizon <- 3356 # 1:1 with chorizon

n_misc_area_rows <- 9
n_misc_area_rows_all <- 285
n_misc_area_rows_all_chorizon <- 19

target_mukeys <- c(463263, 463264)

# test get_SDA_property results -- expect a data.frame result for two legends

test_that("SDA properties (dominant condition) works", {
  
  skip_if_not_installed("httr")
  
  skip_if_offline()

  skip_on_cran()

  x <- get_SDA_property(property = "Taxonomic Suborder",
                        method = "Dominant Condition",
                        areasymbols = target_areas)
  skip_if(is.null(x))
  expect_identical(nrow(x), target_area_rows - n_misc_area_rows)

  x <- get_SDA_property(property = "Taxonomic Suborder",
                        method = "Dominant Condition",
                        areasymbols = target_areas,
                        miscellaneous_areas = TRUE)
  skip_if(is.null(x))
  expect_identical(nrow(x), target_area_rows)

  x <- get_SDA_property(property = c("Taxonomic Suborder","Taxonomic Order"),
                   method = "Dominant Condition",
                   mukeys = target_mukeys)
  skip_if(is.null(x))
  expect_identical(x$mukey, target_mukeys)

})

test_that("SDA properties (dominant component category) works", {
  
  skip_if_not_installed("httr")
  
  skip_if_offline()

  skip_on_cran()
  
  x <- get_SDA_property(property = "Taxonomic Suborder",
                        method = "Dominant Component (Category)",
                        areasymbols = target_areas)
  skip_if(is.null(x))
  expect_identical(nrow(x), target_area_rows - n_misc_area_rows)

  x <- get_SDA_property(property = c("Taxonomic Suborder","Taxonomic Order"),
                        method = "Dominant Component (Category)",
                        mukeys = target_mukeys)
  skip_if(is.null(x))
  expect_identical(x$mukey, target_mukeys)
})

test_that("SDA properties (dominant component numeric) works", {
  
  skip_if_not_installed("httr")
  
  skip_if_offline()

  skip_on_cran()

  x <- get_SDA_property(
    property = "Very Coarse Sand - Rep Value",
    method = "Dominant Component (Numeric)",
    areasymbols = target_areas,
    top_depth = 25,
    bottom_depth = 50
  )
  skip_if(is.null(x))
  expect_identical(nrow(x), target_area_rows)

  x <- get_SDA_property(
    property = c("sandvc_l","sandvc_r","sandvc_h"),
    method = "Dominant Component (Numeric)",
    mukeys = target_mukeys,
    top_depth = 25,
    bottom_depth = 50
  )
  skip_if(is.null(x))
  expect_identical(x$mukey, target_mukeys)

  # dominant component of 1st mapunit is rock outcrop (excluded), pH 5 from Thermalrocks 10-13cm
  # dominant component of 2nd mapunit is very shallow soil with R at 10cm
  x <- get_SDA_property(property = c("ph1to1h2o_r", "claytotal_r"),
                        method = "Dominant Component (Numeric)",
                        mukeys = c(461213L, 461265L),
                        miscellaneous_areas = FALSE,
                        include_minors = TRUE,
                        top_depth = 10,
                        bottom_depth = 20)
  expect_identical(x$ph1to1h2o_r, c(5, NA))
  expect_identical(x$claytotal_r, c(22, NA))
})

test_that("SDA properties (weighted average) works", {
  
  skip_if_not_installed("httr")
  
  skip_if_offline()

  skip_on_cran()

  x <- get_SDA_property(property = 'ph1to1h2o_r',
                   method = "Weighted Average",
                   mukeys = 461465L,
                   miscellaneous_areas = FALSE,
                   include_minors = TRUE,
                   top_depth = 10,
                   bottom_depth = 20)
  expect_equal(x$ph1to1h2o_r, 6.5, tolerance = 1e-5)

  x <- get_SDA_property(
    property = "Total Clay - Rep Value",
    method = "Weighted Average",
    areasymbols = target_areas,
    top_depth = 25,
    bottom_depth = 50
  )
  skip_if(is.null(x))
  expect_identical(nrow(x), target_area_rows)

  x <- get_SDA_property(
    property = c("claytotal_l","claytotal_r","claytotal_h"),
    method = "Weighted Average",
    mukeys = target_mukeys,
    top_depth = 25,
    bottom_depth = 50
  )

  skip_if(is.null(x))
  expect_identical(x$mukey, target_mukeys)

  # check miscellaneous areas and NULL data in horizons
  x <- get_SDA_property(property = c("ph1to1h2o_r", "claytotal_r"),
                        method = "Weighted Average",
                        mukeys = c(461213L, 461265L),
                        miscellaneous_areas = FALSE,
                        include_minors = TRUE,
                        top_depth = 10,
                        bottom_depth = 20)
  expect_equal(round(x$ph1to1h2o_r, 2), c(5.44, 6.9))
  expect_equal(round(x$claytotal_r, 1), c(21.2, 22.1))

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
  skip_if(is.null(agg))
  skip_if(is.null(noagg))

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

  expect_equal(sum(noagg$compwt * compwtsand), agg$sandtotal_r, tolerance = 1e-5)

  # check previously filtered textures and including minors
  agg <- get_SDA_property(
      property = "om_r",
      method = "Weighted Average",
      mukeys = 286248,
      top_depth = 0,
      bottom_depth = 100,
      include_minors = TRUE
    )

  noagg <- get_SDA_property("om_r", mukeys = 286248, method = "None", include_minors = TRUE)
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

  expect_equal(sum(noagg$compwt * compwtom), agg$om_r, tolerance = 1e-5)

  # testing unpopulated minors + significant misc. areas
  noagg1 <- get_SDA_property("ph1to1h2o_r", mukeys = 466601, method = "weighted average", include_minors = TRUE)
  noagg2 <- get_SDA_property("ph1to1h2o_r", mukeys = 466601, method = "weighted average", include_minors = FALSE, miscellaneous_areas = TRUE)
  noagg3 <- get_SDA_property("ph1to1h2o_r", mukeys = 466601, method = "weighted average", include_minors = TRUE, miscellaneous_areas = TRUE)

  skip_if(is.null(noagg1))
  skip_if(is.null(noagg2))
  skip_if(is.null(noagg3))

  expect_equal(c(noagg1$ph1to1h2o_r, noagg2$ph1to1h2o_r, noagg3$ph1to1h2o_r),
               rep(7, 3), tolerance = 1e-5)
})

test_that("SDA properties (min/max) works", {
  
  skip_if_not_installed("httr")
  
  skip_if_offline()

  skip_on_cran()

  x <- get_SDA_property(
    property = "Saturated Hydraulic Conductivity - Rep Value",
    method = "Min/Max",
    areasymbols = target_areas,
    FUN = "MIN",
    miscellaneous_areas = TRUE
  )
  skip_if(is.null(x))
  expect_identical(nrow(x), target_area_rows)

  x <- get_SDA_property(
    property = "Saturated Hydraulic Conductivity - Rep Value",
    method = "Min/Max",
    areasymbols = target_areas,
    FUN = "MIN"
  )
  skip_if(is.null(x))
  expect_identical(nrow(x), target_area_rows)
  
  # 463263      Daulton loam, 15 to 30 percent slopes
  # 463264      Daulton very rocky loam, 15 to 30 percent slopes
  # mukey 463264 Rock outcrop component has a hzdepb_r @ 152cm
  x <- get_SDA_property(
    property = c("ksat_l","hzdepb_r","ksat_h"),
    method = "Min/Max",
    mukeys = target_mukeys,
    FUN = "MAX", 
    miscellaneous_areas = TRUE
  )
  skip_if(is.null(x))
  expect_identical(x$mukey, target_mukeys)
})

test_that("SDA properties (no aggregation) works", {
  
  skip_if_not_installed("httr")
  
  skip_if_offline()

  skip_on_cran()

  # return results 1:1 with component for component properties
  x <- get_SDA_property(
      property = c('rsprod_l', 'rsprod_r', 'rsprod_h'),
      method = "NONE",
      areasymbols = target_areas,
      miscellaneous_areas = FALSE
    )
  skip_if(is.null(x))
  expect_identical(nrow(x), target_area_rows_all - n_misc_area_rows_all)

  x <- get_SDA_property(
    property = c('rsprod_l', 'rsprod_r', 'rsprod_h'),
    method = "NONE",
    areasymbols = target_areas,
    miscellaneous_areas = TRUE
  )
  skip_if(is.null(x))
  expect_identical(nrow(x), target_area_rows_all)


  # return results 1:1 with chorizon for horizon properties (includes cokey)
  x <- get_SDA_property(
    c('sandtotal_l', 'sandtotal_r', 'sandtotal_h'),
    method = "NONE",
    areasymbols = target_areas
  )
  skip_if(is.null(x))
  expect_identical(nrow(x), target_area_rows_all_chorizon - n_misc_area_rows_all_chorizon)

  x <- get_SDA_property(
    c('sandtotal_l', 'sandtotal_r', 'sandtotal_h'),
    method = "NONE",
    areasymbols = target_areas,
    miscellaneous_areas = TRUE
  )
  skip_if(is.null(x))
  expect_identical(nrow(x), target_area_rows_all_chorizon)
})
