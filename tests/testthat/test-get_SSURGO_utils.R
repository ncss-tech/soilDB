test_that(".make_WSS_download_url works", {
  skip_if_not_installed("httr")
  
  skip_if_offline()
  
  skip_on_cran()
  
  x <- .make_WSS_download_url("areasymbol IN ('NH607', 'VT005', 'VT009', 'VT019')")
  expect_equal(length(x), 4)
  
  # vermont has a state-specific template
  x <- .make_WSS_download_url("areasymbol IN ('NH607', 'VT005', 'VT009', 'VT019')", include_template = TRUE)
  expect_equal(grepl("_soildb_US_", x), c(TRUE, FALSE, FALSE, FALSE))
  
})
