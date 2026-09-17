test_that(".normalize_wss_fiscal_year handles various formats", {
  expect_identical(.normalize_wss_fiscal_year("23"), "FY23")
  expect_identical(.normalize_wss_fiscal_year(23), "FY23")
  expect_identical(.normalize_wss_fiscal_year(2023), "FY23")
  expect_identical(.normalize_wss_fiscal_year("2023"), "FY23")
  expect_identical(.normalize_wss_fiscal_year("FY23"), "FY23")
  expect_identical(.normalize_wss_fiscal_year("FY2023"), "FY23")
  expect_identical(.normalize_wss_fiscal_year(c("23", 2024, "FY25")), c("FY23", "FY24", "FY25"))
  expect_identical(.normalize_wss_fiscal_year(NULL), character(0))
  expect_identical(.normalize_wss_fiscal_year(character(0)), character(0))
})

test_that("createSSURGO throws informative error when cache is empty and exdir is NULL", {
  cache_root <- tempfile("soilDB-wss-cache-")
  old_opt <- options(soilDB.WSS.cache_dir = cache_root)
  on.exit(options(old_opt), add = TRUE)
  on.exit(unlink(cache_root, recursive = TRUE, force = TRUE), add = TRUE)

  # When cache_root doesn't exist
  expect_error(
    createSSURGO(filename = "test.gpkg", exdir = NULL),
    "No WSS cache FY directories found in cache root"
  )

  # When cache_root exists but has no FY directories
  dir.create(cache_root, recursive = TRUE)
  expect_error(
    createSSURGO(filename = "test.gpkg", exdir = NULL),
    "No WSS cache FY directories found in cache root"
  )
})

test_that("downloadSSURGO throws error when cached files span multiple fiscal years with exdir = NULL", {
  cache_root <- tempfile("soilDB-wss-cache-")
  old_opt <- options(soilDB.WSS.cache_dir = cache_root)
  on.exit(options(old_opt), add = TRUE)
  on.exit(unlink(cache_root, recursive = TRUE, force = TRUE), add = TRUE)

  # Create mock cached zip files in two different FY directories
  fy23_dir <- file.path(cache_root, "FY23")
  fy24_dir <- file.path(cache_root, "FY24")
  dir.create(fy23_dir, recursive = TRUE)
  dir.create(fy24_dir, recursive = TRUE)

  zip1 <- file.path(fy23_dir, "wss_SSA_CA067_[2023-01-01].zip")
  zip2 <- file.path(fy24_dir, "wss_SSA_CA077_[2024-01-01].zip")
  file.create(zip1)
  file.create(zip2)

  testthat::local_mocked_bindings(
    .make_WSS_download_url = function(...) {
      c(
        "https://websoilsurvey.sc.egov.usda.gov/DSD/Download/Cache/SSA/wss_SSA_CA067_[2023-01-01].zip",
        "https://websoilsurvey.sc.egov.usda.gov/DSD/Download/Cache/SSA/wss_SSA_CA077_[2024-01-01].zip"
      )
    },
    .package = "soilDB"
  )

  expect_error(
    soilDB::downloadSSURGO(areasymbols = c("CA067", "CA077"), extract = FALSE, exdir = NULL),
    "Cached ZIP files correspond to multiple fiscal years/directories"
  )
})


test_that("WSS cache selector respects template archives", {

  entries <- data.frame(
    file = c("non-template-fy23.zip", "non-template-fy24.zip", "template-fy24.zip"),
    basename = c(
      "wss_SSA_CA067_[01/01/2023 00:00:00].zip",
      "wss_SSA_CA067_[01/01/2024 00:00:00].zip",
      "wss_SSA_CA067_soildb_CA_2003_[01/01/2024 00:00:00].zip"
    ),
    db = "SSURGO",
    areasymbol = "CA067",
    saverest = as.Date(c("2023-01-01", "2024-01-01", "2024-01-01")),
    fiscal_year = c("FY23", "FY24", "FY24"),
    template = c(FALSE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  selected_template <- .wss_cache_select(
    entries,
    areasymbols = "CA067",
    db = "SSURGO",
    include_template = TRUE,
    latest_only = TRUE
  )
  expect_identical(selected_template$basename, "wss_SSA_CA067_soildb_CA_2003_[01/01/2024 00:00:00].zip")

  selected_non_template <- .wss_cache_select(
    entries,
    areasymbols = "CA067",
    db = "SSURGO",
    include_template = FALSE,
    latest_only = TRUE
  )
  expect_identical(selected_non_template$basename, "wss_SSA_CA067_[01/01/2024 00:00:00].zip")
})

test_that("WSS cache download targets are organized by fiscal year", {
  url <- "https://websoilsurvey.sc.egov.usda.gov/DSD/Download/Cache/SSA/wss_SSA_CA067_[01/01/2024 00:00:00].zip"
  cache_root <- tempfile("soilDB-wss-cache-")
  filename <- .wss_url_filename(url)

  expect_identical(
    .wss_cache_destfile(url, cache_root, cache_mode = TRUE),
    file.path(cache_root, "FY24", filename)
  )
  expect_identical(
    .wss_cache_destfile(url, cache_root, cache_mode = FALSE),
    file.path(cache_root, filename)
  )
})

test_that("downloadSSURGO fails when a forced redownload cannot replace the cached ZIP", {

  skip_on_cran()
  skip_if_offline()

  cache_root <- tempfile("soilDB-wss-cache-")
  old_opt <- options(soilDB.WSS.cache_dir = cache_root)
  on.exit(options(old_opt), add = TRUE)
  on.exit(unlink(cache_root, recursive = TRUE, force = TRUE), add = TRUE)

  areasymbol <- "CA067"

  res1 <- downloadSSURGO(areasymbols = areasymbol, extract = FALSE, quiet = TRUE)
  expect_true(length(res1) >= 1)

  cache1 <- list_WSS_cache(areasymbols = areasymbol, cache_dir = cache_root)
  expect_true(nrow(cache1) >= 1)

  testthat::local_mocked_bindings(
    curl_download = function(...) {
      stop("forced curl download failure", call. = FALSE)
    },
    .package = "curl"
  )

  expect_error(
    downloadSSURGO(areasymbols = areasymbol, extract = FALSE, quiet = TRUE, force = TRUE),
    "Forced re-download of SSURGO ZIP files failed"
  )
})

test_that("downloadSSURGO warns and falls back when remote metadata lookup fails", {

  skip_on_cran()
  skip_if_offline()

  cache_root <- tempfile("soilDB-wss-cache-")
  old_opt <- options(soilDB.WSS.cache_dir = cache_root)
  on.exit(options(old_opt), add = TRUE)
  on.exit(unlink(cache_root, recursive = TRUE, force = TRUE), add = TRUE)

  areasymbol <- "CA067"

  res1 <- downloadSSURGO(areasymbols = areasymbol, extract = FALSE, quiet = TRUE)
  expect_true(length(res1) >= 1)

  testthat::local_mocked_bindings(
    .make_WSS_download_url = function(...) {
      stop("forced WSS query failure", call. = FALSE)
    },
    .package = "soilDB"
  )

  res2 <- expect_warning(
    downloadSSURGO(areasymbols = areasymbol, extract = FALSE, quiet = FALSE),
    "Unable to query remote WSS metadata"
  )

  expect_true(length(res2) >= 1)
  expect_identical(res1, res2)
})
