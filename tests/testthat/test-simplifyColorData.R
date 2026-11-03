context("Simplification of color data (from NASIS/KSSL)")

# tolerance for comparing color RGB components of mixed colors
.tol <- 0.2

## example data, from NASIS

# single color / horizon
x.simple <- data.frame(
  peiid = c(530765L, 530765L),
  phiid = c(2362223L, 2362223L),
  colormoistst = c("dry", "moist"),
  pct = c(NA_integer_, NA_integer_),
  colorhue = c("7.5YR", "7.5YR"),
  colorvalue = c(3, 2.5),
  colorchroma = c(4, 2)
)

# perform conversion manually
# dry is first row
# res.rgb <- aqp::munsell2rgb(
#   x.simple$colorhue,
#   x.simple$colorvalue,
#   x.simple$colorchroma,
#   return_triplets = TRUE,
#   returnLAB = TRUE
# )
res.rgb <- data.frame(
  r = c(0.376458621305308, 0.283665422628007),
  g = c(0.252292330269399, 0.218599674259645),
  b = c(0.147759502056216, 0.172956479994321),
  L = c(30.2671633028639, 24.8998948532788),
  A = c(10.6076182823961, 5.46708813776506),
  B = c(21.7311902609193, 10.119764643999)
)

# two colors / horizon
x.multiple <- data.frame(
  peiid = c(625874L, 625874L, 625881L, 625881L),
  phiid = c(2889103L, 2889103L, 2889133L, 2889133L),
  colormoistst = c("moist", "moist", "dry", "dry"),
  pct = c(80L, 20L, 75L, 25L),
  colorhue = c("10YR", "7.5YR", "2.5Y", "2.5Y"),
  colorvalue = c(5, 6, 6, 7),
  colorchroma = c(6, 2, 1, 3)
)

x.missing <- data.frame(
  peiid = c(625874L, 625874L, 625874L, 625874L, 625874L, 625874L),
  phiid = c(2889103L, 2889103L, 2889103L, 2889133L,  2889133L,  2889133L),
  colormoistst = c("moist", "moist", "moist", "dry", NA, "dry"),
  pct = c(80L, 20L, NA, 75L, NA, 25L),
  colorhue = c("10YR", "7.5YR", "N", "2.5Y", NA, "2.5Y"),
  colorvalue = c(5, 6, 4, 6, NA, 7),
  colorchroma = c(6, 2, NA, 1, NA, 3)
)

test_that("simplifyColorData: single color / moisture state / horizon", {
  
  skip_if_not_installed("aqp")
  
  # single color / moisture state / horizon
  res <- simplifyColorData(x.simple, id.var = 'phiid', wt = 'pct')
  
  # should be a single row
  expect_equivalent(nrow(res), 1)

  # check parsing / conversion of dry color
  expect_equivalent(res$d_r, res.rgb$r[1])
  expect_equivalent(res$d_g, res.rgb$g[1])
  expect_equivalent(res$d_b, res.rgb$b[1])

  # check parsing / conversion of moist color
  expect_equivalent(res$m_r, res.rgb$r[2])
  expect_equivalent(res$m_g, res.rgb$g[2])
  expect_equivalent(res$m_b, res.rgb$b[2])

})


test_that("simplifyColorData: two colors / moisture state, color percentages provided", {

  skip_if_not_installed("farver")

  # two colors / moisture state, color percentages provided
  suppressMessages({
    res <- simplifyColorData(x.multiple,
                             id.var = 'phiid',
                             wt = 'pct')
  })

  # should be 2 rows
  expect_equivalent(nrow(res), 2)

  # check dry color mixture, should be ~ 2.5Y 6/2
  # using wide tolerance, because changes in the Munsell LUT can create errors
  # dry colors first
  expect_equal(res$d_r[1], 0.6, tolerance = .tol)
  expect_equal(res$d_g[1], 0.6, tolerance = .tol)
  expect_equal(res$d_b[1], 0.5, tolerance = .tol)

  # check moist color mixture, should be ~ 10YR 5/5
  # moist colors second
  expect_equal(res$m_r[2], 0.6, tolerance = .tol)
  expect_equal(res$m_g[2], 0.4, tolerance = .tol)
  expect_equal(res$m_b[2], 0.2, tolerance = .tol)

})

test_that("simplifyColorData: missing data", {

  skip_if_not_installed("rvest")
  
  # fix for running tests with aqp <2.0
  skip_if_not_installed("farver")

  # two colors / moisture state, color percentages provided
  suppressMessages({
    res <- simplifyColorData(x.missing,
                             id.var = 'phiid',
                             wt = 'pct')
  })

  # should be 2 rows
  expect_equivalent(nrow(res), 2)

  # using wide tolerance, because changes in the Munsell LUT can create errors

  # check dry color mixture, should be ~ 2.5Y 6/2
  expect_equal(res$d_r[1], 0.6, tolerance = .tol)
  expect_equal(res$d_g[1], 0.6, tolerance = .tol)
  expect_equal(res$d_b[1], 0.5, tolerance = .tol)

  # check moist color mixture, should be ~ 10YR 5/4 with added neutral hue
  expect_equal(res$m_r[2], 0.4, tolerance = .tol)
  expect_equal(res$m_g[2], 0.3, tolerance = .tol)
  expect_equal(res$m_b[2], 0.2, tolerance = .tol)

})

test_that(".dominantColors: missing data", {
  
  skip_if_not_installed("aqp")
  
  res <- .dominantColors(x.missing)

  # should be 2 rows
  expect_equivalent(nrow(res), 2)

  # dry color dominant should be 2.5Y 6/1 (75%)
  idx1 <- which(res$phiid == "2889133")
  expect_equivalent(res$d_hue[idx1], '2.5Y')
  expect_equivalent(res$d_value[idx1], 6)
  expect_equivalent(res$d_chroma[idx1], 1)

  # moist color dominant should be 10YR 5/6 (80%)
  idx2 <- which(res$phiid == "2889103")
  expect_equivalent(res$m_hue[idx2], '10YR')
  expect_equivalent(res$m_value[idx2], 5)
  expect_equivalent(res$m_chroma[idx2], 6)

})

