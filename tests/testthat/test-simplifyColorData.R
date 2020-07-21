context("Simplification of color data (from NASIS/KSSL)")


## example data, from NASIS

# single color / horizon
x.simple <- structure(
  list(
    peiid = c(530765L, 530765L),
    phiid = c(2362223L, 2362223L),
    colormoistst = c("dry", "moist"),
    pct = c(NA_integer_, NA_integer_),
    colorhue = c("7.5YR", "7.5YR"),
    colorvalue = c(3, 2.5),
    colorchroma = c(4, 2)
  ),
  .Names = c(
    "peiid",
    "phiid",
    "colormoistst",
    "pct",
    "colorhue",
    "colorvalue",
    "colorchroma"
  ),
  row.names = 1:2,
  class = "data.frame"
)

# two colors / horizon
x.multiple <-
  structure(
    list(
      peiid = c(625874L, 625874L, 625881L, 625881L),
      phiid = c(2889103L, 2889103L, 2889133L, 2889133L),
      colormoistst = c("moist", "moist", "dry", "dry"),
      pct = c(80L, 20L, 75L, 25L),
      colorhue = c("10YR", "7.5YR", "2.5Y", "2.5Y"),
      colorvalue = c(5, 6, 6, 7),
      colorchroma = c(6, 2, 1, 3)
    ),
    .Names = c(
      "peiid",
      "phiid",
      "colormoistst",
      "pct",
      "colorhue",
      "colorvalue",
      "colorchroma"
    ),
    row.names = c(344L, 345L, 396L, 397L),
    class = "data.frame"
  )

test_that("simplifyColorData: single color / moisture state / horizon", {
  
  # single color / moisture state / horizon
  res <- simplifyColorData(x.simple, id.var = 'phiid', wt = 'pct')
  
  # perform conversion manually
  # dry is first row
  res.rgb <- munsell2rgb(x.simple$colorhue, x.simple$colorvalue, x.simple$colorchroma, return_triplets = TRUE, returnLAB = TRUE)
  
  # should be a single row
  expect_equal(nrow(res), 1)
  
  # check parsing / conversion of dry color
  expect_equal(res$d_r, res.rgb$r[1])
  expect_equal(res$d_g, res.rgb$g[1])
  expect_equal(res$d_b, res.rgb$b[1])
  
  # check parsing / conversion of moist color
  expect_equal(res$m_r, res.rgb$r[2])
  expect_equal(res$m_g, res.rgb$g[2])
  expect_equal(res$m_b, res.rgb$b[2])
  
})


test_that("simplifyColorData: two colors / moisture state, color percentages provided", {
  
  # two colors / moisture state, color percentages provided
  suppressMessages(res <- simplifyColorData(x.multiple, id.var = 'phiid', wt = 'pct'))
  
  # should be 2 rows
  expect_equal(nrow(res), 2)
  
  # check dry color mixture, should be ~ 2.5Y 6/2
  # dry colors first
  expect_equal(res$d_hue[1], '2.5Y')
  expect_equal(res$d_value[1], 6)
  expect_equal(res$d_chroma[1], 2)
  
  # check moist color mixture, should be ~ 10YR 5/5
  # moist colors second
  expect_equal(res$m_hue[2], '10YR')
  expect_equal(res$m_value[2], 5)
  expect_equal(res$m_chroma[2], 5)
  
})

