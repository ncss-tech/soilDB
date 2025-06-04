context("estimating color mixtures")

## mixing done in CIE LAB by default now

## example data
# http://ncss-tech.github.io/AQP/soilDB/mixing-soil-color-data.html
# colors <- c('10YR 5/6', '7.5YR 6/2')
# weights <- c(80, 20)

# re-make the data expected by estimateColorMixture()
x <- data.frame(
  colorhue = c("10YR", "7.5YR"),
  colorvalue = c(5, 6),
  colorchroma = c(6, 2),
  r = c(0.613039703549513, 0.647387131276519),
  g = c(0.450352989954951, 0.569095177873084),
  b = c(0.216677275892247, 0.504787409851694),
  L = c(51.4337014216073, 61.5648731270279),
  A = c(9.91791561017435, 4.80646661041506),
  B = c(38.6888986191802, 11.4641743697849),
  pct = c(80, 20),
  col = c("#9C7337FF", "#A59181FF"),
  id = c(1, 1),
  colormoistst = c("Dry", "Dry")
)

test_that("estimateColorMixture basic functionality", {
  
  skip_if_not_installed("aqp")
  skip_if_not_installed("farver")

  # there is no back-transformation to Munsell by default, request it
  res <- estimateColorMixture(x, backTransform = TRUE)

  # should be a single row
  expect_identical(nrow(res), 1)

  # ensure values aren't NA
  expect_true(!any(sapply(res, is.na)))

  # check sRGB coordinates
  # using wide tolerance, because changes in the Munsell LUT can create errors
  # 2025-02-14: static color coordinates used in tests should be robust to future changes
  expect_equal(res$r, 0.6, tolerance = 0.1)
  expect_equal(res$g, 0.4, tolerance = 0.1)
  expect_equal(res$b, 0.2, tolerance = 0.1)
})
